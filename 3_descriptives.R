library(tidyverse)
library(haven)
library(glue)
library(labelled)
library(magrittr)
library(janitor)
library(summarytools)
library(ggrepel)
library(shiny)

rm(list = ls())

# 1. Load Lookup and Functions and Create Dictionaries ----
## a. Load Files ----
load("Data/lookup.Rdata")
load("Data/helpers.Rdata")

data <- names(study_dict)[1:4] %>%
  set_names(., .) %>%
  map(~ glue("Data/{.x}.Rds") %>% readRDS()) %>%
  transpose() %>%
  map(~ bind_rows(.x, .id = "study"))

data$wide <- data$wide %>%
  right_join(data$response %>% mutate(response = 2), by = c("study", "iid", "fup")) %>%
  mutate(response = ifelse(is.na(response), 1, 2) %>%
           fct_from_dict(c("Non-Respondent", "Respondent")),
         nssec = fct_recode(nssec, NULL = "Not Working"),
         rgsc = fct_recode(rgsc, NULL = "Not Working"))

data$wide %>%
  group_by(study, iid) %>%
  summarise(
    across(-c(fup), ~ na.omit(.x) %>% unique() %>% length()),
    .groups = "drop"
  ) %>%
  pivot_longer(-c(study, iid), names_to = "variable", values_to = "n_unique") %>%
  group_by(study, variable) %>%
  summarise(max_unique = max(n_unique), 
            .groups = "drop") %>%
  pivot_wider(names_from = study, values_from = max_unique)

data$wide %>%
  mutate(across(-c(study, iid, fup), ~ !is.na(.x))) %>%
  pivot_longer(-c(study, iid, fup), names_to = "variable", values_to = "observed") %>%
  filter(observed) %>%
  mutate(n_fups = n_fups[study]) %>%
  group_by(study, variable) %>%
  summarise(prop_fups = length(unique(fup)) / first(n_fups),
            .groups = "drop") %>%
  pivot_wider(names_from = study, values_from = prop_fups)

saveRDS(data, "Data/data.Rds")

var_dict <- c(
  response = "Respondent",
  sex = "Sex",
  ethnic_group = "Ethnicity",
  gender_identity = "Gender Identity",
  sexuality = "Sexual Orientation",
  country = "Country of Residence",
  region = "Region of Residence",
  fsm = "Free School Meals",
  sen = "Special Educational Needs",
  religion = "Religion",
  disabled = "Disabled",
  nssec = "NS-SEC Social Class",
  rgsc = "Registrar General's Social Class",
  carer = "Carer",
  marstat = "Marital Status"
)

df_levels <- data$wide %>%
  select(where(is.factor)) %>%
  map_dfr(~ levels(.x) %>% enframe(name = "level"),
          .id = "name") %>%
  mutate(name_clean = factor(var_dict[name], var_dict)) %>%
  select(name_clean, level, value) %>%
  arrange(name_clean, level)

df_long <- data$wide %>%
  pivot_longer(-c(study, fup, iid)) %>%
  drop_na()

df_count <- df_long %>%
  count(study, fup, name, value) %>%
  mutate(name_clean = factor(var_dict[name], var_dict),
         study_clean = factor(study_dict[study], study_dict)) %>%
  select(study_clean, fup, name_clean, value, n) %>%
  filter(value != "No") %>%
  complete(study_clean, fup, name_clean, value) %>%
  group_by(study_clean, fup, name_clean) %>%
  filter(!all(is.na(n))) %>%
  group_by(study_clean, name_clean, value) %>%
  filter(!all(is.na(n))) %>%
  ungroup() %>%
  left_join(df_levels, by = c("name_clean", "value")) %>%
  drop_na(level) %>%
  filter(value != "No") %>%
  mutate(n = replace_na(n, 0),
         fup = as.integer(fup)) %>%
  arrange(name_clean, level) %>%
  mutate(value = fct_inorder(value)) %>%
  arrange(study_clean, fup, name_clean, value) %>%
  select(-level)


fup_range <- range(df_long$fup)

df_range <- expand_grid(low = fup_range[1]:fup_range[2],
                        high = fup_range[1]:fup_range[2]) %>%
  filter(low <= high) %>%
  expand_grid(study = unique(df_long$study)) %>%
  mutate(fup = pmap(list(study, low, high), ~ fup_dict[[..1]][between(fup_dict[[..1]], ..2, ..3)])) %>%
  filter(map_int(fup, length) > 0) %>%
  nest(data = -c(study, fup))%>%
  unchop(fup) %>%
  left_join(df_long, by = c("study", "fup")) %>%
  filter(value != "No") %>%
  select(-fup) %>%
  distinct() %>%
  count(study, data, name, value) %>%
  mutate(name_clean = factor(var_dict[name], var_dict),
         study_clean = factor(study_dict[study], study_dict)) %>%
  unnest(data) %>%
  select(low, high, study_clean, name_clean, value, n) %>%
  complete(low, high, study_clean, name_clean, value) %>%
  group_by(low, high, study_clean, name_clean) %>%
  filter(!all(is.na(n))) %>%
  group_by(study_clean, name_clean, value) %>%
  filter(!all(is.na(n))) %>%
  ungroup() %>%
  left_join(df_levels, by = c("name_clean", "value")) %>%
  drop_na(level) %>%
  filter(value != "No") %>%
  mutate(n = replace_na(n, 0)) %>%
  arrange(name_clean, level) %>%
  mutate(value = fct_inorder(value)) %>%
  arrange(study_clean, low, high, name_clean, value) %>%
  select(-level)

var_levels <- keep(data$wide, is.factor) %>%
  map(levels) %>%
  set_names(., var_dict[names(.)])


save(df_count, df_range, var_levels, df_levels,
     file = "Data/shiny_data.Rdata")

write_csv(df_count, "Data/df_count.csv")
write_csv(df_range, "Data/df_range.csv")
write_csv(df_levels, "Data/df_levels.csv")

# shiny::runApp("Code")

