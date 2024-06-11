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
           fct_from_dict(binary_dict))

saveRDS(data, "Data/data.Rds")

var_dict <- c(
  response = "Respondent",
  sex = "Sex",
  ethnic_group = "Ethnicity",
  gender_identity = "Gender Identity",
  sexuality = "Sexual Orientation",
  country = "Country of Residence",
  fsm = "Free School Meals",
  sen = "Special Educational Needs",
  religion = "Religion",
  disabled = "Disabled",
  nssec = "NS-SEC Social Class",
  rgsc = "Registrar General's Social Class",
  carer = "Carer",
  marstat = "Marital Status"
)

df_long <- data$wide %>%
  pivot_longer(-c(study, fup, iid)) %>%
  drop_na()

df_count <- df_long %>%
  count(study, fup, name, value) %>%
  mutate(name_clean = factor(var_dict[name], var_dict),
         study_clean = factor(study_dict[study], study_dict)) %>%
  select(study_clean, fup, name_clean, value, n) %>%
  filter(value != "No")

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
  select(low, high, study_clean, name_clean, value, n)

var_levels <- keep(data$wide, is.factor) %>%
  map(levels) %>%
  set_names(., var_dict[names(.)])

save(df_count, df_range, var_levels, file = "Shiny/shiny_data.Rdata")


unlink("Shinylive", recursive = TRUE)
shinylive::export("Shiny", "Shinylive")
httpuv::runStaticServer("Shinylive")
