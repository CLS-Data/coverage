library(tidyverse)
library(haven)
library(glue)
library(labelled)
library(stringi)
library(magrittr)

rm(list = ls())

# 1. Make Look-up File ----
flds <- Sys.getenv() %>%
  enframe() %>%
  filter(str_detect(name, "_fld$")) %>%
  mutate(name = str_replace(name, "_fld$", ""),
         value = as.character(value)) %>%
  deframe()

get_lookup <- function(file_path){
  data <- read_dta(file_path, n_max = 1)
  
  tibble(pos = 1:ncol(data),
         variable = names(data),
         label = var_label(data, unlist = TRUE),
         col_type = map_chr(data, vctrs::vec_ptype_abbr),
         value_labels = map(data, val_labels))
}

df_file <- enframe(flds, name = "study", value = "study_fld") %>%
  mutate(file_path = map(study_fld, ~ list.files(.x, "\\.dta$", recursive = TRUE, full.names = TRUE))) %>%
  unnest(file_path) %>%
  mutate(file = str_replace(file_path, study_fld, "") %>%
           str_replace("\\/", "") %>% 
           stri_reverse()) %>%
  separate(file, c("dta", "fld"), sep = "/", extra = "merge") %>%
  mutate(across(c(dta, fld), stri_reverse),
         dta = str_replace(dta, "\\.dta$", "")) %>%
  filter(!str_detect(fld, "^UKDS")) %>%
  select(study, fld, dta, file_path) %>%
  mutate(lookup = map(file_path, get_lookup)) %>%
  unnest(lookup) %>%
  mutate(var_low = str_to_lower(variable),
         lab_low = str_to_lower(label)) %>%
  relocate(file_path, .after = last_col())

df_file %>%
  select(-value_labels, -file_path) %>%
  write_csv("Data/lookup.csv")

save(df_file, flds, file = "Data/lookup.Rdata")

## b. Lookup Searching Functions ----
add_type <- function(df_keep, var_type){
  if (!is.null(var_type)){
    df_keep <- df_keep %>%
      mutate(type = !!var_type)
  }
  return(df_keep)
}

label_keep <- function(reg_ex, var_type = NULL, file_reg = ".*"){
  df_keep <- df_file %>%
    filter(str_detect(lab_low, reg_ex),
           str_detect(file_path, file_reg)) %>%
    select(fld, dta, pos, variable, label)
  
  add_type(df_keep, var_type)
}

label_detect <- function(reg_ex, var_type = NULL, file_reg = ".*"){
  df_label <- label_keep(reg_ex, var_type, file_reg)
  View(df_label)
}

var_keep <- function(reg_ex, var_type = NULL, file_reg = ".*"){
  df_keep <- df_file %>%
    filter(str_detect(var_low, reg_ex),
           str_detect(file_path, file_reg)) %>%
    select(fld, dta, pos, variable, label)
  
  add_type(df_keep, var_type)
}

var_detect <- function(reg_ex, var_type = NULL, file_reg = ".*"){
  df_var <- var_keep(reg_ex, var_type, file_reg)
  View(df_var)
}


# 2. Find Variables ----
df_file <- df_file %>% 
  filter(study == "bcs70")

xwave <- df_file

load_bcs <- function(file_path, vars = NULL){
  if (is.null(vars)) df <- read_dta(file_path)
  else df <- read_dta(file_path, col_select = c(any_of(c("bcsid", "BCSID")), all_of(vars)))
  
  df %>%
    rename_with(str_to_lower) %>%
    rename(iid = any_of("bcsid"))
}

negative_to_na <- function(var){
  na_range(var) <- c(-Inf, -1)
  user_na_to_na(var)
}


xwave <- df_file %>%
  filter(dta == "bcs70_response_1970-2016") %>%
  distinct(file_path) %>%
  pull(file_path) %>% load_bcs()

## a. Countries ----
df_country <- df_file %>%
  filter(str_detect(var_low, "bd..?cntry")) %>%
  select(dta, file_path, variable) %>%
  chop(variable) %>%
  mutate(data = map2(file_path, variable, load_bcs)) %$%
  reduce(data, ~ full_join(.x, .y, by = "iid"))

df_country %>%
  mutate(across(-iid, ~ negative_to_na(.x) %>% as_factor())) %>%
  pivot_longer(-iid) %>%
  count(name, value) %>%
  mutate(name = str_replace(name, "bd", "") %>% 
           str_replace("cntry", "") %>%
           as.integer()) %>%
  arrange(name, value) %>%
  drop_na() %>%
  add_count(name, wt = n, name = "total") %>%
  mutate(p = round(100 * n / total, 1),
         n = format(n, big.mark = ",") %>% trimws(),
         string = glue("{n}\n({p}%)")) %>%
  select(name, value, string) %>%
  pivot_wider(names_from = name, values_from = string) %>%
  flextable::flextable()


## b. Sexual and Gender Identities ----
df_sex <- df_file %>%
  filter(var_low %in% c("b9sxid", "b9whnc14", "psamesx")) %>%
  select(dta, file_path, variable) %>%
  chop(variable) %>%
  mutate(data = map2(file_path, variable, load_bcs)) %>%
  select(dta, data) %>% deframe()

df_sex[str_detect(names(df_sex), "partner")] %>%
  map_dbl(~ .x %>%
            filter(psamesx == 1) %>%
            distinct(iid) %>%
            nrow())

df_sex$bcs70_2012_flatfile %>%
  filter(b9sxid %in% 2:4) %>%
  count(b9sxid)

df_sex$bcs70_2012_flatfile %>%
  filter(b9whnc14 == 1) %>%
  count(b9sxid)

df_sex[str_detect(names(df_sex), "partner")] %>%
  map_dfr(~ .x %>%
            filter(psamesx == 1) %>%
            distinct(iid)) %>%
  distinct(iid) %>%
  mutate(same_sex_partner = 1) %>%
  full_join(df_sex$bcs70_2012_flatfile,
            by = "iid") %>%
  count(b9sxid, same_sex_partner) %>%
  filter(b9sxid %in% 2:4 | same_sex_partner == 1) %>%
  mutate(same_sex_partner = ifelse(is.na(same_sex_partner), "no_same_sex", "same_sex")) %>%
  pivot_wider(names_from = same_sex_partner, values_from = n) %>%
  mutate(across(matches("same_sex"), ~ replace_na(.x, 0)))

## c. Ethnic Groups ----
tibble_to_list <- function(df){
  df %>%
    mutate(across(-1, as.list)) %>%
    pivot_longer(-1) %>%
    nest(data = -1) %>%
    deframe() %>%
    map(deframe)
}

df_file %>%
  filter(var_low %in% c("a12_1", "a12_4", "a12_7", "c6_4",
                        "ethnic", "e245", "bd7ethnic")) %>%
  mutate(data = map2(file_path, variable, load_bcs),
         data = map(data,
                    ~ .x %>%
                      select(ethnic = 2) %>%
                      count(ethnic))) %>%
  select(variable, label, data) %>%
  tibble_to_list()

## d. (Physical and Mental) Disability ----

## e. Social Class (e.g. Free School Meals) ----

## f. Marital Status ----
label_detect("marital")

## g. Vulnerabilities: Lonely ----
