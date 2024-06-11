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
  deframe() %>%
  as.list()

get_lookup <- function(file_path){
  data <- read_dta(file_path, n_max = 1)
  
  tibble(pos = 1:ncol(data),
         variable = names(data),
         label = var_label(data, unlist = TRUE),
         col_type = map_chr(data, vctrs::vec_ptype_abbr),
         value_labels = map(data, val_labels))
}

df_file <- enframe(flds, name = "study", value = "study_fld") %>%
  unchop(study_fld) %>%
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

# 2. Make Functions ----
## a. Work with lookup ----
extract_labels <- function(var_regex){
  df_file %>%
    filter(str_detect(variable, !!var_regex)) %>%
    pull(value_labels)
}

labels_to_grid <- function(label_list){
  label_list %>%
    map_dfr(enframe, .id = "variable") %>%
    arrange(variable, value) %>%
    pivot_wider(names_from = variable, values_from = name) 
}

load_files <- function(df_vars, iid = study_iid){
  df_vars %>%
    select(file_path, variable) %>%
    chop(variable) %$%
    map2(file_path, variable, load_file) %>%
    reduce(~ full_join(.x, .y, by = iid))
}


## b. Variable Cleaning ----
negative_to_na <- function(var){
  na_range(var) <- c(-Inf, -1)
  user_na_to_na(var)
}

values_to_na <- function(var, values){
  na_values(var) <- values
  user_na_to_na(var)
}

keep_valid_values <- function(var, values){
  all_values <- na.omit(var) %>% unique()
  na_values(var) <- all_values[!(all_values %in% values)]
  user_na_to_na(var)
}

comb_lookup <- function(old, new){
  new[ifelse(old > 0, old, NA)]
}

fct_recode_int <- function(var, replace_levels){
  new_levels <- set_names(levels(var)[replace_levels], names(replace_levels))
  fct_recode(var, !!!new_levels)
}

add_na_col <- function(df, vars = everything()){
  df$row_miss <- df %>% 
    select({{ vars }}) %>%
    mutate(across(everything(), is.na)) %>%
    rowSums()
  
  return(df)
} 

fct_from_dict <- function(var, dict) factor(dict[var], dict)

clean_to_long <- function(df_wide, prefix, iid = study_iid){
  df_wide %>%
    select(all_of(!!iid), matches(glue("{prefix}_\\d"))) %>%
    pivot_longer(-c(all_of(!!iid)), names_to = "fup", 
                 names_prefix = glue("{prefix}_"), 
                 names_transform = list(fup = as.integer),
                 values_to = prefix)
}

## c. Dictionaries ----
fup_dict <- list(mcs = c(0, 3, 5, 7, 11, 14, 17),
                 next_steps = c(14:20, 25),
                 bcs70 = c(0, 5, 10, 16, 26, 29, 34, 38, 42, 46),
                 ncds = c(0, 7, 11, 16, 23, 33, 42, 44, 46, 50, 55))

study_dict <- c(mcs = "MCS", next_steps = "Next Steps",
                bcs70 = "BCS70", ncds = "NCDS", Total = "Total")

name_dict <- c(n = "N", prop = "% Observations")

ethnic_dict <- c("White", "Mixed", "Indian",
                 "Pakistani or Bangladeshi",
                 "Black", "Other")

sexuality_dict <- c("Heterosexual", "Bisexual",
                    "Homosexual", "Other")

country_dict <- c("England", "Wales",
                  "Scotland", "Northern Ireland")

religion_dict <- c("No Religion", "Christian", "Muslim",
                   "Hindu", "Sikh", "Buddhist", "Jewish")

nssec_dict <- c("Managerial & Professional",
                "Intermediate", 
                "Small Employers or Self-Employed",
                "Lower Supervisory & Technical", 
                "(Semi-)Routine", "Not Working")

rgsc_dict <- c("I Professional", "II Managerial & Technical",
               "III Skilled Non-Manual", "III Skilled Manual",
               "IV Partly Skilled", "V Unskilled", 
               "Not Working")

marital_dict <- c("Single / Never Married", "Married",
                  "Separated", "Divorced", "Widowed")

sexuality_dict <- c("Heterosexual", "Bisexual",
                    "Homosexual", "Other")

binary_dict <- c("No", "Yes")


# 5. Save Objects ----
rm(get_lookup)

save(list = ls(), file = "Data/helpers.Rdata")
