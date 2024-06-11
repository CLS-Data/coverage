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

# # 3. Sample and Attrition ----
# ## a. MCS ----
# mcs <- list()
# 
# mcs$sample <- df_file %>%
#   filter(study == "mcs",
#          variable == "NOCMHH") %$%
#   load_mcs(file_path, variable) %>%
#   filter(nocmhh %in% 1:2) %>%
#   uncount(nocmhh, .id = "cnum") %>%
#   mutate(iid = row_number())
# 
# mcs$attrit <- df_file %>%
#   filter(study == "mcs",
#          variable == "NOCMHH") %$%
#   load_mcs(file_path) %>%
#   right_join(mcs$sample, by = "mcsid") %>%
#   select(iid, matches("aoutc00")) %>%
#   pivot_longer(-iid) %>%
#   mutate(sweep = match(str_sub(name, 1, 1), letters),
#          observed = case_when(sweep == 1 & value == 2 ~ 1,
#                               sweep %in% 2:7 & value == 1 ~ 1,
#                               TRUE ~ 0)) %>%
#   select(iid, sweep, observed)
# 
# ## b. Next Steps ----
# next_steps <- list()
# 
# next_steps$sample <- df_file %>%
#   filter(study == "next_steps",
#          variable == "MAINBOOST") %$%
#   load_next_steps(file_path) %>%
#   mutate(iid = row_number(),
#          boost = mainboost - 1) %>%
#   select(nsid, iid, boost)
# 
# next_steps$attrit <- df_file %>%
#   filter(study == "next_steps",
#          variable == "MAINBOOST") %$%
#   load_next_steps(file_path) %>%
#   select(nsid, matches("outcome")) %>%
#   mutate(across(matches("outcome"), ~ ifelse(.x %in% 1, 1, 0))) %>%
#   pivot_longer(-nsid, values_to = "observed") %>%
#   mutate(sweep = str_sub(name, 2, 2) %>% as.integer()) %>%
#   left_join(next_steps$sample, by = "nsid") %>%
#   select(iid, sweep, observed)
# 
# ## b. BCS70 ----
# bcs70 <- list()
# 
# bcs70$sample <- df_file %>%
#   filter(study == "bcs70",
#          variable == "MULTIPNO") %$%
#   load_file(file_path) %>%
#   mutate(iid = row_number()) %>%
#   select(bcsid, iid)
# 
# bcs70$attrit <- df_file %>%
#   filter(study == "bcs70",
#          variable == "MULTIPNO") %$%
#   load_bcs70(file_path) %>%
#   left_join(bcs70$sample, by = "bcsid") %>%
#   select(iid, matches("outcme")) %>%
#   pivot_longer(-iid) %>%
#   mutate(sweep = str_sub(name, -2) %>% as.integer(),
#          observed = ifelse(value %in% 1, 1, 0)) %>%
#   select(iid, sweep, observed)
# 
# ## d. NCDS ----
# ncds <- list()
# 
# ncds$sample <- df_file %>%
#   filter(study == "ncds",
#          variable == "MULTIPNO") %$%
#   load_bcs70(file_path) %>%
#   mutate(iid = row_number()) %>%
#   select(ncdsid, iid)
# 
# ncds$attrit <- df_file %>%
#   filter(study == "ncds",
#          variable == "MULTIPNO") %$%
#   load_bcs70(file_path) %>%
#   left_join(ncds$sample, by = "ncdsid") %>%
#   select(iid, matches("outcme")) %>%
#   pivot_longer(-iid) %>%
#   mutate(sweep = str_sub(name, -2) %>% as.integer(),
#          sweep = case_when(sweep <= 6 ~ sweep + 1,
#                            sweep >= 7 ~ sweep + 2,
#                            is.na(sweep) ~ 8),
#          observed = ifelse(value %in% 1, 1, 0)) %>%
#   select(iid, sweep, observed)
# 
# 
