library(tidyverse)
library(haven)
library(glue)
library(labelled)
library(magrittr)
library(janitor)
library(summarytools)

rm(list = ls())

# 1. Load Lookup and Functions and Create Dictionaries ----
## a. Load Files ----
load("Data/lookup.Rdata")
load("Data/helpers.Rdata")

df_file <- df_file %>%
  filter(study == "next_steps") %>%
  select(-study)

load_file <- function(file_path, vars = everything()){
  read_dta(file_path, col_select = c(NSID, all_of(vars))) %>%
    rename_with(str_to_lower)
}
study_iid <- "nsid"

rename_var <- function(var, prefix){
  sweep <- str_extract(var, "\\d")
  glue("{prefix}_{sweep}")
}


# 2. Collect and Clean Data ----
clean <- list()

## a. Ethnicity ----
clean$ethnicity <- df_file %>%
  filter(str_detect(var_low, "(w8dethn6|w?ethgrpyp)"),
         str_detect(dta, "(ns8|young_person)")) %>%
  load_files() %>%
  mutate(
    across(-nsid, negative_to_na),
    across(matches("w?ethgrpyp"),
           ~ case_when(between(.x, 1, 3) ~ as.integer(.x),
                       between(.x, 4, 5) ~ 4L,
                       between(.x, 6, 7) ~ 5L,
                       .x == 8 ~ 6L))
  ) %>%
  pivot_longer(-nsid) %>%
  mutate(sweep = str_sub(name, 2, 2) %>% as.integer(),
         fup = fup_dict$next_steps[sweep],
         ethnic_group = fct_from_dict(value, ethnic_dict)) %>%
  select(nsid, fup, ethnic_group) %>%
  drop_na() %>%
  group_by(nsid) %>%
  slice_min(fup) %>%
  ungroup() %>%
  select(nsid, ethnic_group)

## b. Sex ----
clean$sex <- df_file %>%
  filter(str_detect(var_low, "^(w8cmsex|w\\dsex(|yp))$"),
         fld == "eul") %>%
  load_files() %>%
  pivot_longer(-nsid, values_to = "sex", names_to = "sweep") %>%
  mutate(sex = ifelse(sex %in% 1:2, sex, NA),
         sweep = str_sub(sweep, 2, 2) %>% as.integer()) %>%
  drop_na() %>%
  group_by(nsid) %>%
  slice_min(sweep) %>%
  ungroup() %>%
  select(nsid, sex)

clean$sex <- df_file %>%
  filter(str_detect(variable, "^(Sex|ReltoYP)$")) %>%
  load_files() %>%
  filter(reltoyp == 0) %>%
  mutate(sex = ifelse(sex %in% 1:2, sex, NA)) %>%
  full_join(clean$sex, by = "nsid") %>%
  mutate(sex = ifelse(!is.na(sex.x), sex.x, sex.y) %>%
           factor(1:2, c("Male", "Female"))) %>%
  select(nsid, sex) 

## c. Gender Identity ----

## d. Sexual Orientation ----
clean$sexuality <- df_file %>%
  filter(str_detect(var_low, "sexual")) %>%
  load_files() %>%
  mutate(across(-nsid,
                ~ case_when(.x %in% c(1, 4) ~ as.integer(.x),
                            .x %in% 2:3 ~ 5 - as.integer(.x)))) %>%
  pivot_longer(-nsid) %>%
  mutate(sweep = str_sub(name, 2, 2) %>% as.integer(),
         fup = fup_dict$next_steps[sweep],
         sexuality = fct_from_dict(value, sexuality_dict)) %>%
  select(nsid, fup, sexuality) %>%
  drop_na() %>%
  group_by(nsid) %>%
  slice_min(fup) %>%
  ungroup() %>%
  select(nsid, sexuality)

## e. Country ----
clean$country <- df_file %>%
  filter(variable == "W8DCTRY") %>%
  load_files() %>%
  mutate(fup = 25,
         country = negative_to_na(w8dctry) %>%
           as_factor() %>%
           fct_drop()) %>%
  select(nsid, fup, country)

## f. Free School Meals ----

## g. Special Educational Needs ----
clean$sen <- df_file %>%
  filter(str_detect(var_low, "^w.(sen|sencurr|stated)mp$")) %>%
  load_files() %>%
  mutate(across(-nsid, ~ 2 - negative_to_na(.x))) %>%
  pivot_longer(-nsid,
               names_to = c("sweep", ".value"),
               names_pattern = "w(.)(.*)",
               names_transform = list(sweep = as.integer)) %>%
  mutate(fup = fup_dict$next_steps[sweep],
         sen = case_when(senmp == 1 | sencurrmp == 1 ~ 2,
                         senmp == 0 | sencurrmp == 0 ~ 1) %>%
           fct_from_dict(binary_dict)) %>%
  select(nsid, fup, sen) %>%
  drop_na()

## h. Marital Status ----
clean$marstat <- df_file %>%
  filter(str_detect(var_low, "^w\\d.?marstat(|yp)$"),
         fld == "eul") %>%
  load_files() %>%
  mutate(marstat_19 = ifelse(w6marstatyp %in% 1:5, w6marstatyp, NA),
         marstat_25 = case_when(w8dmarstat == 1 ~ 1,
                                w8dmarstat %in% c(2, 6) ~ 2,
                                w8dmarstat %in% c(3, 7) ~ 3,
                                w8dmarstat %in% c(4, 8) ~ 4,
                                w8dmarstat %in% c(5, 9) ~ 5)) %>%
  clean_to_long("marstat") %>%
  mutate(marstat = fct_from_dict(marstat, marital_dict))

## i. Religion ----
clean$religion <- df_file %>%
  filter(str_detect(var_low, "(w8drelig7|relig(|1)yp)")) %>%
  load_files() %>%
  rename(w2religy1yp = relig1yp) %>%
  mutate(religion_w8 = comb_lookup(w8drelig7, c(2, 3, 4, 5, 7, 6, NA, 1)),
         across(matches("w[1-7]relig"), 
                ~ comb_lookup(.x, c(1, 2, 6, 4, 7, 3, 5, NA)),
                .names = "{rename_var(.col, 'religion')}")) %>%
  clean_to_long("religion") %>%
  mutate(religion = fct_from_dict(religion, religion_dict),
         fup = fup_dict$next_steps[fup])

## j. Disability ----
clean$disabled <- df_file %>%
  filter(str_detect(var_low, "w\\ddisab|w8ddisea"),
         str_detect(dta, "young_person|ns8")) %>%
  load_files() %>%
  mutate(
    disabled_8 = ifelse(w8ddisea %in% 0:1, w8ddisea + 1, NA),
    across(c(w7disabv1, w6disab), 
           ~ ifelse(.x %in% 1:2, 3 - .x, NA),
           .names = "{rename_var(.col, 'disabled')}"),
    across(c(w4disabyp, w2disabyp, w1disabyp), 
           ~ comb_lookup(.x, c(2, 1, 1)),
           .names = "{rename_var(.col, 'disabled')}")
  ) %>%
  clean_to_long("disabled") %>%
  mutate(disabled = fct_from_dict(disabled, binary_dict),
         fup = fup_dict$next_steps[fup])

## k. Socioeconomic Position ----
clean$social_class <- df_file %>%
  filter(str_detect(var_low, "^(w[1-4](|c)nssecfam|w5cnssec(mum|dad)|w6nssecyp|w7nssec|w8dnssec5)$")) %>%
  load_files() %>%
  mutate(across(matches("^w5"), negative_to_na),
         w5cnssecfam = pmin(w5cnssecmum, w5cnssecdad, na.rm = TRUE)) %>%
  select(-w5cnssecmum, -w5cnssecdad) %>%
  mutate(across(matches("^w\\d"),
                ~ comb_lookup(.x, c(1, 1, 2, 3, 4, 5, 5, 6)),
                .names = "{rename_var(.col, 'nssec')}")) %>%
  clean_to_long("nssec") %>%
  mutate(nssec = fct_from_dict(nssec, nssec_dict),
         fup = fup_dict$next_steps[fup])

## l. Carer ----
clean$carer <- df_file %>%
  filter(str_detect(var_low, "w\\dcar(e|ing)yp")) %>%
  load_files() %>%
  mutate(across(matches("w\\d"), 
                ~ ifelse(.x %in% 1:2, 3 - .x, NA),
                .names = "{rename_var(.col, 'carer')}")) %>%
  clean_to_long("carer") %>%
  mutate(carer = fct_from_dict(carer, binary_dict),
         fup = fup_dict$next_steps[fup])

# 3. Define Sample and Response ----
df_xwave <- glue("{flds$next_steps}/eul/next_steps_longitudinal_file.dta") %>%
  read_dta() %>%
  rename_with(str_to_lower) %>%
  select(nsid, matches("^w\\doutcome")) %>%
  mutate(iid = row_number())

df_fup <- df_xwave %>% 
  select(nsid, iid) %>%
  uncount(length(fup_dict$next_steps), .id = "fup") %>%
  mutate(fup = fup_dict$next_steps[fup])

next_steps <- list()

next_steps$sample <- df_xwave %>%
  select(iid)

next_steps$wide <- reduce(clean, full_join, .init = df_fup) %>%
  select(-nsid) %>%
  left_join(df_fup %>% select(iid, fup), by = c("iid", "fup"))

next_steps$response <- df_xwave %>%
  select(-nsid) %>%
  pivot_longer(-iid) %>%
  filter(value == 1) %>%
  mutate(sweep = str_sub(name, 2, 2) %>% as.integer(),
         fup = fup_dict$next_steps[sweep]) %>%
  select(iid, fup)

saveRDS(next_steps, "Data/next_steps.Rds")
