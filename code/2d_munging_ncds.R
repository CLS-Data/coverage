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
  filter(study == "ncds") %>%
  select(-study)

load_file <- function(file_path, vars = everything()){
  read_dta(file_path, col_select = c(matches("^(NCDSID|ncdsid)$"), all_of(vars))) %>%
    rename_with(str_to_lower)
}

study_iid <- "ncdsid"

# 2. Collect and Clean Data ----
clean <- list()

## a. Ethnicity ----
clean$ethnicity <- df_file %>%
  filter(variable == "ETHNICID") %>%
  load_files() %>%
  mutate(ethnic_group = fct_from_dict(ethnicid, ethnic_dict)) %>%
  select(ncdsid, ethnic_group)

## b. Sex ----
clean$sex <- df_file %>%
  filter(variable == "N622",
         fld == "xwave") %>%
  load_files() %>%
  mutate(sex = keep_valid_values(n622, 1:2) %>% as_factor()) %>%
  select(ncdsid, sex)

## c. Gender Identity ----

## d. Sexual Orientation ----

## e. Country ----
clean$country <- df_file %>%
  filter(str_detect(var_low, "(n[0-8]region|nd9regn)")) %>%
  load_files() %>%
  mutate(across(-ncdsid, negative_to_na),
         n6region = values_to_na(n6region, 11)) %>%
  pivot_longer(-ncdsid) %>%
  mutate(sweep = str_sub(name, 2, -7) %>%
           as.integer() %>%
           replace_na(9),
         sweep = ifelse(sweep <= 6, sweep + 1, sweep + 2),
         fup = fup_dict$ncds[sweep],
         country = case_when(sweep <= 4 & between(value, 1, 9) ~ 1,
                             sweep <= 4 & between(value, 10, 11) ~ value - 8,
                             sweep > 4 & between(value, 1, 8) ~ 1,
                             sweep > 4 & between(value, 9, 10) ~ value - 7) %>%
           fct_from_dict(country_dict)) %>%
  select(ncdsid, fup, country) %>%
  drop_na()

## f. Free School Meals ----
clean$fsm <- df_file %>%
  filter(str_detect(var_low, "^n(1229|858|2440)$")) %>%
  load_files() %>%
  mutate(across(-ncdsid, ~ negative_to_na(.x) %>%
                  values_to_na(3:4)),
         fsm_11 = case_when(n1229 == 1 | n858 == 1 ~ 2,
                            n1229 == 2 | n858 == 2 ~ 1),
         fsm_16 = 3 - n2440) %>%
  clean_to_long("fsm") %>%
  mutate(fsm = fct_from_dict(fsm, binary_dict)) %>%
  drop_na() # %>%
# group_by(ncdsid) %>%
# mutate(fsm_invar = max(fsm)) %>%
# ungroup() %>%
# mutate(across(matches("^fsm"), ~ fct_from_dict(.x, binary_dict)))

## g. Special Educational Needs ----
clean$sen <- df_file %>%
  filter(str_detect(var_low, "n39[0-2]|n147[6-9]")) %>%
  load_files() %>%
  mutate(
    sen_07 = case_when(n390 == 9 ~ 2,
                       n391 == 9 ~ 2,
                       n392 == 9 ~ 2,
                       n390 %in% c(1:2, 4:8, 10:12) ~ 1),
    
    sen_11 = case_when(n1477 %in% 5:6 ~ 2,
                       n1478 %in% 5:6 ~ 2,
                       n1479 %in% 5:6 ~ 2,
                       n1476 %in% 1:5 ~ 1)
  ) %>%
  clean_to_long("sen") %>%
  mutate(sen = fct_from_dict(sen, binary_dict)) %>%
  drop_na() # %>%
# group_by(ncdsid) %>%
# mutate(sen_invar = max(sen)) %>%
# ungroup() %>%
# mutate(across(matches("^sen"), ~ fct_from_dict(.x, binary_dict)))


## h. Marital Status ----
clean$marstat <- df_file %>%
  filter(str_detect(variable, "^(n5113|n506515|ms|marital|nd7ms|ND[8-9]MS)$")) %>%
  load_files() %>%
  mutate(
    marstat_23 = ifelse(n5113 %in% 1:5, n5113, NA),
    
    marstat_33 = case_when(n506515 == 1 ~ 1,
                           n506515 %in% 2:3 ~ 2,
                           n506515 %in% 4:6 ~ n506515 - 1),
    
    marstat_42 = case_when(ms == 1 ~ 2,
                           ms %in% 2:3 ~ 1,
                           ms %in% 4:6 ~ ms - 1),
    
    marstat_44 = case_when(marital == 1 ~ 1,
                           marital %in% 2:3 ~ 2,
                           marital %in% 4:6 ~ marital - 1),
    
    marstat_46 = case_when(nd7ms == 1 ~ 2,
                           nd7ms %in% 2:3 ~ 1,
                           nd7ms %in% 4:6 ~ nd7ms - 1),
    
    marstat_50 = case_when(nd8ms == 1 ~ 1,
                           nd8ms %in% 2:3 ~ 2,
                           nd8ms %in% 4:6 ~ nd8ms - 1,
                           nd8ms %in% 7:9 ~ nd8ms - 4),
    
    marstat_55 = case_when(nd9ms == 1 ~ 3,
                           nd9ms == 2 ~ 2,
                           nd9ms %in% 3:4 ~ nd9ms + 1,
                           nd9ms == 5 ~ 2,
                           nd9ms %in% 6:7 ~ nd9ms - 2,
                           nd9ms == 8 ~ 1)
    
  ) %>%
  clean_to_long("marstat") %>%
  mutate(marstat = fct_from_dict(marstat, marital_dict))

## i. Religion ----
clean$religion <- df_file %>%
  filter(str_detect(variable, "^(n5967|n504651|religion|N8RNOWRL|N8RRELNW)$")) %>%
  load_files() %>%
  mutate(
    religion_23 = case_when(n5967 == 1 ~ 1,
                            n5967 %in% 2:8 ~ 2,
                            n5967 == 9 ~ 4,
                            n5967 == 10 ~ 7,
                            n5967 == 11 ~ 3,
                            n5967 %in% 12:13 ~ n5967 - 7),
    
    religion_33 = case_when(n504651 == 1 ~ 1,
                            n504651 %in% 2:8 ~ 2,
                            n504651 == 9 ~ 4,
                            n504651 == 10 ~ 7,
                            n504651 == 11 ~ 3,
                            n504651 %in% 12:13 ~ n504651 - 7),
    
    religion_42 = case_when(religion == 1 ~ 1,
                            religion %in% 2:9 ~ 2,
                            religion == 10 ~ 4,
                            religion == 11 ~ 7,
                            religion == 12 ~ 3,
                            religion %in% 13:14 ~ religion - 8),
    
    religion_50 = case_when(n8rnowrl == 2 ~ 1,
                            n8rrelnw %in% 1:3 ~ 2,
                            n8rrelnw == 4 ~ 6,
                            n8rrelnw == 5 ~ 4,
                            n8rrelnw == 6 ~ 7,
                            n8rrelnw == 7 ~ 3,
                            n8rrelnw == 8 ~ 5),
    
    across(matches("religion_\\d\\d"), ~ fct_from_dict(.x, religion_dict))
  ) %>%
  clean_to_long("religion") %>%
  mutate(religion = fct_from_dict(religion, religion_dict))

## j. Disability ----
clean$disabled <- df_file %>%
  filter(str_detect(variable, "^(n281|n2662|n4139|n5740|n504121|n504122|n504125|n504128|n504131|dmdisab|bdmph|lsireg2|n7khldsl|ND8KHLDS|ND9DISEQ)$"),
         dta != "ncds8_imagine") %>%
  load_files() %>%
  mutate(
    disabled_07 = ifelse(n281 %in% 2:3,  4 - n281, NA),
    
    disabled_16 = ifelse(n2662 %in% 1:2, 3 - n2662, NA),
    
    disabled_23 = ifelse(n4139 %in% 1:2, 3 - n4139, NA),
    
    disabled_33 = case_when(n504121 == 2 ~ 1,
                            n504122 == 1 ~ 2,
                            n504125 == 1 ~ 2,
                            n504128 == 1 ~ 2,
                            n504131 == 1 ~ 2),
    
    disabled_42 = ifelse(lsireg2 %in% 1:2, 3 - lsireg2, NA),
    
    disabled_44 = ifelse(bdmph %in% 1:2, 3 - bdmph, NA),
    
    disabled_46 = case_when(n7khldsl == 1 ~ 2,
                            n7khldsl %in% 2:3 ~ 1),
    
    disabled_50 = ifelse(nd8khlds %in% 1:2, 3 - nd8khlds, NA),
    
    disabled_55 = ifelse(nd9diseq %in% 0:1, nd9diseq + 1, NA),
    
  ) %>%
  clean_to_long("disabled") %>%
  mutate(disabled = fct_from_dict(disabled, binary_dict))

## k. Socioeconomic Position ----
clean$social_class <- df_file %>%
  filter(str_detect(variable, "^(nd7ns8|N8NS8|N9CNS8)$"),) %>%
  load_files() %>%
  mutate(
    nssec_46 = case_when(between(nd7ns8, 1, 2) ~ 1,
                         nd7ns8 %in% 3:5 ~ nd7ns8 - 1,
                         nd7ns8 %in% 6:7 ~ 5,
                         nd7ns8 == 8 ~ 6),
    
    nssec_50 = case_when(between(n8ns8, 1, 2) ~ 1,
                         n8ns8 %in% 3:5 ~ n8ns8 - 1,
                         n8ns8 %in% 6:7 ~ 5,
                         n8ns8 == 8 ~ 6),
    
    nssec_55 = case_when(between(n9cns8, 1, 2) ~ 1,
                         n9cns8 %in% 3:5 ~ n9cns8 - 1,
                         n9cns8 %in% 6:7 ~ 5,
                         n9cns8 == 8 ~ 6),
    
    across(matches("nssec_\\d\\d"), ~ fct_from_dict(.x, nssec_dict))
  ) %>%
  clean_to_long("nssec") %>%
  mutate(nssec = fct_from_dict(nssec, nssec_dict))

clean$social_class <- df_file %>%
  filter(str_detect(variable, "^(n236|n660|n190|n1687|n2384|n2393|n6149|n540033|sc|n7sc|N8SC|N9CSC)$"),) %>%
  load_files() %>%
  mutate(
    across(c(n236, n660), 
           list(clean = ~ ifelse(.x == 1, 8, negative_to_na(.x)) %>%
                  values_to_na(c(10, 12)))),
    rgsc_raw_00 = pmin(n236_clean, n660_clean, na.rm = TRUE),
    rgsc_00 = case_when(rgsc_raw_00 %in% 2:7 ~ rgsc_raw_00 - 1),
    
    rgsc_07 = case_when(n190 %in% 2:5 ~ n190 - 1,
                                n190 %in% 6:8 ~ n190 - 2),
    
    rgsc_11 = ifelse(n1687 %in% 1:6, n1687, NA),
    
    across(c(n2384, n2393), list(clean = ~ ifelse(.x %in% 1:7, .x, NA))),
    rgsc_raw_16 = pmin(n2384_clean, n2393_clean, na.rm = TRUE),
    rgsc_16 = case_when(rgsc_raw_16 %in% 1:5 ~ rgsc_raw_16,
                                rgsc_raw_16 %in% 6:7 ~ rgsc_raw_16 - 1),
    
    rgsc_23 = case_when(n6149 == 11 ~ 1,
                                n6149 == 21 ~ 2,
                                n6149 == 31 ~ 3,
                                n6149 == 32 ~ 4,
                                n6149 %in% c(41, 42) ~ 5,
                                n6149 == 52 ~ 6),
    
    rgsc_33 = case_when(n540033 %in% c(10, 20) ~ n540033 / 10,
                                n540033 %in% c(31, 32) ~ n540033 - 28,
                                n540033 %in% c(40, 50) ~ 1 + (n540033 / 10)),
    
    rgsc_42 = case_when(sc %in% 1:2 ~ sc,
                                sc == 3.1 ~ 3,
                                sc == 3.2 ~ 4,
                                sc %in% 4:5 ~ sc + 1),
    
    rgsc_46 = case_when(n7sc %in% 1:2 ~ n7sc,
                                n7sc == 3.1 ~ 3,
                                n7sc == 3.2 ~ 4,
                                n7sc %in% 4:5 ~ n7sc + 1),
    
    rgsc_50 = case_when(n8sc %in% 1:2 ~ n8sc,
                                n8sc == 3.1 ~ 3,
                                n8sc == 3.2 ~ 4,
                                n8sc %in% 4:5 ~ n8sc + 1),
    
    rgsc_55 = case_when(n9csc %in% 1:2 ~ n9csc,
                                n9csc == 3.1 ~ 3,
                                n9csc == 3.2 ~ 4,
                                n9csc %in% 4:5 ~ n9csc + 1),
    
  ) %>%
  clean_to_long("rgsc") %>%
  mutate(rgsc = fct_from_dict(rgsc, rgsc_dict)) %>%
  full_join(clean$social_class, by = c("ncdsid", "fup"))

## l. Carer ----
# df_file %>% 
#   filter(str_detect(lab_low, "care")) %>%
#   select(fld, variable, label) %>%
#   print(n = Inf)
# 
# clean$social_class <- df_file %>%
#   filter(str_detect(variable, "^(n492|n526|n190|n1687|n2384|n2393|n6149|n540033|sc|n7sc|N8SC|N9CSC)$"),) %>%
#   load_files()
# 
# df_file %>% filter(var_low %in% names(clean$social_class), var_low != "ncdsid") %>% select(fld, var_low, label)
# map(clean$social_class, attr, "labels")$sc
# enframe(rgsc_collapse_dict); enframe(rgsc_dict)
# clean$social_class %>% count(n9csc)

# 3. Define Sample and Response and Combine ----
df_xwave <- glue("{flds$ncds}/xwave/ncds_response.dta") %>%
  read_dta() %>%
  rename_with(str_to_lower) %>%
  select(ncdsid, matches("^outcme..$")) %>%
  mutate(iid = row_number())

df_fup <- df_xwave %>% 
  select(ncdsid, iid) %>%
  uncount(length(fup_dict$ncds), .id = "fup") %>%
  mutate(fup = fup_dict$ncds[fup])

ncds <- list()

ncds$sample <- df_xwave %>%
  select(iid)

ncds$wide <- reduce(clean, full_join, .init = df_fup) %>%
  select(-ncdsid) %>%
  left_join(df_fup %>% select(iid, fup), by = c("iid", "fup"))

ncds$response <- df_xwave %>%
  select(-ncdsid) %>%
  pivot_longer(-iid) %>%
  filter(value == 1) %>%
  mutate(sweep = str_sub(name, 7, 8) %>% as.integer(),
         sweep = case_when(sweep <= 6 ~ sweep + 1,
                           is.na(sweep) ~ 8,
                           sweep >= 7 ~ sweep + 2),
         fup = fup_dict$ncds[sweep]) %>%
  select(iid, fup)

saveRDS(ncds, "Data/ncds.Rds")
