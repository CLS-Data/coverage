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
  filter(study == "bcs70") %>%
  select(-study)

load_file <- function(file_path, vars = everything()){
  read_dta(file_path, col_select = c(matches("^(BCSID|bcsid)$"), all_of(vars))) %>%
    rename_with(str_to_lower)
}
study_iid <- "bcsid"


# 2. Collect and Clean Data ----
clean <- list()

## a. Ethnicity ----
clean$ethnicity <- df_file %>%
  filter(str_detect(variable, "^(a12_(1|4|7)|c6_14|oa3_1|ethnic|bd7ethnic|e245)$"),
         !(variable == "c6_14" & fld == "10y")) %>%
  load_files() %>%
  mutate(
    across(-bcsid, negative_to_na),
    ethnic = values_to_na(ethnic, 98:99),
    
    ethnic_group_05 = case_when(between(e245, 1, 2) ~ 1,
                          e245 %in% c(3, 6) ~ 5),
    
    ethnic_group_10 = case_when(a12_1 %in% 1:3 & a12_4 %in% c(NA, 1:3) & is.na(a12_7) ~ 1,
                          a12_1 == 4 & is.na(a12_4) & is.na(a12_7) ~ 5,
                          a12_1 == 5 & a12_4 %in% c(NA, 5) & is.na(a12_7) ~ 3,
                          a12_1 %in% 6:7 & a12_4 %in% c(NA, 6:7) & is.na(a12_7) ~ 4,
                          a12_1 == 8 & a12_4 %in% c(NA, 8) & a12_7 %in% c(NA, 8) ~ 6,
                          !is.na(a12_1) & !is.na(a12_4) ~ 2,
                          !is.na(a12_1) & !is.na(a12_7) ~ 2,
                          !is.na(a12_4) & !is.na(a12_7) ~ 2),
    
    ethnic_group_16 = case_when(between(oa3_1, 1, 3) ~ 1,
                          oa3_1 == 4 ~ 5,
                          oa3_1 == 5 ~ 3,
                          between(oa3_1, 6, 7) ~ 4,
                          c6_14 == 1 ~ 1,
                          c6_14 == 2 ~ 5,
                          c6_14 == 5 ~ 2,
                          c6_14 == 6 ~ 6),
    
    ethnic_group_29 = case_when(between(ethnic, 1, 3) ~ 1,
                          between(ethnic, 4, 7) ~ 2,
                          ethnic == 8 ~ 3,
                          between(ethnic, 9, 10) ~ 4,
                          between(ethnic, 12, 14) ~ 5,
                          ethnic %in% c(11, 15, 16) ~ 6),
    
    ethnic_group_34 = case_when(between(bd7ethnic, 1, 3) ~ 1,
                          between(bd7ethnic, 4, 7) ~ 2,
                          bd7ethnic == 8 ~ 3,
                          between(bd7ethnic, 9, 10) ~ 4,
                          between(bd7ethnic, 12, 14) ~ 5,
                          bd7ethnic %in% c(11, 15, 16) ~ 6),
    
  ) %>%
  clean_to_long("ethnic_group") %>%
  mutate(ethnic_group = fct_from_dict(ethnic_group, ethnic_dict)) %>%
  drop_na() %>%
  group_by(bcsid) %>%
  slice_min(fup) %>%
  ungroup() %>%
  select(bcsid, ethnic_group)

## b. Sex ----
clean$sex <- df_file %>%
  filter(fld == "xwave",
         variable == "SEX") %>%
  load_files() %>%
  mutate(sex = values_to_na(sex, 3) %>%
           as_factor() %>%
           fct_drop())

## c. Gender Identity ----

## d. Sexual Orientation ----
clean$sexuality <- df_file %>%
  filter(variable %in% c("B9SXID", "B9WHNC14")) %>%
  load_files() %>%
  mutate(sexuality = case_when(b9sxid %in% c(1, 4) ~ as.integer(b9sxid),
                               b9sxid %in% 2:3 ~ 5 - as.integer(b9sxid)) %>%
           fct_from_dict(sexuality_dict)) %>%
  select(bcsid, sexuality)

## e. Country ----
clean$country <- df_file %>%
  filter(str_detect(var_low, "bd.*cntry")) %>%
  load_files() %>%
  mutate(across(-bcsid, negative_to_na),
         across(-bcsid, ~ values_to_na(.x, c(5, 6, 12)))) %>%
  pivot_longer(-bcsid) %>%
  mutate(sweep = str_sub(name, 3, -6) %>%
           as.integer(),
         fup = fup_dict$bcs70[sweep],
         country = case_when(sweep < 10 ~ as.integer(value),
                             sweep == 10 & value %in% c(1, 3) ~ as.integer(value),
                             sweep == 10 & value == 2 ~ 4L,
                             sweep == 10 & value == 4 ~ 2L) %>%
           fct_from_dict(country_dict)) %>%
  select(bcsid, fup, country) %>%
  drop_na()


## f. Free School Meals ----
clean$fsm <- df_file %>%
  filter(str_detect(var_low, "^m12(6|8)$")) %>%
  load_files() %>% 
  mutate(across(-bcsid, negative_to_na),
         m126 = values_to_na(m126, 3), 
         fup = 10,
         fsm = case_when(m126 == 1 ~ 2,
                         m128 == 1 ~ 2,
                         m126 == 2 ~ 1) %>%
           fct_from_dict(binary_dict)) %>%
  select(bcsid, fup, fsm) %>%
  drop_na()

## g. Special Educational Needs ----
clean$sen <- df_file %>%
  filter(str_detect(var_low, "^(mea11|l7_5)$")) %>%
  load_files() %>%
  mutate(fup = 10,
         sen = case_when(mea11 %in% 4:6 ~ 2,
                         l7_5 == 1 ~ 2,
                         mea11 %in% 1:3 ~ 1,
                         l7_5 == 2 ~ 1) %>%
           fct_from_dict(binary_dict)) %>%
  select(bcsid, fup, sen) %>%
  drop_na()

## h. Marital Status ----
clean$marstat <- df_file %>%
  filter(str_detect(var_low, "^(b960322|marstat2|bd(\\d|10)ms)$")) %>%
  load_files() %>%
  mutate(
    marstat_26 = case_when(b960322 == 1 ~ 1,
                           b960322 %in% 2:3 ~ 2,
                           b960322 %in% 4:6 ~ b960322 - 1),
    marstat_29 = case_when(marstat2 == 1 ~ 1,
                           marstat2 %in% 2:3 ~ 2,
                           marstat2 %in% 4:6 ~ marstat2 - 1),
    marstat_34 = case_when(bd7ms == 1 ~ 2,
                           bd7ms %in% 2:3 ~ 1,
                           bd7ms %in% 4:6 ~ bd7ms - 1),
    marstat_38 = case_when(bd8ms == 1 ~ 1,
                           bd8ms %in% 2:3 ~ 2,
                           bd8ms %in% 4:6 ~ bd8ms - 1,
                           bd8ms %in% 7:8 ~ bd8ms - 4),
    marstat_42 = case_when(bd9ms == 1 ~ 3,
                           bd9ms == 2 ~ 2,
                           bd9ms %in% 3:4 ~ bd9ms + 1,
                           bd9ms == 5 ~ 2,
                           bd9ms %in% 6:7 ~ bd9ms - 2,
                           bd9ms == 8 ~ 1),
    marstat_46 = case_when(bd10ms == 1 ~ 3,
                           bd10ms == 2 ~ 2,
                           bd10ms %in% 3:4 ~ bd10ms + 1,
                           bd10ms == 5 ~ 2,
                           bd10ms %in% 6:7 ~ bd10ms - 2,
                           bd10ms == 8 ~ 1),
  ) %>%
  clean_to_long("marstat") %>%
  mutate(marstat = fct_from_dict(marstat, marital_dict))


## i. Religion ----
clean$religion <- df_file %>%
  filter(str_detect(var_low, "^(gd1a|b960663|religion|q48|b7rnowrl|b7rrelnw)$")) %>%
  load_files() %>% 
  mutate(
    religion_16 = case_when(gd1a %in% 1:3 ~ 2,
                            gd1a %in% 4:5 ~ gd1a - 1,
                            gd1a %in% 6:7 ~ 12 - gd1a,
                            gd1a == 8 ~ 7,
                            gd1a == 10 ~ 1),
    
    religion_26 = case_when(b960663 == 2 ~ 1,
                            q48 %in% 2:22 ~ 2,
                            q48 == 23 ~ 4,
                            q48 == 24 ~ 7,
                            q48 == 25 ~ 3,
                            q48 %in% 26:27 ~ q48 - 21),
    
    religion_29 = case_when(religion == 1 ~ 1,
                            religion %in% 2:9 ~ 2,
                            religion == 10 ~ 4,
                            religion == 11 ~ 7,
                            religion == 12 ~ 3,
                            religion %in% 13:14 ~ religion - 8),
    
    religion_34 = case_when(b7rnowrl == 2 ~ 1,
                            b7rrelnw == 1 ~ 2,
                            b7rrelnw == 2 ~ 6,
                            b7rrelnw == 3 ~ 4,
                            b7rrelnw == 4 ~ 7,
                            b7rrelnw == 5 ~ 3,
                            b7rrelnw == 6 ~ 5)
  ) %>%
  clean_to_long("religion") %>%
  mutate(religion = fct_from_dict(religion, religion_dict))


## j. Disability ----
clean$disabled <- df_file %>%
  filter(str_detect(variable, "^(lsireg2|b7lsiany|b7khldl2|b8khldl2|BD9DISEQ|BD10DISEQ)$")) %>%
  load_files() %>%
  mutate(
    disabled_29 = ifelse(lsireg2 %in% 1:2, 3 - lsireg2, NA),
    
    disabled_34 = case_when(b7khldl2 == 1 ~ 2,
                            b7khldl2 %in% 2:3 ~ 1),
    
    disabled_38 = ifelse(b8khldl2 %in% 1:2, 3 - b8khldl2, NA),
    
    disabled_42 = ifelse(bd9diseq %in% 0:1, bd9diseq + 1, NA),
    
    disabled_46 = ifelse(bd10diseq %in% 0:1, bd10diseq + 1, NA)
  ) %>%
  clean_to_long("disabled") %>%
  mutate(disabled = fct_from_dict(disabled, binary_dict))

## k. Socioeconomic Position ----
clean$social_class <- df_file %>%
  filter(str_detect(variable, "^(bd7ns8|b8nssec|B9CNS8|BD10CNS8)$")) %>%
  load_files() %>%
  mutate(
    nssec_34 = case_when(bd7ns8 %in% 1:2 ~ 1,
                         bd7ns8 %in% 3:5 ~ bd7ns8 - 1,
                         bd7ns8 %in% 6:7 ~ 5),
    
    nssec_42 = case_when(between(b9cns8, 1, 2) ~ 1,
                         b9cns8 %in% 3:5 ~ b9cns8 - 1,
                         b9cns8 %in% 6:7 ~ 5,
                         b9cns8 == 8 ~ 6),
    
    nssec_46 = case_when(between(bd10cns8, 1, 2) ~ 1,
                         bd10cns8 %in% 3:5 ~ bd10cns8 - 1,
                         bd10cns8 %in% 6:7 ~ 5,
                         bd10cns8 == 8 ~ 6)
    
  ) %>%
  clean_to_long("nssec") %>%
  mutate(nssec = fct_from_dict(nssec, nssec_dict))

clean$social_class <- df_file %>%
  filter(str_detect(variable, "^(BD1PSOC|BD2SOC|BD3PSOC|BD4PSOC|rgsc91|sc|b7sc|b8sc|B9CSC)$")) %>%
  load_files() %>%
  mutate(
    rgsc_00 = case_when(bd1psoc == 1 ~ 7,
                        bd1psoc %in% 3:8 ~ 9 - bd1psoc),
    
    rgsc_05 = ifelse(bd2soc %in% 1:6, 7 - bd2soc, NA),
    rgsc_10 = ifelse(bd3psoc %in% 1:6, 7 - bd3psoc, NA),
    rgsc_16 = ifelse(bd4psoc %in% 1:6, 7 - bd4psoc, NA),
    
    rgsc_26 = case_when(rgsc91 %in% 10:20 ~ rgsc91 / 10,
                        rgsc91 %in% 31:32 ~ rgsc91 - 28,
                        rgsc91 %in% 40:50 ~ 1 + (rgsc91 / 10)),
    
    rgsc_29 = case_when(sc %in% 1:2 ~ sc,
                        sc == 3.1 ~ 3,
                        sc == 3.2 ~ 4,
                        sc %in% 4:5 ~ sc + 1),
    
    rgsc_34 = case_when(b7sc %in% 1:2 ~ b7sc,
                        b7sc == 3.1 ~ 3,
                        b7sc == 3.2 ~ 4,
                        b7sc %in% 4:5 ~ b7sc + 1),
    
    rgsc_38 = case_when(b8sc %in% 1:2 ~ b8sc,
                        b8sc == 3.1 ~ 3,
                        b8sc == 3.2 ~ 4,
                        b8sc %in% 4:5 ~ b8sc + 1),
    
    rgsc_42 = case_when(b9csc %in% 1:2 ~ b9csc,
                        b9csc == 3.1 ~ 3,
                        b9csc == 3.2 ~ 4,
                        b9csc %in% 4:5 ~ b9csc + 1),
  ) %>%
  clean_to_long("rgsc") %>%
  mutate(rgsc = fct_from_dict(rgsc, rgsc_dict)) %>%
  full_join(clean$social_class, by = c("bcsid", "fup"))

## l. Carer ----


# 3. Define Sample and Response ----
df_xwave <- glue("{flds$bcs70}/xwave/bcs70_response_1970-2016.dta") %>%
  read_dta() %>%
  rename_with(str_to_lower) %>%
  select(bcsid, matches("^outcme\\d\\d")) %>%
  mutate(iid = row_number())

df_fup <- df_xwave %>% 
  select(bcsid, iid) %>%
  uncount(length(fup_dict$bcs70), .id = "fup") %>%
  mutate(fup = fup_dict$bcs70[fup])

bcs70 <- list()

bcs70$sample <- df_xwave %>%
  select(iid)

bcs70$wide <- reduce(clean, full_join, .init = df_fup) %>%
  select(-bcsid) %>%
  left_join(df_fup %>% select(iid, fup), by = c("iid", "fup"))
  
bcs70$response <- df_xwave %>%
  select(-bcsid) %>%
  pivot_longer(-iid) %>%
  filter(value == 1) %>%
  mutate(sweep = str_sub(name, 7, 8) %>% as.integer(),
         fup = fup_dict$bcs70[sweep]) %>%
  select(iid, fup)

saveRDS(bcs70, "Data/bcs70.Rds")
