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
  filter(study == "mcs") %>%
  select(-study)

load_mcs <- function(file_path, vars = everything()){
  read_dta(file_path, col_select = c(MCSID, matches("(C|P)NUM00"), all_of(vars))) %>%
    rename_with(str_to_lower)
}

var_selection_to_long <- function(df_vars){
  df_vars %>%
    select(file_path, variable) %>%
    chop(variable) %$%
    map2_dfr(
      file_path, variable, 
      ~ load_mcs(.x, .y) %>%
        pivot_longer(-mcsid,
                     cols_vary = "slowest",
                     names_to = c("sweep", ".value"),
                     names_pattern = "(.)(.*)")
    ) %>%
    mutate(sweep = match(sweep, letters),
           fup = fup_dict$mcs[sweep])
}

# 2. Collect and Clean Data ----
clean <- list()

df_cm <- glue("{flds$mcs}/xwave/mcs_longitudinal_family_file.dta") %>%
  read_dta() %>%
  rename_with(str_to_lower) %>%
  filter(nocmhh %in% 1:2) %>%
  uncount(nocmhh, .id = "cnum") %>%
  select(mcsid, cnum) %>%
  mutate(iid = row_number())

## a. Ethnicity ----
extract_labels("^.DC(06E|E06)00$") %>%
  labels_to_grid()

clean$ethnicity <- df_file %>%
  filter(str_detect(variable, "^.DC(06E|E06)00$")) %>%
  var_selection_to_long() %>%
  mutate(cnum = as.integer(cnum00),
         ethnic_group = case_when(between(dc06e00, 1, 6) ~ as.integer(dc06e00),
                                  between(dce0600, 1, 6) ~ as.integer(dce0600)) %>%
           fct_from_dict(ethnic_dict)) %>%
  select(mcsid, cnum, fup, ethnic_group) %>%
  drop_na() %>%
  group_by(mcsid, cnum) %>%
  slice_min(fup) %>%
  ungroup() %>%
  select(mcsid, cnum, ethnic_group)


## b. Sex ----
df_file %>%
  filter(str_detect(dta, "hhgrid"),
         str_detect(var_low, "csex")) %>%
  select(variable, value_labels) %>%
  deframe()

clean$sex <- df_file %>%
  filter(str_detect(dta, "hhgrid"),
         str_detect(var_low, "csex")) %>%
  select(file_path, variable) %$%
  map2_dfr(
    file_path, variable, 
    ~ load_mcs(.x, .y) %>%
      pivot_longer(-mcsid,
                   cols_vary = "slowest",
                   names_to = c("sweep", ".value"),
                   names_pattern = "(.)(.*)")
  ) %>%
  filter(cnum00 %in% 1:2) %>%
  mutate(sweep = match(sweep, letters),
         fup = fup_dict$mcs[sweep],
         cnum = as.integer(cnum00),
         sex = case_when(hcsex00 %in% 1:2 ~ as.integer(hcsex00),
                         csex0000 %in% 1:2 ~ as.integer(csex0000)) %>%
           factor(labels = c("Male", "Female"))) %>%
  select(mcsid, cnum, fup, sex) %>%
  drop_na() %>%
  group_by(mcsid, cnum) %>%
  slice_min(fup) %>%
  ungroup() %>%
  select(mcsid, cnum, sex)

## c. Gender Identity ----
clean$gender <- df_file %>%
  filter(variable == "GCGNID00") %$%
  load_mcs(file_path, variable) %>%  
  mutate(cnum = as.integer(gcnum00)) %>%
  left_join(clean$sex, by = c("mcsid", "cnum")) %>%
  mutate(
    fup = 17,
    gender_identity = case_when(gcgnid00 %in% 1:2 & gcgnid00 == as.integer(sex) ~ 1,
                                gcgnid00 %in% 1:2 & gcgnid00 != as.integer(sex) ~ 2,
                                gcgnid00 %in% 4:6 ~ 3,
                                gcgnid00 == 3 ~ 4) %>%
      factor(1:4, c("Cisgender", "Transgender", "Non-Binary/ Gender Fluid / Adrogynous", "Other"))
  ) %>%
  select(mcsid, cnum, fup, gender_identity)

## d. Sexual Orientation ----
# clean$sexuality <- df_file %>%
#   filter(variable %in% c("GCSXID00", "GCSXAT00",
#                          "FCROMG00", "FCROMB00")) %>%
#   var_selection_to_long() %>%
#   filter(cnum00 %in% 1:2) %>%
#   mutate(cnum = as.integer(cnum00)) %>%
#   left_join(clean$sex, by = c("mcsid", "cnum")) %>%
#   mutate(
#     across(c(cromg00, cromb00), negative_to_na),
#     csxat00 = values_to_na(csxat00, 7:9),
#     csxid00 = negative_to_na(csxid00) %>% values_to_na(7:8),
#     
#     homo_att_ever = case_when(sweep == 6 & sex == "Male" ~ 2 - cromb00,
#                               sweep == 6 & sex == "Female" ~ 2 - cromg00,
#                               sweep == 7 & between(csxat00, 2, 5) ~ 1,
#                               sweep == 7 & csxat00 %in% c(1, 6) ~ 0),
#     
#     homo_att_half = case_when(between(csxat00, 3, 5) ~ 1,
#                               csxat00 %in% c(1, 2, 6) ~ 0),
#     
#     homo_att_maj = case_when(between(csxat00, 3, 5) ~ 1,
#                              csxat00 %in% c(1, 2, 6) ~ 0),
#     
#     homo_att_exc = case_when(csxat00 == 5 ~ 1,
#                              csxat00 %in% c(1:4, 6) ~ 0),
#     
#     sexuality = case_when(csxid00 %in% 1:2 ~ 1,
#                           csxid00 == 3 ~ 2,
#                           csxid00 %in% 4:5 ~ 3,
#                           csxid00 == 6 ~ 4) %>%
#       factor(labels = sexuality_dict),
#     
#     sexuality_ever = ifelse(between(csxid00, 2, 5), 1, 0),
#     homo_att_half = ifelse(between(csxid00, 3, 5), 1, 0),
#     homo_att_maj = ifelse(between(csxid00, 4, 5), 1, 0),
#     homo_att_exc = ifelse(csxid00 == 5, 1, 0),
#   ) %>%
#   select(mcsid, cnum, sweep, matches("^homo_")) %>%
#   add_na_col(matches("^homo_")) %>%
#   filter(row_miss < max(row_miss)) %>%
#   select(-row_miss)

clean$sexuality <- df_file %>%
  filter(variable == "GCSXID00") %$%
  load_mcs(file_path, variable) %>%
  mutate(cnum = as.integer(gcnum00),
         fup = 17,
         sexuality = case_when(gcsxid00 %in% 1:2 ~ 1,
                               gcsxid00 == 3 ~ 2,
                               gcsxid00 %in% 4:5 ~ 3,
                               gcsxid00 == 6 ~ 4) %>%
           fct_from_dict(sexuality_dict)) %>%
  select(mcsid, cnum, fup, sexuality)

## e. Country ----
clean$country <- df_file %>%
  filter(str_detect(var_low, "^.actry00$"),
         str_detect(dta, "family_derived"))  %$%
  map2(file_path, variable, load_mcs)  %>%
  reduce(full_join, by = "mcsid") %>%
  pivot_longer(-mcsid) %>%
  mutate(sweep = str_sub(name, 1, 1) %>% match(letters),
         fup = fup_dict$mcs[sweep],
         country = ifelse(between(value, 1, 4), value, NA) %>%
           fct_from_dict(country_dict)) %>%
  select(mcsid, fup, country) %>%
  inner_join(df_cm %>% select(-iid), by = "mcsid") %>%
  relocate(cnum, .after = fup) %>%
  drop_na()

clean$country <- df_file %>%
  filter(var_low == "gactry00") %$%
  load_mcs(file_path) %>%
  filter(gcnum00 %in% 1:2) %>%
  select(mcsid, gcnum00, gactry00) %>%
  mutate(fup = 17,
         cnum = as.integer(gcnum00),
         country = ifelse(between(gactry00, 1, 4), gactry00, NA) %>%
           fct_from_dict(country_dict)) %>%
  select(mcsid, cnum, fup, country) %>%
  drop_na() %>%
  bind_rows(clean$country)

## f. Free School Meals ----
clean$fsm <- df_file %>%
  filter(str_detect(var_low, "^.p(schd|frem|elfr)00$")) %>%
  select(file_path, variable) %>%
  chop(variable) %$%
  map2_dfr(file_path, variable, 
           ~ load_mcs(.x, .y) %>%
             pivot_longer(-mcsid,
                          names_pattern = "(.)(.*)",
                          names_to = c("sweep", ".value")) %>%
             mutate(sweep = match(sweep, letters))) %>%
  mutate(across(-mcsid, negative_to_na),
         across(-c(mcsid, sweep, cnum00, pnum00),
                ~ values_to_na(.x, 3:4))) %>%
  select(-ppnum00) %>% 
  mutate(cnum = as.integer(cnum00),
         pnum = as.integer(pnum00),
         fup = fup_dict$mcs[sweep],
         fsm_rec = case_when(fup == 5 & pschd00 == 2 ~ 1,
                             pfrem00 %in% c(1, 2) ~ pfrem00),
         fsm_elig = ifelse(pelfr00 == 1, 2, 1),
         fsm = case_when(fsm_rec == 2 ~ 2,
                         fsm_elig == 2 ~ 2,
                         fsm_rec == 1 ~ 1,
                         fsm_elig == 1 ~ 1) %>%
           fct_from_dict(binary_dict)) %>%
  select(mcsid, cnum, fup, fsm) %>%
  drop_na() %>%
  group_by(mcsid, cnum, fup) %>%
  slice(1) %>%
  ungroup()

## g. Special Educational Needs ----
# clean$sen <- df_file %>%
#   # filter(str_detect(var_low, "^[d-f]p(csen|sens|rasn)0(0|[a-z])$")) %>%
#   filter(str_detect(var_low, "^ep(csen|sens)00$")) %>%
#   select(file_path, variable) %>%
#   chop(variable) %$%
#   map2_dfr(file_path, variable, load_mcs) %>%
#   mutate(cnum = as.integer(ecnum00),
#          fup = 11,
#          sen = ifelse(epcsen00 %in% 1:2 ~ 3 - epcsen00, NA) %>%
#            fct_from_dict(binary_dict)),
#          # sen_confirm = case_when(epsens00 == 1 ~ 1,
#          #                         sen_told == 0 ~ 0,
#          #                         epsens00 %in% 2:3 ~ 0)
#          ) %>%
#   select(mcsid, cnum, sen) %>%
#   drop_na()

clean$sen <- df_file %>%
  filter(str_detect(var_low, "^[d-f]pcsen00$")) %>%
  var_selection_to_long()%>%
  mutate(cnum = as.integer(cnum00),
         sen = ifelse(pcsen00 %in% 1:2, 3 - pcsen00, NA) %>%
           fct_from_dict(binary_dict)) %>%
  select(mcsid, cnum, fup, sen) %>%
  drop_na() %>%
  group_by(mcsid, cnum, fup) %>%
  slice(1) %>%
  ungroup()

## h. Marital Status ----

## i. Religion ----
clean$religion <- df_file %>%
  filter(str_detect(dta, "cm_interview"),
         str_detect(var_low, "^.crel.00")) %>%
  select(file_path, variable) %>%
  chop(variable) %$%
  map2_dfr(file_path, variable, 
           ~ load_mcs(.x, .y) %>%
             rename_with(str_to_lower) %>%
             rename_with(~ str_remove(.x, "_r20$")) %>%
             pivot_longer(-mcsid,
                          cols_vary = "slowest",
                          names_to = c("sweep", ".value"),
                          names_pattern = "(.)(.*)")) %>%
  mutate(sweep = match(sweep, letters),
         fup = fup_dict$mcs[sweep],
         cnum = as.integer(cnum00),
         religion = case_when(
           crele00 %in% c(1:2, 4) ~ crele00,
           crele00 == 6 ~ 3,
           crele00 == 7 ~ 5,
           crele00 == 3 ~ 6,
           crele00 == 5 ~ 7,
           crelw00 %in% c(1:2, 4) ~ crelw00,
           crelw00 == 6 ~ 3,
           crelw00 == 7 ~ 5,
           crelw00 == 3 ~ 6,
           crelw00 == 5 ~ 7,
           crels00 == 1 ~ 1,
           crels00 %in% 2:4 ~ 2,
           crels00 == 5 ~ 3,
           crels00 == 9 ~ 4,
           crels00 == 7 ~ 5,
           crels00 == 6 ~ 6,
           crels00 == 8 ~ 7,
           creln00 == 6 ~ 1,
           creln00 %in% 1:4 ~ 2
         ) %>%
           fct_from_dict(religion_dict)
  ) %>%
  select(mcsid, cnum, fup, religion) %>%
  drop_na()

clean$religion <- df_file %>%
  filter(str_detect(dta, "parent_derived"),
         str_detect(var_low, "(elig00|drlg00)")) %>%
  var_selection_to_long() %>%
  filter(elig00 == 1) %>%
  mutate(religion_raw = ifelse(!is.na(ddrlg00), ddrlg00, drlg00),
         religion = case_when(religion_raw == 8 ~ 1,
                              religion_raw %in% 1:4 ~ religion_raw + 1,
                              religion_raw %in% 5:6 ~ 12 - religion_raw) %>%
           fct_from_dict(religion_dict)) %>%
  select(mcsid, fup, religion) %>%
  filter(fup < 14) %>%
  drop_na() %>%
  inner_join(df_cm %>% select(-iid), by = "mcsid") %>%
  bind_rows(clean$religion)


## j. Disability ----
clean$disabled <- df_file %>%
  filter(str_detect(var_low, "^.(c|p)cls(i|l)00$")) %>%
  var_selection_to_long() %>%
  mutate(cnum = as.integer(cnum00),
         disabled = case_when(pclsi00 == 2 ~ 1,
                              sweep %in% 2:3 & pclsl00 == 2 ~ 1,
                              sweep %in% 4:6 & pclsl00 == 3 ~ 1,
                              sweep %in% 2:3 & pclsl00 == 1 ~ 2,
                              sweep %in% 4:6 & pclsl00 %in% 1:2 ~ 2,
                              cclsi00 == 2 ~ 1,
                              cclsl00 == 3 ~ 1,
                              cclsl00 %in% 1:2 ~ 2) %>%
           fct_from_dict(binary_dict)) %>%
  select(mcsid, cnum, fup, disabled)

## k. Socioeconomic Position ----
clean$social_class <- df_file %>%
  filter(str_detect(var_low, "^.(|d)d05(s|c)00$")) %>%
  var_selection_to_long() %>%
  mutate(across(matches("d05(s|c)00"), ~ values_to_na(.x, c(-1, 8))),
         social_class_curr = ifelse(!is.na(dd05s00), dd05s00, d05s00),
         social_class_past = as.integer(dd05c00),
         social_class = ifelse(!is.na(social_class_curr), social_class_curr, social_class_past)) %>%
  drop_na(social_class) %>%
  group_by(mcsid, fup) %>%
  summarise(nssec = min(social_class) %>% 
              fct_from_dict(nssec_dict),
            .groups = "drop")

## l. Carer ----
clean$carer <- df_file %>%
  filter(variable == "GCCARE00") %>%
  var_selection_to_long() %>% 
  mutate(cnum = as.integer(cnum00),
         carer = case_when(ccare00 == 1 ~ 2,
                           ccare00 == 2 ~ 1) %>%
           fct_from_dict(binary_dict)) %>%
  select(mcsid, cnum, fup, carer) %>%
  drop_na()

# 3. Define Sample and Response ----
df_fup <- df_cm %>% 
  uncount(length(fup_dict$mcs), .id = "fup") %>%
  mutate(fup = fup_dict$mcs[fup])

mcs <- list()

mcs$sample <- df_cm %>%
  select(iid)

mcs$wide <- reduce(clean, full_join, .init = df_fup) %>%
  select(-c(mcsid, cnum)) %>%
  left_join(df_fup %>% select(iid, fup), by = c("iid", "fup"))

mcs$response <- map_dfr(1:7,
                        ~ glue("{flds$mcs}/{fup_dict$mcs[.x]}y/mcs{.x}_cm_interview.dta") %>%
                          read_dta(col_select = c(MCSID, matches("^.CNUM00"))) %>%
                          select(mcsid = MCSID, cnum = matches("CNUM")),
                        .id = "sweep") %>%
  mutate(cnum = as.integer(cnum),
         fup = fup_dict$mcs[as.integer(sweep)]) %>%
  inner_join(df_cm, by = c("mcsid", "cnum")) %>%
  select(iid, fup)

saveRDS(mcs, "Data/mcs.Rds")
