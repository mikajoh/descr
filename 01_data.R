## Mikael Poul Johannesson
## August 2017

## Start matter ------------------------------------------------------

library(dplyr)
library(tidyr)
library(haven)
library(readxl)

## Raw data ----------------------------------------------------------

## Norwegian Citisen Panel (Wave 1-7). Available for reasearchers for
## free via NSD.
## Md5sum: fa8591edb6c48ecf3933ac3fcfcc791a
## tools::md5sum("raw/Norwegian citizen panel - wave 1-7 EN.sav")
ncp_raw <- read_sav(
  "raw/Norwegian citizen panel - wave 1-7 EN.sav"
)

## Spreadsheet on which raw vars belongs to which wave and var for the
## NCP data.
## Md5sum: 7f36d1dd9ff3770eb20aa39088ec56f9
## tools::md5sum("raw/vars-resp.xlsx")
vars_resp_raw <- read_excel("raw/vars-resp.xlsx")

## Experiment I: Spreadsheet with candidate attribute variables.
## Md5sum: e885647830a670ab1171fec41f140d6a
## tools::md5sum("raw/vars-exp-represent.xlsx")
vars_repr_raw <- read_excel("raw/vars-exp-represent.xlsx")

## Experiment I: Spreadsheet with candidate attribute labels.
## Md5sum: e051004ba2165a2e45a6b3d966416764
## tools::md5sum("raw/labels-exp-represent.xlsx")
labels_repr_raw <- read_excel("raw/labels-exp-represent.xlsx")

## Experiment II: Spreadsheet with candidate attribute variables.
## Md5sum: c2c53727ae39012eddf95d0d6bb21852
## tools::md5sum("raw/vars-exp-predict.xlsx")
vars_pred_raw <- read_excel("raw/vars-exp-predict.xlsx")

## Experiment II: Spreadsheet with candidate attribute labels.
## Md5sum: d9d490ad65a16d37e5a34c9b5ea0ec38
## tools::md5sum("raw/labels-exp-predict.xlsx")
labels_pred_raw <- read_excel("raw/labels-exp-predict.xlsx")

## Prepare background data -------------------------------------------

## Prep ncp_raw data
ncp_raw$responseid = as.numeric(ncp_raw$responseid)
for (val in c(96, 97, 98, 99, 999)) ncp_raw[ncp_raw == val] <- NA

vars_resp <-
  vars_resp_raw %>%
  gather(wave, variable, matches("^wave_")) %>%
  filter(!is.na(id), !is.na(variable))

## Time constant variables
resp_constant <-
  ncp_raw %>%
  select(responseid, one_of(vars_resp$variable[vars_resp$time_constant == 1])) %>%
  gather(variable, value, -responseid) %>%
  mutate(
    responseid = as.numeric(responseid),
    resp_wave = vars_resp$wave[match(variable, vars_resp$variable)],
    resp_wave = as.numeric(gsub("[^0-9.-]+", "", as.character(resp_wave))),
    variable = vars_resp$id[match(variable, vars_resp$variable)]
  ) %>%
  group_by(responseid, variable) %>%
  arrange(responseid, variable, resp_wave) %>%
  filter(!is.na(value)) %>%
  summarize(value = last(value)) %>%
  ungroup() %>%
  spread(variable, value)

## Time-varying variables
resp_varying <-
  ncp_raw %>%
  select(responseid, one_of(vars_resp$variable[vars_resp$time_constant == 0])) %>%
  gather(variable, value, -responseid) %>%
  extract(variable, "resp_wave", "^.{1,2}(\\d)", remove = FALSE) %>%
  mutate(
    resp_wave = vars_resp$wave[match(variable, vars_resp$variable)],
    resp_wave = as.numeric(gsub("[^0-9.-]+", "", as.character(resp_wave))),
    variable = vars_resp$id[match(variable, vars_resp$variable)]
  ) %>%
  spread(variable, value)

## Recoded data about work to join on data set
resp_work <-
  ncp_raw %>%
  mutate(
    resp_work7 = case_when(
      r7k34_3 == 1 | r7k34_4 == 1 | r7k34_5 == 1 ~ "Nursing or care services",
      r7k34_17 == 1 ~ "Telecommunications/IT",
      r7k34_20 == 1 ~ "Oil/gas",
      r7k34_7 == 1 ~ "Agriculture/fishing",
      r7k33 %in% 2:8 ~ "None"),
    resp_work6 = case_when(
      r6k34_3 == 1 | r6k34_4 == 1 | r6k34_5 == 1 ~ "Nursing or care services",
      r6k34_17 == 1 ~ "Telecommunications/IT",
      r6k34_20 == 1 ~ "Oil/gas",
      r6k34_7 == 1 ~ "Agriculture/fishing",
      r6k33 %in% 2:8 ~ "None"
    )) %>%
  gather(var, resp_work, matches("resp_work")) %>%
  mutate(resp_wave  = as.numeric(gsub(".*(\\d)$", "\\1", var)),
         var = gsub("\\d", "", var)) %>%
  filter(!is.na(resp_work)) %>%
  arrange(desc(resp_wave)) %>%
  group_by(responseid) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    resp_work_value = case_when(
      resp_work == "Nursing or care services" ~ 1,
      resp_work == "Telecommunications/IT" ~ 5,
      resp_work == "Oil/gas" ~ 3,
      resp_work == "Agriculture/fishing" ~ 2,
      resp_work == "None" ~ 6)
  ) %>%
  select(responseid, resp_work, resp_work_value)

## Combine
to_reverse <- unique(vars_resp$id[vars_resp$reverse_scale == 1])
to_reverse <- to_reverse[!is.na(to_reverse)]
resp_combined <-
  full_join(resp_constant, resp_varying, by = "responseid") %>%
  left_join(resp_work, by = "responseid") %>%
  filter(resp_wave %in% 6:7) %>%
  filter(!is.na(resp_subgroup)) %>% # If is.na(subgroup) then resp. did not part. in that wave.
  mutate_at(
    to_reverse,
    function(x) min(x, na.rm = TRUE) + max(x, na.rm = TRUE) - x
  )

## Recode variables to match experiment
resp_01 <-
  resp_combined %>%
  mutate(
    resp_age_group = ifelse(resp_age %in% 1:2, "18-35 yrs old", "36+ yrs old"),
    resp_education_group = case_when(
      resp_education %in% 3:4 ~ "Have higher education",
      resp_education == 1     ~ "Not higher education"),
    resp_politics_complicated = case_when(
      resp_politics_complicated %in% 5:7 ~ "Politics is complicated",
      resp_politics_complicated %in% 1:4 ~ "Politics is not complicated"),
    resp_politics_complicated = factor(resp_politics_complicated),
    resp_political_interest = case_when(
      resp_political_interest %in% 4:5 ~ "More than somewhat\ninterested in politics",
      resp_political_interest %in% 1:3 ~ "Somewhat or less\ninterested in politics"),
    resp_political_interest = factor(resp_political_interest),
    resp_like_frp_group = case_when(
      resp_like_frp %in% 5:7 ~ "Like populist Party",
      resp_like_frp %in% 1:4 ~ "Does not like populist\nparty"),
    resp_like_frp = factor(resp_like_frp),
    resp_dem_satisfaction_group = case_when(
      resp_dem_satisfaction %in% 4:5 ~ "Satisfied with democracy",
      resp_dem_satisfaction %in% 1:3 ~ "Not satisfied with democracy"),
    resp_dem_satisfaction_group = factor(resp_dem_satisfaction_group)
  )

resp_02 <-
  resp_01 %>%
  mutate(
    resp_relationship = case_when(
      resp_marital_status %in% 4                      ~ "Cohabitant",
      resp_marital_status %in% 2                      ~ "Married",
      resp_marital_status %in% c(1, 3, 5, 6, 7, 8, 9) ~ "Live alone"),
    resp_relationship_value = case_when(
      resp_marital_status %in% 4                      ~ 3,
      resp_marital_status %in% 2                      ~ 1,
      resp_marital_status %in% c(1, 3, 5, 6, 7, 8, 9) ~ 2),
    resp_religion = case_when(
      resp_religion %in% c(1, 2, 3, 4, 5) ~ "Christianity",
      resp_religion %in% 7                ~ "Islam",
      resp_religion %in% 10               ~ "None",
      resp_religion %in% c(6, 8, 9)       ~ "Other"),
    resp_religion_value = case_when(
      resp_religion == "Christianity" ~ 2,
      resp_religion == "Islam"        ~ 3,
      resp_religion == "None"         ~ 1,
      resp_religion == "Other"        ~ 4)
  )

resp_03 <-
  resp_02 %>%
  mutate(
    resp_agree_opinion_1 = case_when(
      resp_reduce_ineq %in% 5:7 ~ "Agree that the state should reduce income inequality",
      resp_reduce_ineq %in% 1:4 ~ "Do not agree"),
    resp_agree_opinion_1 = factor(resp_agree_opinion_1, levels = rev(levels(factor(resp_agree_opinion_1)))),
    resp_agree_opinion_2 = case_when(
      resp_ref_social_rights %in% 5:7 ~ "Agree that refugees should have the same right to social assistance",
      resp_ref_social_rights %in% 1:4 ~ "Do not agree"),
    resp_agree_opinion_2 = factor(resp_agree_opinion_2, levels = rev(levels(factor(resp_agree_opinion_2)))),
    resp_agree_opinion_3 = case_when(
      resp_emi_red_abroad %in% 5:7 ~ "Agree that emission reductions should be done abroad",
      resp_emi_red_abroad %in% 1:4 ~ "Do not agree"),
    resp_agree_opinion_3 = factor(resp_agree_opinion_3, levels = rev(levels(factor(resp_agree_opinion_3)))),
    resp_age_2 = case_when(
      resp_age == 1 ~ "Aged 18-25",
      resp_age == 2 ~ "Aged 26-35",
      resp_age == 3 ~ "Aged 36-45",
      resp_age == 4 ~ "Aged 46-55",
      resp_age == 5 ~ "Aged 56-65",
      resp_age == 6 ~ "Aged 66-75",
      resp_age == 7 ~ "Aged 76+"),
    resp_region_2 = forcats::fct_recode(
      factor(resp_region),
      "Oslo" = "1",
      "Eastern Norway" = "2",
      "Southern Norway" = "3",
      "Western Norway" = "4",
      "Central Norway" = "5",
      "Northern Norway" = "6"),
    resp_education_2 = forcats::fct_recode(
      factor(resp_education),
      "Elementary school" = "1",
      "High School" = "2",
      "Higher education" = "3"),
    resp_gender_2 = forcats::fct_recode(
      factor(resp_gender),
      "Male" = "1",
      "Female" = "2")
  )

resp <- resp_03


## Prepare Experiment I: Candidate choice ----------------------------

vars_repr <-
  vars_repr_raw %>%
  filter(!is.na(id)) %>%
  filter(candidate_constant == 0)

vars_repr_exp <-
  vars_repr_raw %>%
  filter(!is.na(id)) %>%
  filter(candidate_constant == 1)

labels_repr <-
  labels_repr_raw %>%
  filter(!is.na(id), !grepl("_dist", id)) %>%
  mutate(id = paste0("cand_", id)) %>%
  select(id, label, value)

## Candidate-level data (2 cand per exp)
repr_cand <-
  ncp_raw %>%
  dplyr::select(responseid, one_of(unique(vars_repr$variable))) %>%
  mutate_all(funs(as.numeric)) %>%
  mutate_at(
    unique(vars_repr$variable[vars_repr$reverse_scale == 1]),
    function(x) min(x, na.rm = TRUE) + max(x, na.rm = TRUE) - x
  ) %>%
  gather(variable, value, -responseid, na.rm = TRUE) %>%
  mutate(
    cand_candidate = vars_repr$candidate[match(variable, vars_repr$variable)],
    exp_version = vars_repr$round[match(variable, vars_repr$variable)],
    exp_version_label = vars_repr$round_label[match(variable, vars_repr$variable)],
    variable = vars_repr$id[match(variable, vars_repr$variable)]
  ) %>%
  spread(variable, value)

## Experiment-level data (2 exp per resp)
repr_exp <-
  ncp_raw %>%
  dplyr::select(responseid, one_of(unique(vars_repr_exp$variable))) %>%
  mutate_all(funs(as.numeric)) %>%
  gather(variable, value, -responseid, na.rm = TRUE) %>%
  mutate(
    exp_version = vars_repr_exp$round[match(variable, vars_repr_exp$variable)],
    variable = vars_repr_exp$id[match(variable, vars_repr_exp$variable)]
  ) %>%
  spread(variable, value)

## Combine
repr_combined <- repr_cand %>%
  left_join(repr_exp, by = c("responseid", "exp_version")) %>%
  mutate(
    post = case_when(
      exp_decision == cand_candidate ~ 1,
      exp_decision != cand_candidate ~ 0),
    resp_wave = 6
  ) %>%
  filter(!is.na(post))

## Match with resp data
repr <-
  repr_combined %>%
  left_join(resp, by = c("responseid", "resp_wave")) %>%
  mutate(
    cand_matched_age = ifelse(abs(cand_age - resp_age) > 0, "Different", "Same (+/- 5 yrs)"),
    cand_matched_gender = ifelse(resp_gender == cand_gender, "Same", "Different"),
    cand_matched_education = case_when(
      cand_education == 1 & resp_education == 1     ~ "Same",
      cand_education == 2 & resp_education == 2     ~ "Same",
      cand_education %in% 3:4 & resp_education == 3 ~ "Same"),
    cand_matched_education = ifelse(is.na(cand_matched_education) & !is.na(resp_education), "Different", cand_matched_education),
    cand_matched_region = ifelse(cand_region == resp_region, "Same", "Different"),
    cand_matched_relationship = ifelse(cand_relationship == resp_relationship_value, "Same", "Different"),
    cand_matched_religion = ifelse(cand_religion == resp_religion_value, "Same", "Different"),
    cand_matched_work = ifelse(cand_work == resp_work_value, "Same", "Different"),
    cand_matched_opinion_1 = abs(cand_opinion_1 - resp_reduce_ineq),
    cand_matched_opinion_1 = case_when(
      cand_matched_opinion_1 == 0:1 ~ "Same position",
      cand_matched_opinion_1 == 2:3 ~ "Difference of ±2/3",
      cand_matched_opinion_1 == 4:6 ~ "Difference of ±4/5/6"),
    cand_matched_opinion_1 = factor(cand_matched_opinion_1, levels = c("Same position", "Difference of ±2/3", "Difference of ±4/5/6")),
    cand_matched_opinion_2 = abs(cand_opinion_2 - resp_ref_social_rights),
    cand_matched_opinion_2 = case_when(
      cand_matched_opinion_2 == 0:1 ~ "Same position",
      cand_matched_opinion_2 == 2:3 ~ "Difference of ±2/3",
      cand_matched_opinion_2 == 4:6 ~ "Difference of ±4/5/6"),
    cand_matched_opinion_2 = factor(cand_matched_opinion_2, levels = c("Same position", "Difference of ±2/3", "Difference of ±4/5/6")),
    cand_matched_opinion_3 = abs(cand_opinion_3 - resp_emi_red_abroad),
    cand_matched_opinion_3 = case_when(
      cand_matched_opinion_3 == 0:1 ~ "Same position",
      cand_matched_opinion_3 == 2:3 ~ "Difference of ±2/3",
      cand_matched_opinion_3 == 4:6 ~ "Difference of ±4/5/6"),
    cand_matched_opinion_3 = factor(cand_matched_opinion_3, levels = c("Same position", "Difference of ±2/3", "Difference of ±4/5/6"))) %>%
  gather_("id", "value", unique(labels_repr$id)) %>%
  left_join(labels_repr, by = c("id", "value")) %>%
  select(-value) %>%
  spread(id, label) %>%
  mutate(
    exp_version_2 = case_when(
      exp_version == 3     ~ "Given both descriptive\nand substantive information",
      exp_version %in% 1:2 ~ "Given either descriptive\nor substantive information"),
    exp_version_2 = factor(exp_version_2, levels = rev(levels(factor(exp_version_2))))
  )

for (var in unique(labels_repr$id)) {
  repr[[var]] <- factor(
    repr[[var]],
    levels = labels_repr$label[labels_repr$id == var]
  )
}

## Prepare Experiment II: Candidate prediction -----------------------

vars_pred <-
  vars_pred_raw %>%
  filter(!is.na(id)) %>%
  filter(candidate_constant == 0)

vars_pred_exp <-
  vars_pred_raw %>%
  filter(!is.na(id)) %>%
  filter(candidate_constant == 1)

labels_pred <-
  labels_pred_raw %>%
  filter(!is.na(id)) %>%
  mutate(id = paste0("cand_", id)) %>%
  select(id, label, label_old)

## Candidate-level data (2 per exp)
pred_cand <-
  ncp_raw %>%
  dplyr::select(responseid, one_of(unique(vars_pred$variable))) %>%
  gather(variable, value, -responseid, na.rm = TRUE) %>%
  filter(value != "") %>%
  mutate(
    cand_candidate = vars_pred$candidate[match(variable, vars_pred$variable)],
    exp_attitude = vars_pred$attitude[match(variable, vars_pred$variable)],
    exp_attitude_label = vars_pred$attitude_label[match(variable, vars_pred$variable)],
    variable = vars_pred$id[match(variable, vars_pred$variable)]
  ) %>%
  spread(variable, value)

## Experiment-level data (3 per resp)
pred_exp <-
  ncp_raw %>%
  dplyr::select(responseid, one_of(unique(vars_pred_exp$variable))) %>%
  gather(variable, value, -responseid, na.rm = TRUE) %>%
  mutate(
    exp_attitude = vars_pred_exp$attitude[match(variable, vars_pred_exp$variable)],
    exp_type = vars_pred_exp$type[match(variable, vars_pred_exp$variable)],
    variable = vars_pred_exp$id[match(variable, vars_pred_exp$variable)]
  ) %>%
  spread(variable, value)

## Combine
pred_combined <-
  pred_cand %>%
  left_join(pred_exp, by = c("responseid", "exp_attitude")) %>%
  mutate(
    post = ifelse(exp_decision == cand_candidate, 1, 0),
    resp_wave = 7
  ) %>%
  filter(!is.na(post))

pred <-
  pred_combined %>%
  left_join(resp, by = c("responseid", "resp_wave")) %>%
  gather(id, label_old, matches("cand_")) %>%
  left_join(labels_pred, by = c("id", "label_old")) %>%
  mutate(label = ifelse(is.na(label), label_old, label)) %>%
  select(-label_old) %>%
  spread(id, label) %>%
  mutate(
    exp_attitude_label_2 = case_when(
      exp_attitude == 1 ~ "The gov should reduce\nincome inequality",
      exp_attitude == 2 ~ "Refugees should have\nsame right to social\nassistance as Norwegians",
      exp_attitude == 3 ~ "Emission reductions\nshould be done abroad"),
    exp_attitude_label_2 = factor(exp_attitude_label_2)
  )

for (var in unique(labels_pred$id)) {
  pred[[var]] <- factor(
    pred[[var]],
    levels = labels_pred$label[labels_pred$id == var]
  )
}

## Save data ---------------------------------------------------------

write.csv(
  resp,
  file = "data/data-respondents.csv",
  row.names = FALSE
)

write.csv(
  repr,
  file = "data/data-experiment-one.csv",
  row.names = FALSE
)

write.csv(
  pred,
  file = "data/data-experiment-two.csv",
  row.names = FALSE
)

## END ---------------------------------------------------------------
