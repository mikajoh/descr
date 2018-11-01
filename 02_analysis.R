## Mikael Poul Johannesson & Dominik Duell
## August 2017

## Start Matter ------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(lmtest)
library(sandwich)

source("00_utils.R")

set.seed(2016)

## Raw data ----------------------------------------------------------

## Data for experiment I (candidate choice). See `01_data.R`.
## Md5sum: c8c626eda905dc6052876f4b2975142f
## tools::md5sum("data/data-experiment-one.csv")
exp_one_raw <- read.csv("data/data-experiment-one.csv")

## Data for experiment II (candidate prediction). See `01_data.R`.
## Md5sum: dcd2a2412a6865422059f311107a21f2
## tools::md5sum("data/data-experiment-two.csv")
exp_two_raw <- read.csv("data/data-experiment-two.csv")

## Treatment labels.
## Md5sum: ba9b9f40178e395a195ee30b7659ee5d
## tools::md5sum("data/labels-vars.xlsx")
labels_treatment_raw <- read_excel("data/labels-vars.xlsx")

## Prep data ---------------------------------------------------------

## DESCRIPTION
## The prefix of the variable denotes the unit level, as follows:
## 'resp_' are respondent-level variables, 'exp_' are experiment-level
## variables (three for each respondent), 'cand_' are candidate-level
## variables (two for each experiment). Variable 'post' is also
## candidate-level (1/0 for whether that candidate were choosen).

## Experiment one: Choosing representative
exp_one <-
  exp_one_raw %>%
  mutate(
    cand_gender = lvls_reorder(factor(cand_gender), c(2, 1)),
    cand_education = lvls_reorder(factor(cand_education), c(1, 2, 4, 3)),
    cand_region = lvls_reorder(factor(cand_region), c(4, 2, 5, 6, 1, 3)),
    cand_work = lvls_reorder(factor(cand_work), c(4, 1, 2, 3, 5, 6)),
    cand_relationship = lvls_reorder(factor(cand_relationship), c(2, 1, 3))
  ) %>%
  mutate_at(
    vars(cand_matched_opinion_1, cand_matched_opinion_2, cand_matched_opinion_3),
    function(x) lvls_reorder(factor(x), c(2, 1, 3))
  ) %>%
  mutate(
    cand_opinion_1 = lvls_reorder(factor(cand_opinion_1), c(3, 7, 5, 2, 1, 4, 6)),
    cand_opinion_2 = lvls_reorder(factor(cand_opinion_2), c(3, 7, 5, 2, 1, 4, 6)),
    cand_opinion_3 = lvls_reorder(factor(cand_opinion_3), c(3, 7, 5, 2, 1, 4, 6)),
    cand_fullMatched_opinion_1 = abs(as.numeric(cand_opinion_1) - resp_reduce_ineq),
    cand_fullMatched_opinion_1 = case_when(
      cand_fullMatched_opinion_1 == 0 ~ "Same position",
      cand_fullMatched_opinion_1 == 1 ~ "Difference of 1",
      cand_fullMatched_opinion_1 == 2 ~ "Difference of 2",
      cand_fullMatched_opinion_1 == 3 ~ "Difference of 3",
      cand_fullMatched_opinion_1 == 4 ~ "Difference of 4",
      cand_fullMatched_opinion_1 == 5 ~ "Difference of 5",
      cand_fullMatched_opinion_1 == 6 ~ "Difference of 6"),
    cand_fullMatched_opinion_1 = factor(
      cand_fullMatched_opinion_1, 
      levels = c(
        "Same position", 
        "Difference of 1", 
        "Difference of 2",
        "Difference of 3",
        "Difference of 4",
        "Difference of 5",
        "Difference of 6")),
    cand_fullMatched_opinion_2 = abs(as.numeric(cand_opinion_2) - resp_ref_social_rights),
    cand_fullMatched_opinion_2 = case_when(
      cand_fullMatched_opinion_2 == 0 ~ "Same position",
      cand_fullMatched_opinion_2 == 1 ~ "Difference of 1",
      cand_fullMatched_opinion_2 == 2 ~ "Difference of 2",
      cand_fullMatched_opinion_2 == 3 ~ "Difference of 3",
      cand_fullMatched_opinion_2 == 4 ~ "Difference of 4",
      cand_fullMatched_opinion_2 == 5 ~ "Difference of 5",
      cand_fullMatched_opinion_2 == 6 ~ "Difference of 6"),
    cand_fullMatched_opinion_2 = factor(
      cand_fullMatched_opinion_2, 
      levels = c(
        "Same position", 
        "Difference of 1", 
        "Difference of 2",
        "Difference of 3",
        "Difference of 4",
        "Difference of 5",
        "Difference of 6")),
    cand_fullMatched_opinion_3 = abs(as.numeric(cand_opinion_3) - resp_emi_red_abroad),
    cand_fullMatched_opinion_3 = case_when(
      cand_fullMatched_opinion_3 == 0 ~ "Same position",
      cand_fullMatched_opinion_3 == 1 ~ "Difference of 1",
      cand_fullMatched_opinion_3 == 2 ~ "Difference of 2",
      cand_fullMatched_opinion_3 == 3 ~ "Difference of 3",
      cand_fullMatched_opinion_3 == 4 ~ "Difference of 4",
      cand_fullMatched_opinion_3 == 5 ~ "Difference of 5",
      cand_fullMatched_opinion_3 == 6 ~ "Difference of 6"),
    cand_fullMatched_opinion_3 = factor(
      cand_fullMatched_opinion_3, 
      levels = c(
        "Same position", 
        "Difference of 1", 
        "Difference of 2",
        "Difference of 3",
        "Difference of 4",
        "Difference of 5",
        "Difference of 6"))
  ) 

## Experiment two: Prediciting attitude

## Lining up factor lvls as I want them for the figures
exp_two <-
  exp_two_raw %>%  
  mutate(
    cand_gender = lvls_reorder(factor(cand_gender), c(2, 1)),
    cand_education = lvls_reorder(factor(cand_education), c(1, 2, 4, 3)),
    cand_region = lvls_reorder(factor(cand_region), c(4, 2, 5, 6, 1, 3)),
    cand_religion = lvls_reorder(factor(cand_religion), c(3, 1, 2)),
    cand_work = lvls_reorder(factor(cand_work), c(4, 1, 2, 3, 5, 6)),
    cand_relationship = lvls_reorder(factor(cand_relationship), c(2, 1, 3))
  ) 

## Get labels ------------------------------------------------------

labels_treatment <-
  labels_treatment_raw %>%
  filter(!is.na(treatment))

add_treatment_labels <- . %>%
  mutate(
    value = factor(value, levels = rev(unique(value))),
    treatment = as.character(treatment)
  ) %>%
  left_join(labels_treatment, by = "treatment") %>%
  mutate(treatment_label = factor(treatment_label, levels = unique(labels_treatment$treatment_label[labels_treatment$treatment_label %in% treatment_label]))) %>%
  arrange(treatment_label) %>%
  mutate(
    treatment_label = as.character(treatment_label),
    treatment_label = sapply(treatment_label, str_wrap, width = 20),
    treatment_label = factor(treatment_label, levels = unique(treatment_label))
  )

# Figure 1 -----------------------------------------------------------

observed_1 <-
  exp_two %>%
  mutate(
    resp_gender_2 = factor(resp_gender_2, levels = rev(levels(factor(resp_gender_2)))),
    resp_agree_opinion_1 = case_when(
      resp_agree_opinion_1 == "Agree that the state should reduce income inequality" ~ 1,
      resp_agree_opinion_1 == "Do not agree" ~ 0),
    resp_religion = ifelse(resp_religion=="Other",NA,resp_religion),
    resp_religion = lvls_reorder(factor(resp_religion), c(3, 1, 2)),
    resp_religion = factor(
      resp_religion,
      labels = c("No religion", "Christianity", "Islam")),
    resp_region_2 = lvls_reorder(factor(resp_region_2), c(4, 2, 5, 6, 1, 3)),
    resp_region_2 = factor(
      resp_region_2,
      labels = c(
        "Oslo", "Eastern Norway", "Southern Norway", "Western Norway",
        "Central Norway", "Northern Norway")),
    resp_work = lvls_reorder(factor(resp_work), c(2, 1, 3, 4, 5)),
    resp_work = factor(
      resp_work,
      labels = c(
        "No work experience", "Farmer", "Care worker",
        "IT consultant", "Oil worker")),
    resp_relationship = lvls_reorder(factor(resp_relationship), c(2, 1, 3)),
    resp_relationship = factor(resp_relationship, labels = c("Live alone", "Cohabitant", "Married"))
  ) %>%
  diff_effect(
    resp_agree_opinion_1, resp_age_2, resp_gender_2, resp_education_2, resp_region_2,
    resp_religion, resp_relationship, resp_work
  ) %>%
  mutate(
    exp_attitude_label = "Income inequality\nissue",
    data = "Respondents actual opinions"
  )

observed_2 <-
  exp_two %>%
  mutate(
    resp_gender_2 = factor(
      resp_gender_2,
      levels = rev(levels(factor(resp_gender_2)))),
    resp_agree_opinion_2 = case_when(
      resp_agree_opinion_2 == "Agree that refugees should have the same right to social assistance" ~ 1,
      resp_agree_opinion_2 == "Do not agree" ~ 0),
    resp_religion = ifelse(resp_religion=="Other",NA,resp_religion),
    resp_religion = lvls_reorder(factor(resp_religion), c(3, 1, 2)),
    resp_religion = factor(
      resp_religion,
      labels = c("No religion", "Christianity", "Islam")),
    resp_region_2 = lvls_reorder(factor(resp_region_2), c(4, 2, 5, 6, 1, 3)),
    resp_region_2 = factor(
      resp_region_2,
      labels = c(
        "Oslo", "Eastern Norway", "Southern Norway",
        "Western Norway", "Central Norway", "Northern Norway")),
    resp_work = lvls_reorder(factor(resp_work), c(2, 1, 3, 4, 5)),
    resp_work = factor(
      resp_work,
      labels = c(
        "No work experience", "Farmer", "Care worker",
        "IT consultant", "Oil worker")),
    resp_relationship = lvls_reorder(
      factor(resp_relationship),
      c(2, 1, 3)),
    resp_relationship = factor(
      resp_relationship,
      labels = c("Live alone", "Cohabitant", "Married"))
  ) %>%
  diff_effect(
    resp_agree_opinion_2, resp_age_2, resp_gender_2, resp_education_2,
    resp_region_2, resp_religion, resp_relationship, resp_work
  ) %>%
  mutate(
    exp_attitude_label = "Refugee rights\nissue",
    data = "Respondents actual opinions"
  )

observed_3 <-
  exp_two %>%
  mutate(
    resp_gender_2 = factor(
      resp_gender_2,
      levels = rev(levels(factor(resp_gender_2)))),
    resp_agree_opinion_3 = case_when(
      resp_agree_opinion_3 == "Agree that emission reductions should be done abroad" ~ 1,
      resp_agree_opinion_3 == "Do not agree" ~ 0),
    resp_religion = ifelse(resp_religion=="Other",NA,resp_religion),
    resp_religion = lvls_reorder(factor(resp_religion), c(3, 1, 2)),
    resp_religion = factor(
      resp_religion,
      labels = c("No religion", "Christianity", "Islam")),
    resp_region_2 = lvls_reorder(factor(resp_region_2), c(4, 2, 5, 6, 1, 3)),
    resp_region_2 = factor(
      resp_region_2,
      labels = c(
        "Oslo", "Eastern Norway", "Southern Norway",
        "Western Norway", "Central Norway", "Northern Norway")),
    resp_work = lvls_reorder(factor(resp_work), c(2, 1, 3, 4, 5)),
    resp_work = factor(
      resp_work,
      labels = c(
        "No work experience", "Farmer", "Care worker",
        "IT consultant", "Oil worker")),
    resp_relationship = lvls_reorder(factor(resp_relationship), c(2, 1, 3)),
    resp_relationship = factor(
      resp_relationship,
      labels = c("Live alone", "Cohabitant", "Married"))
  ) %>%
  diff_effect(
    resp_agree_opinion_3, resp_age_2, resp_gender_2, resp_education_2,
    resp_region_2, resp_religion, resp_relationship, resp_work
  ) %>%
  mutate(
    exp_attitude_label = "Emission reduction\nissue",
    data = "Respondents actual opinions"
  )

postscript("output/fig_01.eps", height = 6, width = 8.5)
fig_01 <-
  bind_rows(observed_1, observed_2, observed_3) %>%
  add_treatment_labels() %>%
  mutate(exp_attitude_label = lvls_reorder(exp_attitude_label, c(2, 3, 1))) %>%
  ggplot(aes(x = estimate, y = value)) +
  facet_grid(
    treatment_label ~ exp_attitude_label,
    scales = "free_y",
    space = "free_y") +
  geom_errorbar2() +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  scale_x_continuous(
    limits = c(-.3, .3),
    expand = c(0, 0),
    breaks = seq(-.2, .2, .1)) +
  labs(
    x = "Change in observed Pr(Respondent agrees with statement)",
    y = NULL) +
  theme_descr()
fig_01
dev.off()

## Figure 2 ----------------------------------------------------------

data_02_amce <-
  exp_one %>%
  filter(exp_version %in% c(1, 3)) %>%
  diff_effect(
    post, cand_age, cand_education, cand_gender, cand_region, cand_religion,
    cand_relationship, cand_work,
    subgroup = "exp_version", cluster = "responseid"
  ) %>%
  add_treatment_labels()

data_02_diff <-
  exp_one %>%
  filter(exp_version %in% c(1, 3)) %>%
  diff_diff_effect(
    post, cand_age, cand_education,cand_gender, cand_region, cand_religion,
    cand_relationship, cand_work,
    diff = "exp_version", cluster = "responseid"
  ) %>%
  add_treatment_labels()

data_02 <- bind_rows(data_02_amce, data_02_diff) %>%
  mutate(
    condition = case_when(
      exp_version == 1 ~ "Group treatment",
      exp_version == 3 ~ "Both treatment",
      is.na(exp_version) ~ "Difference"),
    condition = lvls_reorder(factor(condition), c(3, 1, 2))
  )

postscript("output/fig_02.eps", height = 6, width = 8.5)
fig_02 <-
  data_02 %>%
  ggplot(aes(x = estimate, y = value)) +
  facet_grid(
    treatment_label ~ condition,
    scales = "free_y",
    space = "free_y") +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  geom_errorbar2() +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(-.3, .3),
    breaks = seq(-.2, .2, .1)) +
  labs(
    y = NULL,
    x = "Change in predicted Pr(Respondent prefers candidate)") +
  theme_descr()
fig_02
dev.off()

## Figure 3 ----------------------------------------------------------

data_03_amce <-
  exp_one %>%
  filter(exp_version %in% c(2, 3)) %>%
  diff_effect(
    post, cand_opinion_1, cand_opinion_2, cand_opinion_3,
    subgroup = "exp_version",
    cluster = "responseid"
  ) %>%
  add_treatment_labels()

data_03_diff <-
  exp_one %>%
  filter(exp_version %in% c(2, 3)) %>%
  diff_diff_effect(
    post, cand_opinion_1, cand_opinion_2, cand_opinion_3,
    diff = "exp_version", cluster = "responseid"
  ) %>%
  add_treatment_labels()

data_03 <- bind_rows(data_03_amce, data_03_diff) %>%
  mutate(
    condition = case_when(
      exp_version == 2 ~ "Issue treatment",
      exp_version == 3 ~ "Both treatment",
      is.na(exp_version) ~ "Difference"),
    condition = lvls_reorder(factor(condition), c(3, 1, 2))
  )

postscript("output/fig_03.eps", height = 4.5, width = 8.5)
fig_03 <-
  data_03 %>%
  ggplot(aes(x = estimate, y = value)) +
  facet_grid(
    treatment_label ~ condition,
    scales = "free_y",
    space = "free_y") +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  geom_errorbar2() +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(-.3, .3),
    breaks = seq(-.2, .2, .1)) +
  labs(
    y = NULL,
    x = "Change in predicted Pr(Respondent prefers candidate)") +
  theme_descr()
fig_03
dev.off()

## Figure 4 ----------------------------------------------------------

data_04_amce <-
  exp_one %>%
  filter(exp_version %in% c(1, 3)) %>%
  diff_effect(
    post, cand_matched_age, cand_matched_education,
    cand_matched_gender, cand_matched_region, cand_matched_religion,
    cand_matched_relationship, cand_matched_work,
    subgroup = "exp_version",
    cluster = "responseid"
  ) %>%
  add_treatment_labels()

data_04_diff <-
  exp_one %>%
  filter(exp_version %in% c(1, 3)) %>%
  diff_diff_effect(
    post, cand_matched_age, cand_matched_education,
    cand_matched_gender, cand_matched_region, cand_matched_religion,
    cand_matched_relationship, cand_matched_work,
    diff = "exp_version",
    cluster = "responseid"
  ) %>%
  add_treatment_labels()

data_04 <-
  bind_rows(data_04_amce, data_04_diff) %>%
  mutate(
    condition = case_when(
      exp_version == 1 ~ "Group treatment",
      exp_version == 3 ~ "Both treatment",
      is.na(exp_version) ~ "Difference"),
    condition = lvls_reorder(factor(condition), c(3, 1, 2))
  )

postscript("output/fig_04.eps", height = 3.5, width = 8.5)
fig_04 <-
  data_04 %>%
  ggplot(aes(x = estimate, y = value)) +
  facet_grid(
    treatment_label ~ condition,
    scales = "free_y",
    space = "free_y") +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  geom_errorbar2() +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(-.2, .2),
    breaks = seq(-.2, .2, .1)) +
  labs(
    y = "Candidate compared\nto respondent\nattributes",
    x = "Change in predicted Pr(Respondent prefers candidate)") +
  theme_descr()
fig_04
dev.off()

## Figure 5 ----------------------------------------------------------

data_05_amce <-
  exp_one %>%
  filter(exp_version %in% c(2, 3)) %>%
  diff_effect(
    post, cand_matched_opinion_1, cand_matched_opinion_2,
    cand_matched_opinion_3,
    subgroup = "exp_version",
    cluster = "responseid"
  ) %>%
  add_treatment_labels()

data_05_diff <-
  exp_one %>%
  filter(exp_version %in% c(2, 3)) %>%
  diff_diff_effect(
    post, cand_matched_opinion_1, cand_matched_opinion_2,
    cand_matched_opinion_3,
    diff = "exp_version",
    cluster = "responseid"
  )  %>%
  arrange(value) %>%
  add_treatment_labels()

data_05 <-
  bind_rows(data_05_amce, data_05_diff) %>%
  mutate(
    condition = case_when(
      exp_version == 2 ~ "Issue treatment",
      exp_version == 3 ~ "Both treatment",
      is.na(exp_version) ~ "Difference"),
    condition = lvls_reorder(factor(condition), c(3, 1, 2)),
    value = lvls_reorder(factor(value), c(3, 1, 2))
  )

postscript("output/fig_05.eps", height =3.5, width = 8.5)
fig_05 <-
  data_05 %>%
  ggplot(aes(x = estimate, y = value)) +
  facet_grid(
    treatment_label ~ condition,
    scales = "free_y",
    space = "free_y") +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  geom_errorbar2() +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(-.4, .4),
    breaks = seq(-.3, .3, .15)) +
  labs(
    y = NULL,
    x = "Change in predicted Pr(Respondent prefers candidate)") +
  theme_descr()
fig_05
dev.off()

## Figure 6 ----------------------------------------------------------

data_06 <-
  exp_two %>%
  diff_effect(
    post, cand_age, cand_education, cand_gender, cand_region,
    cand_religion, cand_relationship, cand_work,
    subgroup = "exp_attitude_label",
    cluster = "responseid"
  ) %>%
  add_treatment_labels() %>%
  arrange(exp_attitude_label) %>%
  mutate(
    exp_attitude_label = paste0('"', as.character(sapply(exp_attitude_label, str_wrap)), '"'),
    exp_attitude_label = lvls_reorder(factor(exp_attitude_label), c(3, 2, 1)),
    issue = case_when(
      grepl("income", exp_attitude_label) ~ "Income inequality issue",
      grepl("Refugees", exp_attitude_label) ~ "Refugee rights issue",
      grepl("emissions", exp_attitude_label) ~ "Emission reduction issue"),
    issue = lvls_reorder(factor(issue), c(2, 3, 1))
  )

postscript("output/fig_06.eps", height = 6, width = 8.5)
fig_06 <-
  data_06 %>%
  ggplot(aes(x = estimate, y = value)) +
  facet_grid(
    treatment_label ~ issue,
    scales = "free_y",
    space = "free_y") +
  geom_errorbar2() +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  scale_x_continuous(
    limits = c(-.4, .4),
    expand = c(0, 0),
    breaks = seq(-.4, .4, .2)) +
  labs(
    x = "Change in predicted Pr(Respondent thinks that candidate agrees with issue statement)",
    y = NULL) +
  theme_descr()
fig_06
dev.off()

## Figure 5 equivalent with all categories ---------------------------

data_05_full_amce <-
  exp_one %>%
  filter(exp_version %in% c(2, 3)) %>%
  diff_effect(
    post, cand_fullMatched_opinion_1, cand_fullMatched_opinion_2,
    cand_fullMatched_opinion_3,
    subgroup = "exp_version",
    cluster = "responseid"
  ) %>%
  add_treatment_labels()

data_05_full_diff <-
  exp_one %>%
  filter(exp_version %in% c(2, 3)) %>%
  diff_diff_effect(
    post, cand_fullMatched_opinion_1, cand_fullMatched_opinion_2,
    cand_fullMatched_opinion_3,
    diff = "exp_version",
    cluster = "responseid"
  )  %>%
  arrange(value) %>%
  add_treatment_labels()

data_05_full <-
  bind_rows(data_05_full_amce, data_05_full_diff) %>%
  mutate(
    condition = case_when(
      exp_version == 2 ~ "Issue treatment",
      exp_version == 3 ~ "Both treatment",
      is.na(exp_version) ~ "Difference"),
    condition = lvls_reorder(factor(condition), c(3, 1, 2)),
    value = lvls_reorder(factor(value), c(1, 2, 3, 4, 5, 6, 7))
  )

pdf("output/fig_05_full.pdf", height = 3.5, width = 8.5)
fig_05_full <-
  data_05_full %>%
  ggplot(aes(x = estimate, y = value)) +
  facet_grid(
    treatment_label ~ condition,
    scales = "free_y",
    space = "free_y") +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  geom_errorbar2() +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(-.4, .4),
    breaks = seq(-.4, .4, .2)) +
  labs(
    y = NULL,
    x = "Change in predicted Pr(Respondent prefers candidate)") +
  theme_descr()
fig_05_full
dev.off()

## Numbers in text ---------------------------------------------------

exp_one %>%
  filter(!duplicated(responseid)) %>%
  select(resp_religion) %>%
  table()  # 6 muslim respondents

exp_one %>%
  filter(resp_religion == "Islam", post == 1) %>%
  select(cand_religion) %>%
  table() # whom choose equally between the categories

## N RESP in EXP 1
exp_one %>%
  filter(!duplicated(responseid)) %>%
  count()

## N RESP in EXP 2
exp_two %>%
  filter(!duplicated(responseid)) %>%
  count()

