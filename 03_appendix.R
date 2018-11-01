## Mikael Poul Johannesson
## August 2017

## Start Matter ------------------------------------------------------

library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)

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
  ## Lining up factor lvls as I want them for the figures
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
    cand_fullMatched_opinion_1 = factor(cand_fullMatched_opinion_1, 
                                        levels = c("Same position", 
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
    cand_fullMatched_opinion_2 = factor(cand_fullMatched_opinion_2, 
                                        levels = c("Same position", 
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
    cand_fullMatched_opinion_3 = factor(cand_fullMatched_opinion_3, 
                                        levels = c("Same position", 
                                                   "Difference of 1", 
                                                   "Difference of 2",
                                                   "Difference of 3",
                                                   "Difference of 4",
                                                   "Difference of 5",
                                                   "Difference of 6"))) 

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

## TABLE about respondents, exp one ----------------------------------

exp_one %>%
  filter(!duplicated(responseid)) %>%
  select(resp_age_2, resp_region_2, resp_religion, resp_work,
         resp_relationship, resp_education_2, resp_gender_2,
         resp_ref_social_rights, resp_emi_red_abroad, resp_reduce_ineq) %>%
  gather(variable, value, na.rm = TRUE) %>%
  group_by(variable) %>%
  mutate(n_var = n()) %>%
  ungroup() %>%
  group_by(variable, value) %>%
  summarize(n = n(),
            prop = n / n_var[1],
            prop = round(prop, 2) * 100) %>%
  ungroup() %>%
  mutate(
    variable = case_when(
      variable == "resp_age_2" ~ "Age",
      variable == "resp_education_2" ~ "Education",
      variable == "resp_emi_red_abroad" ~ "Emission reduction issue",
      variable == "resp_gender_2" ~ "Gender",
      variable == "resp_reduce_ineq" ~ "Income inequality issue",
      variable == "resp_ref_social_rights" ~ "Refugee rights issue",
      variable == "resp_region_2" ~ "Region",
      variable == "resp_relationship" ~ "Marital Status",
      variable == "resp_religion" ~ "Religious affiliation",
      variable == "resp_work" ~ "Professional field"
    ),
    variable = factor(variable),
    variable = lvls_reorder(variable, c(1, 2, 4, 7, 6, 9, 10, 3, 5, 8))) %>%
  arrange(variable) %>%
  rename_(
    "Variable" = "variable",
    "Value" = "value",
    "N" = "n",
    "%" = "prop"
  ) %>%
  knitr::kable(format = "latex", booktabs = TRUE)

## TABLE about respondents, exp two ----------------------------------

exp_two %>%
  filter(!duplicated(responseid)) %>%
  select(resp_age_2, resp_region_2, resp_religion, resp_work,
         resp_relationship, resp_education_2, resp_gender_2,
         resp_ref_social_rights, resp_emi_red_abroad, resp_reduce_ineq) %>%
  gather(variable, value, na.rm = TRUE) %>%
  group_by(variable) %>%
  mutate(n_var = n()) %>%
  ungroup() %>%
  group_by(variable, value) %>%
  summarize(n = n(),
            prop = n / n_var[1],
            prop = round(prop, 2) * 100) %>%
  ungroup() %>%
  mutate(
    variable = case_when(
      variable == "resp_age_2" ~ "Age",
      variable == "resp_education_2" ~ "Education",
      variable == "resp_emi_red_abroad" ~ "Emission reduction issue",
      variable == "resp_gender_2" ~ "Gender",
      variable == "resp_reduce_ineq" ~ "Income inequality issue",
      variable == "resp_ref_social_rights" ~ "Refugee rights issue",
      variable == "resp_region_2" ~ "Region",
      variable == "resp_relationship" ~ "Marital Status",
      variable == "resp_religion" ~ "Religious affiliation",
      variable == "resp_work" ~ "Professional field"
    ),
    variable = factor(variable),
    variable = lvls_reorder(variable, c(1, 2, 4, 7, 6, 9, 10, 3, 5, 8))) %>%
  arrange(variable) %>%
  rename_(
    "Variable" = "variable",
    "Value" = "value",
    "N" = "n",
    "%" = "prop"
  ) %>%
  knitr::kable(format = "latex", booktabs = TRUE)


## TBL: Treatment Assignement, Experiment 1 --------------------------

exp_one %>%
  select(cand_age, cand_education, cand_gender, cand_region, cand_relationship, cand_religion, cand_work, cand_opinion_1, cand_opinion_2, cand_opinion_3,  exp_version_label) %>%
  gather(variable, value, matches("cand_"), na.rm = TRUE) %>%
  group_by(exp_version_label, variable) %>%
  mutate(n_var = n()) %>%
  ungroup() %>%
  group_by(exp_version_label, variable, value) %>%
  summarize(n = n(),
            prop = n / n_var[1],
            prop = round(prop, 2) * 100) %>%
  ungroup() %>%
  mutate(
    n = paste0(n, " (.", prop, ")"),
    exp_version_label = ifelse(exp_version_label == "Both", "both", "only"),
    variable = case_when(
      variable == "cand_age" ~ "Age",
      variable == "cand_education" ~ "Education",
      variable == "cand_opinion_3" ~ "Emission reduction issue",
      variable == "cand_gender" ~ "Gender",
      variable == "cand_opinion_1" ~ "Income inequality issue",
      variable == "cand_opinion_2" ~ "Refugee rights issue",
      variable == "cand_region" ~ "Region",
      variable == "cand_relationship" ~ "Marital Status",
      variable == "cand_religion" ~ "Religious affiliation",
      variable == "cand_work" ~ "Occupation"
    ),
    variable = factor(variable),
    variable = lvls_reorder(variable, c(1, 4, 2, 6, 7, 9, 10, 3, 5, 8)))  %>%
  select(-prop) %>%
  spread(exp_version_label, n) %>%
  ## mutate(both = ifelse(is.na(both), "0 (0)", both),
  ##        only = ifelse(is.na(only), "0 (0)", only)) %>%
  rename(
    "Treatment" = "variable",
    "Value" = "value",
    "Both" = "both",
    "Group or issue only" = "only"
  ) %>%
  knitr::kable(format = "latex", booktabs = TRUE)
#
## TBL: Treatment Assignement, Experiment 2 -----------------------------------------------------------
#
exp_two %>%
  select(cand_age, cand_education, cand_gender, cand_region, cand_relationship, cand_religion, cand_work, cand_opinion_1, cand_opinion_2, cand_opinion_3,  exp_version_label) %>%
  gather(variable, value, - exp_version_label, na.rm = TRUE) %>%
  group_by(exp_version_label, variable) %>%
  mutate(n_var = n()) %>%
  ungroup() %>%
  group_by(exp_version_label, variable, value) %>%
  summarize(n = n(),
            prop = n / n_var[1],
            prop = round(prop, 2) * 100) %>%
  ungroup() %>%
  mutate(
    variable = case_when(
      variable == "cand_age" ~ "Age",
      variable == "cand_education" ~ "Education",
      variable == "cand_opinion_3" ~ "Emission reduction issue",
      variable == "cand_gender" ~ "Gender",
      variable == "cand_opinion_1" ~ "Income inequality issue",
      variable == "cand_opinion_2" ~ "Refugee rights issue",
      variable == "cand_region" ~ "Region",
      variable == "cand_relationship" ~ "Marital Status",
      variable == "cand_religion" ~ "Religious affiliation",
      variable == "cand_work" ~ "Occupation"
    ),
    variable = factor(variable),
    variable = lvls_reorder(variable, c(1, 4, 2, 6, 7, 9, 10, 3, 5, 8)))  %>%
  arrange(variable) %>%
  rename_(
    "Treatment" = "variable",
    "Value" = "value",
    "N" = "n",
    "%" = "prop"
  ) %>%
  knitr::kable(format = "latex", booktabs = TRUE)
#
## ------------------------------------------------------------------------------------------------
#
## Figure with effect of "politician" vs "person" ---------------------
#
## Figure a5
fig_a5_data <- exp_two %>%
  diff_diff_effect(post, cand_age, cand_education,
                   cand_gender, cand_region, cand_religion,
                   cand_relationship, cand_work,
                   subgroup = "exp_attitude_label",
                   diff = "exp_type",
                   cluster = "responseid") %>%
  add_treatment_labels() %>%
  arrange(exp_attitude_label) %>%
  mutate(exp_attitude_label = paste0('"', as.character(sapply(exp_attitude_label, str_wrap)), '"'),
         exp_attitude_label = lvls_reorder(factor(exp_attitude_label), c(3, 2, 1)),
         issue = case_when(
           grepl("income", exp_attitude_label) ~ "Income inequality issue",
           grepl("Refugees", exp_attitude_label) ~ "Refugee rights issue",
           grepl("emissions", exp_attitude_label) ~ "Emission reduction issue"),
         issue = lvls_reorder(factor(issue), c(2, 3, 1)))
#
pdf("output/fig_a5.pdf", height=4, width = 8.5)
fig_a5 <- fig_a5_data %>%
  ggplot(aes(x = estimate, y = value)) +
  facet_grid(treatment_label ~ issue,
             scales = "free_y",
             space = "free_y") +
  geom_errorbar2() +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  scale_x_continuous(limits = c(-.4, .4),
                     expand = c(0, 0),
                     breaks = seq(-.4, .4, .2)) +
  labs(x = "Difference in change in predicted Pr(Respondent thinks that candidate agrees with issue statement) vs other person",
       y = NULL) +
  theme_descr()
fig_a5
dev.off()
#
#
# ------------------------------------------------
# Figure C.3
# Compare respondents effect marker on preferences vs effect of candidates' marker on preference (Figure 4)
fig4_data <- exp_two %>%
  diff_effect(post, cand_age, cand_education,
              cand_gender, cand_region, cand_religion,
              #cand_relationship, cand_work,
              subgroup = "exp_attitude_label",
              cluster = "responseid") %>%
  add_treatment_labels() %>%
  arrange(exp_attitude_label) %>%
  mutate(exp_attitude_label = paste0('"', as.character(sapply(exp_attitude_label, str_wrap)), '"'),
         exp_attitude_label = lvls_reorder(factor(exp_attitude_label), c(3, 2, 1)),
         issue = case_when(
           grepl("income", exp_attitude_label)    ~ "Income inequality\nissue",
           grepl("Refugees", exp_attitude_label)  ~ "Refugee rights\nissue",
           grepl("emissions", exp_attitude_label) ~ "Emission reduction\nissue"),
         issue = lvls_reorder(factor(issue), c(2, 3, 1)))
#
pdf("output/fig_a4.pdf", height=4, width = 8.5)
fig4_data %>%
  ggplot(aes(x = estimate, y = value)) +
  facet_grid(treatment_label ~ issue,
             scales = "free_y",
             space = "free_y") +
  geom_errorbar2() +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  scale_x_continuous(limits = c(-.3, .3),
                     expand = c(0, 0),
                     breaks = seq(-.3, .3, .15)) +
  labs(x = "Difference in change in predicted Pr(Respondent thinks that candidate agrees with issue statement) vs other person",
       y = NULL) +
  theme_descr()
dev.off()


## TBL: Balance tests, Experiment 1 and 2 ----------------------------

attach(exp_one)
binary <- ifelse(exp_version_label=="Only substantive information", NA, as.numeric(exp_version_label))
listCategorial <- c("resp_education", "resp_gender", "resp_region", 'resp_religion', 'resp_relationship', 
                    "resp_work")
lapply(listCategorial, function(var) {
  variable <- get(var)
  table <- table(variable, binary)
  fi.test <- fisher.test(table, simulate.p.value = TRUE, B = 1e5)
  print(var) 
  print(paste("Fisher: ", fi.test$p.value))
})

t.test(resp_age~binary)
wilcox.test(resp_age,binary, exact=FALSE, correct=FALSE)

attach(exp_two)
listCategorial <- c("resp_agree_opinion_1", "resp_agree_opinion_2", "resp_agree_opinion_3")
lapply(listCategorial, function(var) {
  variable <- get(var)
  category <- exp_attitude_label
  table <- table(variable, category)
  test <- fisher.test(table, simulate.p.value = TRUE, B = 1e5)
  print(var) 
  print(test$p.value)
})
