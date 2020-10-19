#### Preamble ####
# Purpose: The purpose of this data is to analyze an aspect of the 2017 GSS
# data. A tidy version of the 2017 GSS data was taken. The variable of interest
# in this analysis is marriage status. More specifically we investigate 
# predictors of a longer term committed relationship. This code therefore 
# reduces the data set to include only variables of interest, creates a model
# investigating the variable of interests and creates graphs to visualize 
# various relationships.
# Authors: Jasmine Carlos, Matthew Caringi, Haeun Choi, Mahmoud Elsheikh
# Date: 17 October 2020
# Contact: 
# Pre-requisites: Need to have downloaded the csv titled "gss.csv". The data set 
# is available in the inputs folder. The data set is a cleaned version of the
# 2017 general social survey. 


#### Workspace set-up ####

# Load the packages

library(tidyverse)
library(readr)
library(dplyr)
library(knitr)
library(ggplot2)
library(kableExtra)

# The following reads in the cleaned 2017 general social survey (GSS) data which
# is the data that will be used for the following analysis.

clean_gss_data <- read.csv("/cloud/project/inputs/gss.csv")

#### Clean Data ####

# The following eliminates the NA values for variables that are being grouped

data_no_nas <- 
  clean_gss_data %>%
  filter(!marital_status == 'NA') %>%
  filter(!education == 'NA')

# The following builds the data of interest. Specifically we are creating new
# groupings for the variable of education and marital status

grouped_data <-
  data_no_nas %>%
  mutate(
    education_group = case_when(
      education == "High school diploma or a high school equivalency certificate"
      ~ "Highschool diploma or less",
      education == "Less than high school diploma or its equivalent" ~ 
        "Highschool diploma or less",
      education == "Trade certificate or diploma" ~ "Trade certificate or 
      diploma",
      education == "College, CEGEP or other non-university certificate or diploma"
      ~ "Post-Secondary school certifcate or diploma",
      education == "University certificate or diploma below the bachelor's level" 
      ~ "Post-Secondary school certifcate or diploma",
      education == "University certificate, diploma or degree above the bachelor's level"
      ~ "Post-Secondary school certifcate or diploma",
      education == "Bachelor's degree (e.g. B.A., B.Sc., LL.B.)" ~ 
        "Post-Secondary school certifcate or diploma"
    )
  ) %>%
  mutate(
    marital_group = case_when(
      marital_status == "Married" ~ "Committed Relationship",
      marital_status == "Living common-law" ~ "Committed Relationship",
      marital_status == "Divorced" ~ "Non-Committed Relationship",
      marital_status == "Single, never married"~ "Non-Committed Relationship",
      marital_status == "Separated"~ "Non-Committed Relationship",
      marital_status == "Widowed"~ "Committed Relationship",
      TRUE ~ "NA")
  ) 


# The following reduces the data set to include only the variables of interest

reduced_data <-
  grouped_data %>%
  select(marital_group, education_group, total_children, age_first_child, 
         income_family)


#### Make Model ####

# The following uses the glm function to create a standard logistic regression
# model. The dependent variable is marital group while the predictor variables
# used are education group, total number of children, age of respondent's first
# child and income of the family


my_model <- glm(as.factor(marital_group) ~ as.factor(education_group) + 
                  total_children + age_first_child + 
                  as.factor(income_family),
                data = reduced_data, family = "binomial")

summary(my_model)


#### Make Graphs ####

# The following is a graph that visualizes the distribution of relationship
# status over the education groups. Note that we filtered out the NA values 
# for each variable so there is no bar for NA values

rel_ed_graph <- 
  reduced_data %>%
  filter(!education_group == "NA") %>%
  filter(!marital_group == "NA") %>%
  ggplot( aes(x = education_group, fill = marital_group)) + 
  geom_bar(position = "dodge") +
  geom_text(stat="count", aes(label=..count..), position=position_dodge(width=1),
            vjust=-1) +
  theme(axis.text.x  = element_text(angle=45, hjust = 1)) +
  ylim(c(0, 6000)) +
  labs(
    title = "Figure 1: Distribution of Relationship Status 
    over Education Groups",
    x = "Education Group",
    y = "Number of Respondents",
    fill = "Relationship Status",
    caption = "Source = 2017 General Social Survey")

rel_ed_graph

# The following is a graph that visualizes the distribution of relationship
# status over the total number of children a respondent has. Note that we 
# filtered out the NA values for each variable so there is no bar for NA values

rel_total_children_graph <- 
  reduced_data %>%
  filter(!total_children == "NA") %>%
  filter(!marital_group == "NA") %>%
  ggplot( aes(x = total_children, fill = marital_group)) + 
  geom_bar(position = "dodge") +
  geom_text(stat="count", aes(label=..count..), position=position_dodge(width=1),
            vjust=-1) +
  ylim(c(0, 5200)) +
  labs(
    title = "Figure 2: Distribution of 
    Relationship Status over Total Number of Children",
    x = "Total Number of Children",
    y = "Number of Respondents",
    fill = "Relationship Status",
    caption = "Source = 2017 General Social Survey")

rel_total_children_graph

# The following is a graph that visualizes the distribution of relationship
# status over the age of the first child of a respondent. Note that we 
# filtered out the NA values for each variable so there is no bar for NA values.
# For this graph we also created age groups for a cleaner visual.

rel_age_child_graph <- 
  reduced_data %>%
  filter(!age_first_child == "NA") %>%
  filter(!marital_group == "NA") %>%
  mutate(as.numeric(age_first_child)) %>%
  mutate(
    age_first_child_group = case_when(
      age_first_child>=0 & age_first_child<=9  ~ "0-9",
      age_first_child>=10 & age_first_child<=19   ~ "10-19",
      age_first_child>=20 & age_first_child<=29   ~ "20-29",
      age_first_child>=30 & age_first_child<=39   ~ "30-39",
      age_first_child>=40 & age_first_child<=49   ~ "40-49",
      age_first_child>=50 & age_first_child<=59   ~ "50-59",
      age_first_child>=60 & age_first_child<=69   ~ "60-69")
  ) %>%
  ggplot( aes(x = age_first_child_group, fill = marital_group)) + 
  geom_bar(position = "dodge") +
  geom_text(stat="count", aes(label=..count..), position=position_dodge(width=1),
            vjust=-1) +
  ylim(c(0, 2200)) +
  labs(
    title = "Figure 3: Distribution of 
    Relationship Status Over Age of First Child",
    x = "Age of First Child",
    y = "Number of Respondents",
    fill = "Relationship Status",
    caption = "Source = 2017 General Social Survey")

rel_age_child_graph

# The following is a graph that visualizes the distribution of relationship
# status over the income of a respondent's family. Note that we filtered out the
# NA values for each variable so there is no bar for NA values. For this graph 
# we also ensured that the income groups appeared in ascending order along the
# x-axis.

rel_income_graph <- 
  reduced_data %>%
  filter(!income_family == "NA") %>%
  filter(!marital_group == "NA") %>%
  mutate(income_family_group = fct_relevel(income_family, "Less than $25,000", 
                                           "$25,000 to $49,999", 
                                           "$50,000 to $74,999", 
                                           "$75,000 to $99,999", 
                                           "$100,000 to $ 124,999",
                                           "$125,000 and more")) %>%
  ggplot( aes(x = income_family_group, fill = marital_group)) + 
  geom_bar(position = "dodge") +
  geom_text(stat="count", aes(label=..count..), position=position_dodge(width=1),
            vjust=-1) +
  theme(axis.text.x  = element_text(angle=45, hjust = 1)) +
  ylim(c(0, 4100)) +
  labs(
    title = "Figure 4: Distribution of 
    Relationship Status Over Income of Family",
    x = "Income of Family",
    y = "Number of Respondents",
    fill = "Relationship Status",
    caption = "Source = 2017 General Social Survey")

rel_income_graph

# The following formats the summary statistics of the model into a table


my_model_summary <-
  broom::tidy(my_model)

table_summary <- my_model_summary %>%
  kbl(caption = 'Summary of Model Statistics',  digits=4) %>%
  kable_classic_2() 

table_summary