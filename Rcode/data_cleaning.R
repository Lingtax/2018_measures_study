#devtools::install_github("lingtax/Qualtrics")

library(Qualtrics) # Internal lab package 
library(tidyverse)
library(snakecase)
library(here) # for file management
library(janitor)

# Read in data ------------------------------------------------------------
path <- here("data", "2018measures_data.csv")

df <-  read_qualtrics(path)

meta <- read_csv(here("data", "2018measures_metadata.csv")) %>%
  mutate(VariableID = to_snake_case(VariableID, parsing_option = 4))

df <- meta_rename(df, meta, VariableID, VariableLabel) #apply cogent names

# Clean and reformat data.
df <- df %>% 
  mutate(age = case_when(
    age == 1981 ~ 37L,
    age == 1994 ~ 24L, 
    TRUE ~ age),
    gender = as.factor(case_when(
      gender == 1 ~ "male",
      gender == 2 ~ "female",
      gender == 3 ~ "not listed",
      gender == 4 ~ "prefer not to answer")),
   education_cat = case_when(
      education ==  1 ~ "Less than High School",
      education ==  2 ~ "High School graduate",
      education ==  3 ~ "Technical school graduate",
      education ==  4 ~ "Associate degree",
      education ==  5 ~ "Bachelors degree",
      education ==  6 ~ "Masters degree",
      education ==  7 ~ "Doctorate")
    ) 

write_csv(df, here("data", "2018measures_data_clean.csv"))
