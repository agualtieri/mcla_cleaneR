# MCLA Quality Check scritp
# REACH Yemen - Data Unit
# 10 July 2019 
# V3

# Reset workspace
rm(list=ls())


# Install and load necessary packages
# devtools::install_github("mabafaba/xlsformfill", force = T)
# devtools::install_github("mabafaba/composr", force = T)
# devtools::install_github("mabafaba/cleaninginspectoR", force = T)
# devtools::install_github("mabafaba/reachR2", force = T)
# devtools::install_github("mabafaba/hypegrammaR", force = T)

require("xlsformfill")
require("cleaninginspectoR")
require("tidyverse")
require("readr")
require("composr")
require("hypegrammaR")
library("stringr")
library("koboquest")
library("splitstackshape")
library("reshape")

# Load necessary functions
source("./R/run_checks_from_dataframe.R")
source("./R/batch_issues_check.R")
source("./R/quality_check_log_to_long_format.R")

# Upload kobo tool and fill it with fake data
# Load questionnaire 
questions <- read.csv("data/questionnaire_questions.csv", stringsAsFactors = F)
choices <- read.csv("data/questionnaire_choices.csv", stringsAsFactors = F)

# Generate fake data with `n` rows
fake_dataset <- xlsform_fill(questions, choices, n = 500)
fake_dataset <- fake_dataset[-c(636, 635, 634, 633, 632)]

# Load dataset
#dataset <- read.csv("data/main_dataset_v2.csv", stringsAsFactors = F)

# Test Run
# Load the conditions_list
conditions_list <- read.csv("data/test_issues_sheet_v2.csv", stringsAsFactors = F)


#data_cleaning_log %>% write.csv("test_run.csv") 
#browseURL("test_run.csv")

cleaning_log <- run_checks_from_dataframe(data = fake_dataset,
                                          conditions_dataframe = conditions_list,
                                          condition.column = "conditions",
                                          test.name.column = "check_names",
                                          meta_to_keep = c("uuid", "A1_Metadata", "A2_Metadata", "A3_Metadata"))


cleaning_log_melt <- quality_checks_log_to_long_format(data = cleaning_log,
                                                       meta_not_to_transform = c("uuid", "A1_Metadata", "A2_Metadata", "A3_Metadata"))


### Split aggregated transformed cleaning log into a one-row-per-variable

## Add quality checks to dataframe
cleaning_log_melt$quality_checks <- conditions_list$conditions[match(cleaning_log_melt$variable, conditions_list$check_names)]

## Using the splitstackshape package split the rows - unfortunately it allows for only one separator at a time

clog_separated <- cSplit(cleaning_log_melt, "quality_checks", sep = "&", "long")

clog_separated <- cSplit(clog_separated, "quality_checks", sep = "|", "long")

## Rename column variable
clog_separated <- reshape::rename(clog_separated, c(variable = "description"))

## Next steps:
# - divide the quality_checks column into two column "variable" and "old value"
# - if possible, using koboquest rename all variables and old values using the questinnaire labels to make data cleaning easier for paper forms
# - add one last column named "new value"






