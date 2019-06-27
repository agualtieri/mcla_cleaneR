# MCLA Quality Check scritp
# REACH Yemen - Data Unit
# 23 June 2019 
# V2

# Reset workspace
rm(list=ls())


# Install and load necessary packages

#devtools::install_github("mabafaba/xlsformfill", force = T)
#devtools::install_github("mabafaba/composr", force = T)
#devtools::install_github("mabafaba/cleaninginspectoR", force = T)
#devtools::install_github("mabafaba/reachR2", force = T)
#devtools::install_github("mabafaba/hypegrammaR", force = T)

require("xlsformfill")
require("cleaninginspectoR")
require("tidyverse")
require("readr")
require("reachR")
require("xlsx")
require("composr")
require("reachR2")
require("composr")
require("hypegrammaR")

# Load necessary functions
source("./R/run_checks_from_dataframe.R")
source("./R/batch_issues_check.R")
source("./R/quality_check_log_to_long_format.R")

# Upload kobo tool and fill it with fake data
# Load questionnaire 
#kobo_questions_v1 <- read.csv("data/tool/mcla_questions.csv", stringsAsFactors = F)
#kobo_choices_v1 <- read.csv("data/tool/mcla_choices.csv", stringsAsFactors = F)


questions <- read.csv("data/questionnaire_questions.csv", stringsAsFactors = F)
choices <- read.csv("data/questionnaire_choices.csv", stringsAsFactors = F)

# Generate fake data with `n` rows
fake_dataset <- xlsform_fill(questions, choices, n = 500)
fake_dataset <- fake_dataset[-c(636, 635, 634, 633, 632)]

# Load dataset
#dataset <- read.csv("data/main_dataset_v2.csv", stringsAsFactors = F)

# Test Run
# Load the conditions_list
conditions_list <- read.csv("data/test_issues_sheet.csv", stringsAsFactors = F)



#data_cleaning_log %>% write.csv("test_run.csv") 
#browseURL("test_run.csv")


cleaning_log <- run_checks_from_dataframe(data = fake_dataset,
                                          conditions_dataframe = conditions_list,
                                          condition.column = "conditions",
                                          test.name.column = "check_names",
                                          meta_to_keep = c("uuid", "A1_Metadata", "A2_Metadata", "A3_Metadata"))


cleaning_log_melt <- quality_checks_log_to_long_format(data = cleaning_log,
                                                       meta_not_to_transform = c("uuid", "A1_Metadata", "A2_Metadata", "A3_Metadata"))


