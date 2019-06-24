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
source("./code/basic/run_checks_from_dataframe.R")
source("./code/basic/batch_issues_check.R") # this function is stored inside the previous one

# Upload kobo tool and fill it with fake data
# Load questionnaire
kobo_questions_v1 <- read.csv("data/tool/mcla_questions.csv", stringsAsFactors = F)
kobo_choices_v1 <- read.csv("data/tool/mcla_choices.csv", stringsAsFactors = F)

# Generate fake data with `n` rows
fake_dataset <- xlsform_fill(kobo_questions_v1, kobo_choices_v1, n = 500)
fake_dataset <- fake_dataset[-c(709, 710, 711, 712, 713)]

# Test Run
# Load the conditions_list
conditions_list <- read.csv("data/mcla_issues_sheet.csv", stringsAsFactors = F)

data_cleaning_log <- run_checks_from_dataframe(data = fake_dataset,
                                               conditions_dataframe = conditions_list,
                                               condition.column = "conditions",
                                               test.name.column = "check_names",
                                               meta_to_keep = c("uuid", "A7_Metadata", "A6_Metadata", "A1_Metadata", "A2_Metadata", "A3_Metadata", "A4_Metadata", "A5_Metadata"))


data_cleaning_log %>% write.csv("test_run.csv") 
browseURL("test_run.csv")


