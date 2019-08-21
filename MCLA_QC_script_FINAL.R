# MCLA Quality Check scritp
# REACH Yemen - Data Unit
# 18 August 2019
# V5

# Reset workspace
rm(list=ls())


# Install and load necessary packages
# devtools::install_github("mabafaba/xlsformfill", force = T)
# devtools::install_github("mabafaba/composr", force = T)
# devtools::install_github("mabafaba/cleaninginspectoR", force = T)
# devtools::install_github("mabafaba/reachR2", force = T)
# devtools::install_github("mabafaba/hypegrammaR", force = T)
# devtools::install_github("agualtieri/dataqualitycontrol", force = T)

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
library("dataqualitycontrol")

# Upload kobo tool and fill it with fake data
# Load questionnaire 
questions <- read.csv("data/questionnaire_questions.csv", stringsAsFactors = F)
choices <- read.csv("data/questionnaire_choices.csv", stringsAsFactors = F)

# Generate fake data with `n` rows
fake_dataset <- xlsform_fill(questions, choices, n = 500)
fake_dataset <- fake_dataset[-c(636, 635, 634, 633, 632)]

# Load dataset
# --- TBD ----

### Test Run
# Load the conditions_list, produce the cleaning log and melt it into a readable form
conditions_list <- read.csv("data/test_issues_sheet_v2.csv", stringsAsFactors = F)


cleaning_log <- run_checks_from_dataframe(df = fake_dataset,
                                          conditions_df = conditions_list,
                                          condition.column = "conditions",
                                          test.name.column = "check_names",
                                          meta_to_keep = c("uuid", "A1_Metadata", "A2_Metadata", "A3_Metadata"))

write.csv(cleaning_log, "./output/cleaning_log.csv", row.names = F)
browseURL("./output/cleaning_log.csv")

cleaning_log_melt <- quality_checks_log_to_long_format(data = cleaning_log,
                                                       meta_not_to_transform = c("uuid", "A1_Metadata", "A2_Metadata", "A3_Metadata"))

#write.csv(cleaning_log_melt, "./output/melted_clog.csv", row.names = F)
#browseURL("./output/melted_clog.csv")


### Split the melted cleaning log into a one row per variable format
## Add quality checks to dataframe
cleaning_log_melt$quality_checks <- conditions_list$conditions[match(cleaning_log_melt$variable, conditions_list$check_names)]

## Rename column variable and delete the value column
clog_separated <- reshape::rename(cleaning_log_melt, c(variable = "description"))
clog_separated$value <- NULL

#write.csv(clog_separated, "./output/clog_separated.csv", row.names = F)
#browseURL("./output/clog_separated.csv")

## Separate reformatted quality checks into three variable to allow for easier data cleaning
clog_reformatted <- separate_on_multiple(clog_separated, "quality_checks", sep1 = "&", sep2 = "|")

clog_test <- separate_on_multiple(clog_reformatted, "quality_checks", sep1 = "=")


?separate_on_multiple




clog_reformatted$qchecks_sep <- as_tibble(str_replace_all(clog_reformatted$quality_checks, "[= | !=]", " "))
var_split <- str_split_fixed(clog_reformatted$qchecks_sep, " ", 2)

cleaning_log_final <- cbind(clog_reformatted, var_split)
cleaning_log_final <- reshape::rename(cleaning_log_final, c(V1 = "variable_name", V2 = "old_value"))

cleaning_log_final$new_value <- NA

cleaning_log_final$quality_checks <- NULL
cleaning_log_final$qchecks_sep <- NULL

cleaning_log_final$old_value<- gsub("\"", "", cleaning_log_final$old_value)
cleaning_log_final$old_value <- gsub("\ ", "", cleaning_log_final$old_value)

write.csv(cleaning_log_final, "./output/cleaning_log_final_codes.csv", row.names = F)
browseURL("./output/cleaning_log_final_codes.csv")

# Replace variable names to align them with paper form
cleaning_log_final$variable_name <- questions$label..English[match(cleaning_log_final$variable_name, questions$name)]

# Replace options codes to align them with paper form
cleaning_log_final$old_value <- choices$label..English[match(cleaning_log_final$old_value, choices$name)]

write.csv(cleaning_log_final, "./output/cleaning_log_final_desc.csv", row.names = F)
browseURL("./output/cleaning_log_final_desc.csv")











