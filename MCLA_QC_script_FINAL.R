# MCLA Quality Check script
# REACH Yemen - Data Unit
# In case of issues contact: alberto.gualtieri@reach-initiative.org
# 17 September 2019
# V7


# Reset workspace
rm(list=ls())

# Install and load necessary packages
# devtools::install_github("mabafaba/xlsformfill", force = T, build_vignettes = T)
# devtools::install_github("mabafaba/composr", force = T, build_vignettes = T)
# devtools::install_github("mabafaba/cleaninginspectoR", force = T, build_vignettes = T)
# devtools::install_github("mabafaba/reachR2", force = T, build_vignettes = T)
# devtools::install_github("mabafaba/hypegrammaR", force = T, build_vignettes = T)
# devtools::install_github("agualtieri/dataqualitycontrol", force = T, build_vignettes = T)

require("xlsformfill")
require("cleaninginspectoR")
require("tidyverse")
require("readr")
require("composr")
require("hypegrammaR")
library("koboquest")
library("reshape")
library("dataqualitycontrol")


# Set data for outputs
today <- Sys.Date()

# Upload kobo tool and fill it with fake data
# Load questionnaire 
questions <- read.csv("data/questions.csv", stringsAsFactors = F, check.names = F)
choices <- read.csv("data/choices.csv", stringsAsFactors = F, check.names = F)

# normalise and remove empty columns
questions$name<-tolower(questions$name)
questions$relevant<-tolower(questions$relevant)
questions$calculation<-tolower(questions$calculation)
questions$type<-tolower(questions$type)

choices <- choices[, colnames(choices)!=""]

# Generate fake data with `n` rows
response <- xlsform_fill(questions, choices, n = 500)
response <- response %>% select(-c("start", "end", "deviceid", "formname" ))

# Load dataset
#response <- read.csv("data/test_dataset.csv", stringsAsFactors = F)



## Use cleaninginspector to run basic duplicates and outliers checks
general_issues <- inspect_all(response)
write.csv(general_issues, paste0("./output/MCLA_issues_sheet_",today,".csv"), row.names = F)

# Check that district matches the population
## Add "YE" to locations
response$A2_Metadata <- paste0("YE", response$A2_Metadata)
response$A3_Metadata <- paste0("YE", response$A3_Metadata)
response$A4_Metadata <- paste0("YE", response$A4_Metadata)
response$A5_Metadata <- paste0("YE", response$A5_Metadata)

## Load sampling frame
sf_district <- read.csv("data/sf_district.csv", stringsAsFactors = F)
sf_governorate <- read.csv("data/sf_governorate.csv", stringsAsFactors = F)


## Check inconsistencies between dataset and sampling frame
### Response
response <- response %>%
  new_recoding(data_stratum_id, source = a1_metadata) %>%
  recode_to("nondisplaced",where.selected.any = "a1_1") %>% 
  recode_to("idps",where.selected.any = "a1_2") %>% 
  recode_to("returnees",where.selected.any = "a1_3") %>% 
  recode_to("refugees",where.selected.any = "a1_4") %>%
  recode_to("migrants",where.selected.any = "a1_5") %>% 
  end_recoding()

# response <- mutate(response, weight_dis_id = paste0(A3_Metadata, data_stratum_id)) # creates the district pop id
response <- mutate(response, weight_gov_id = paste0(a2_metadata, data_stratum_id)) # creates the governorate pop id

### District sampling frame
sf_district <- gather(sf_district, key = "pop_group", value = "pop", NonDisplaced, IDPs, Returnees, Refugees, Migrants) # transpose and gathers dataset
sf_district <- mutate(sf_district, weight_dis_id = paste0(DistrictPcode, pop_group)) # adds matching vector
sf_district <- sf_district[apply(sf_district["pop"], 1, function(z) !any(z==0)),] # deletes rows with 0s
  
### Governorate sampling frame
sf_governorate <- gather(sf_governorate, key = "pop_group", value = "pop", nondisplaced, idps, returnees, refugees, migrants)
sf_governorate <- mutate(sf_governorate, weight_gov_id = paste0(governorate_code, pop_group))
sf_governorate <- sf_governorate[apply(sf_governorate["pop"], 1, function(z) !any(z==0)),]
sf_governorate <- sf_governorate[complete.cases(sf_governorate),]

## Create warning lists: this tells you how many records are matching it should be always 100% !!!!
### District
cat(crayon::red(mean(response$weight_dis_id %in% sf_district$weight_dis_id) %>% multiply_by(100) %>% round(2) %>% paste0("% of records matched in samplingframe")))

district_outliers <- response %>% subset(!(response$weight_dis_id %in% sf_district$weight_dis_id)) # dataset showing the outliers
write.csv(district_outliers, paste0("./output/district_outliers_",today,".csv"), row.names = F)
browseURL(paste0("./output/district_outliers_",today,".csv"))

#### Filtered response
response_tidy <- response %>% filter(response$weight_dis_id %in% sf_district$weight_dis_id)

#### Create coverage list - district level
response_tidy <- add_count(response_tidy, weight_dis_id, name = "district_count")

district_coverage <- sf_district

district_coverage$response <- response_tidy$district_count[match(sf_district$weight_dis_id, response_tidy$weight_dis_id)]

district_coverage$weight_dis_id <- NULL

district_coverage <- district_coverage %>% mutate(coverage_percent = round((response/pop*100), 2))

write.csv(district_coverage, paste0("./output/district_coverage_",today,".csv"), row.names = F)
browseURL(paste0("./output/district_coverage_",today,".csv"))


### Governorate
cat(crayon::red(mean(response$weight_gov_id %in% sf_governorate$weight_gov_id) %>% multiply_by(100) %>% round(2) %>% paste0("% of records matched in samplingframe")))

governorate_outliers <- response %>% subset(!(response$weight_gov_id %in% sf_governorate$weight_gov_id))
write.csv(governorate_outliers, paste0("./output/governorate_outliers_",today,".csv"), row.names = F)
browseURL(paste0("./output/governorate_outliers_",today,".csv"))

#### Filtered response
response_tidy <- response %>% filter(response$weight_gov_id %in% sf_governorate$weight_gov_id)

#### Create coverage list - district level
response_tidy <- add_count(response_tidy, weight_gov_id, name = "governorate_count")

governorate_coverage <- sf_governorate

governorate_coverage$response <- response_tidy$governorate_count[match(sf_governorate$weight_gov_id, response_tidy$weight_gov_id)]

governorate_coverage$weight_gov_id <- NULL

governorate_coverage <- governorate_coverage %>% mutate(coverage_percent = round((response/pop*100), 2))

write.csv(governorate_coverage, paste0("./output/governorate_coverage_",today,".csv"), row.names = F)
browseURL(paste0("./output/governorate_coverage_",today,".csv"))



### Quality check
# Load the conditions_list, produce the cleaning log and melt it into a readable form
conditions_list <- read.csv("data/test_issues_sheet_v4.csv", stringsAsFactors = F, check.names = F)


cleaning_log <- run_checks_from_dataframe(df = response_tidy,
                                          conditions_df = conditions_list,
                                          condition.column = "codes",
                                          test.name.column = "names",
                                          meta_to_keep = c("uuid", "a1_metadata", "a2_metadata"))

write.csv(cleaning_log, paste0("./output/cleaning_log.csv_",today,".csv"), row.names = F)
browseURL(paste0("./output/cleaning_log.csv_",today,".csv"))

cleaning_log_melt <- quality_checks_log_to_long_format(data = cleaning_log,
                                                       meta_not_to_transform = c("uuid", "a1_metadata", "a2_metadata"))

#write.csv(cleaning_log_melt, "./output/melted_clog.csv", row.names = F)
#browseURL("./output/melted_clog.csv")


### Split the melted cleaning log into a one row per variable format
## Add quality checks to dataframe
cleaning_log_melt$codes <- conditions_list$codes[match(cleaning_log_melt$variable, conditions_list$names)]

## Rename column variable and delete the value column
clog_separated <- reshape::rename(cleaning_log_melt, c(variable = "description"))
clog_separated$value <- NULL

#write.csv(clog_separated, "./output/clog_separated.csv", row.names = F)
#browseURL("./output/clog_separated.csv")

## Separate reformatted quality checks into three variable to allow for easier data cleaning
clog_reformatted <- separate_on_multiple(clog_separated, "codes", sep1 = "&", sep2 = "|")


clog_reformatted$qchecks_sep <- as_tibble(str_replace_all(clog_reformatted$codes, "[= | !=]", " "))
var_split <- str_split_fixed(clog_reformatted$qchecks_sep, " ", 2)

cleaning_log_final <- cbind(clog_reformatted, var_split)
cleaning_log_final <- reshape::rename(cleaning_log_final, c(V1 = "variable_name", V2 = "old_value"))

cleaning_log_final$new_value <- NA

cleaning_log_final$codes <- NULL
cleaning_log_final$qchecks_sep <- NULL

cleaning_log_final$old_value<- gsub("\"", "", cleaning_log_final$old_value)
cleaning_log_final$old_value <- gsub("\ ", "", cleaning_log_final$old_value)

write.csv(cleaning_log_final, paste0("./output/cleaning_log_final_codes_",today,".csv"), row.names = F)
browseURL(paste0("./output/cleaning_log_final_codes_",today,".csv"))

# Replace variable names to align them with paper form
cleaning_log_final$variable_name <- questions$`label::English`[match(cleaning_log_final$variable_name, questions$name)]

# Replace options codes to align them with paper form
cleaning_log_final$old_value <- choices$`label::English`[match(cleaning_log_final$old_value, choices$name)]

write.csv(cleaning_log_final, paste0("./output/cleaning_log_final_desc_",today,".csv"), row.names = F)
browseURL(paste0("./output/cleaning_log_final_desc_",today,".csv"))











