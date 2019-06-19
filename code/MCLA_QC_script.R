## MCLA 2019 Data Cleaning/QC Script
## REACH Yemen
## 12 June 2019
## V1

#reset workspace
rm(list=ls())


#call in any cleaning we have done before, Koboloader would be housed in this script as it takes the 
#raw data and any structural changes we have made an adapts it so we are not seeing the same problems
#we have fixed over again.
source("/code/Issue_table_MCLA_fun.R")

## Install packages
devtools::install_github("mabafaba/xlsformfill", force = T)
devtools::install_github("mabafaba/composr", force = T)

require("devtools")
require("xlsformfill")
require("cleaninginspectoR")
require("tidyverse")
require("readr")
require("reachR")
require("xlsx")
require("composr")

# Upload kobo tool and fill it with fake data
# load questionnaire
kobo_questions_v1 <- read.csv("data/tool/mcla_questions.csv", stringsAsFactors = F)
kobo_choices_v1 <- read.csv("data/tool/mcla_choices.csv", stringsAsFactors = F)

# generate fake data with `n` rows
fake_dataset <- xlsform_fill(kobo_questions_v1, kobo_choices_v1, n = 500)
fake_dataset <- fake_dataset[-c(709, 710, 711, 712, 713)]

#reincorporate the cleaned data from previous section
#this will pull the most recent file from the cleaned data folder (So dont put new files in there)

#########
#timefile <- file.info(list.files("/data/Cleaned Data", full.names = T))
#data.frame.cleaned<-read.csv(rownames(timefile)[which.max(timefile$mtime)])
#########

#Create dataset that pulls all integer questions into a new data.frame we can run a loop through
kobo_integer_quest<-kobo_questions_v1[kobo_questions_v1[,1]=="integer",]

issues <- for(i in seq_along(kobo_integer_quest$name)){
  x<-kobo_integer_quest$name[i]
  sd_x<-sd(fake_dataset[,x])
  mean_x<-mean(fake_dataset[,x])
  sd_x_max<-sd_x*3
  sd_x_min<-sd_x*-3
  mean_sd_max<-mean_x+sd_x_max
  mean_sd_min<-mean_x+sd_x_min
  for(j in nrow(fake_dataset)){
    if(fake_dataset[j,x]>mean_sd_max){
      issues_table_MCLA(fake_dataset,x,j,"outlier") 
    }else if(fake_dataset[j,x]<mean_sd_min){
      issues_table_MCLA(fake_dataset,x,j,"outlier") 
    }
  }
}


issues_integer_table <- find_outliers(select(fake_dataset, "B3y_Demographics", "B3m_Demographics", "B5_Demographics", "B713_YearBorn", "B714_MonthBorn",
                         "B715_Age", "B9_DemographicsMem", "B10_YearBorn", "B10_MonthBorn", "C5_DisplacementStatus", "F5_WASH", "F7_WASH","H8_Health", "H17_Health"))


#write file to part of the issues table sheet
write.xlsx(issues_integer_table,
           "/data/Issues/Issues_Table_",sys.date,".csv",
           sheet="Integer Issues",
           col.names=T, 
           row.names=T, 
           append=F)

#will have to append of the data issues to this in various sheets.


### Metadata Section - Quality Check
### To be done when we have real data






### Demographic Section - Quality Check

#### Displacement status and nationality - Refugee and migrants cannot be Yemeni
fake_dataset <- mutate(fake_dataset, check_nation = ifelse(grepl("yemeni", fake_dataset$B2_Demographics), 1, 0))
count_check_nation <- count(fake_dataset, check_nation)


fake_dataset <- mutate(fake_dataset, check_pop = ifelse(fake_dataset$A1_Metadata == "a1_2" | fake_dataset$A1_Metadata == "a1_4", 1, 0))
count_check_pop <- count(fake_dataset, check_pop)

table_nation <- fake_dataset %>% select("uuid", "A1_Metadata", "A2_Metadata", "A3_Metadata", "A4_Metadata", "A6_Metadata", "B2_Demographics", "check_nation", "check_pop") %>%
                filter(fake_dataset$check_nation == 1 & fake_dataset$check_pop == 1)

test <- table(fake_dataset$check_nation, fake_dataset$check_pop)


#### Count unaccompanied_children 
fake_dataset <- mutate(fake_dataset, check_unacc_child = ifelse(fake_dataset$B715_Age < 18 & fake_dataset$B717_Relationship =="b71_rel_11", 1, 0))
count_unacc_child <- count(fake_dataset, check_unacc_child)

table_unacc_child <- fake_dataset %>% select("uuid", "A1_Metadata", "A2_Metadata", "A3_Metadata", "A4_Metadata", "A6_Metadata", "B715_Age", "B717_Relationship", "check_unacc_child") %>%
                     filter(fake_dataset$check_unacc_child == 1)

#### Count unaccompanied_elderly
fake_dataset <- mutate(fake_dataset, check_unacc_elder = ifelse(fake_dataset$B715_Age > 59 & fake_dataset$B717_Relationship =="b71_rel_11", 1, 0))
count_unacc_elder <- count(fake_dataset, check_unacc_elder)

table_unacc_elder <- fake_dataset %>% select("uuid", "A1_Metadata", "A2_Metadata", "A3_Metadata", "A4_Metadata", "A6_Metadata", "B715_Age", "B717_Relationship", "check_unacc_elder") %>%
                     filter(fake_dataset$check_unacc_elder == 1)

#### Count female that pregnant or lactating
fake_dataset <- mutate(fake_dataset, check_pregnant = ifelse(fake_dataset$B712_Gender == "female" & fake_dataset$B715_Age >= 12 & fake_dataset$B715_Age <= 59 & fake_dataset$B720_Pregnent == "yes", 1,0))
count_pregnant <- count(fake_dataset, check_pregnant)

table_pregnant <- fake_dataset %>% select("uuid", "A1_Metadata", "A2_Metadata", "A3_Metadata", "A4_Metadata", "A6_Metadata", "B712_Gender", "B715_Age", "B720_Pregnent", "check_pregnant") %>%
                  filter(check_pregnant == 1)


#### Count individual considered disable
disable <- c("B72_Vision", "B72_Hearing", "B72_Mobility", "B72_Communication", "B72_Cognition", "B72_SelfCare")

#### Visual disability
fake_dataset <- fake_dataset %>%
                new_recoding(source = B72_Vision,
                             target = check_dis_vision) %>% 
                                      recode_to(1, where.selected.any = c("b71_dif_3", "b71_dif_4")) %>%
                                      recode_to(0, where.selected.any = c("b71_dif_1", "b71_dif_2")) %>% end_recoding()
                                                                                   
#### Hearing disability 
fake_dataset <- fake_dataset %>%
                new_recoding(source = B72_Hearing,
                             target = check_dis_hearing) %>% 
                                      recode_to(1, where.selected.any = c("b71_dif_3", "b71_dif_4")) %>%
                                      recode_to(0, where.selected.any = c("b71_dif_1", "b71_dif_2")) %>% end_recoding()

#### Mobility disability
fake_dataset <- fake_dataset %>%
                new_recoding(source = B72_Mobility,
                             target = check_dis_mobility) %>% 
                                      recode_to(1, where.selected.any = c("b71_dif_3", "b71_dif_4")) %>%
                                      recode_to(0, where.selected.any = c("b71_dif_1", "b71_dif_2")) %>% end_recoding()


#### Communicative disability
fake_dataset <- fake_dataset %>%
                new_recoding(source = B72_Communication,
                             target = check_dis_comms) %>% 
                                      recode_to(1, where.selected.any = c("b71_dif_3", "b71_dif_4")) %>%
                                      recode_to(0, where.selected.any = c("b71_dif_1", "b71_dif_2")) %>% end_recoding()


#### Cognitive disability
fake_dataset <- fake_dataset %>%
                new_recoding(source = B72_Cognition,
                             target = check_dis_cognition) %>% 
                                      recode_to(1, where.selected.any = c("b71_dif_3", "b71_dif_4")) %>%
                                      recode_to(0, where.selected.any = c("b71_dif_1", "b71_dif_2")) %>% end_recoding()


#### Selfcare disability
fake_dataset <- fake_dataset %>%
                new_recoding(source = B72_SelfCare,
                             target = check_dis_selfcare) %>% 
                                      recode_to(1, where.selected.any = c("b71_dif_3", "b71_dif_4")) %>%
                                      recode_to(0, where.selected.any = c("b71_dif_1", "b71_dif_2")) %>% end_recoding()


#### Code if individual is disable or not
fake_dataset <- mutate(fake_dataset, check_disable = ifelse(fake_dataset$check_dis_vision ==1 |
                                                            fake_dataset$check_dis_hearing == 1 |
                                                            fake_dataset$check_dis_mobility == 1 |
                                                            fake_dataset$check_dis_comms == 1 |
                                                            fake_dataset$check_dis_cognition == 1 |
                                                            fake_dataset$check_dis_selfcare == 1, 1, 0))

count_disable <- count(fake_dataset, check_disable)




### Displacement dynamics Section - Quality Check




### Priority needs Section - Quality Check





### Shelter Section - Quality Check





### WASH Section - Quality Check




### Health Section - Quality Check




### Protection Section - Quality Check




### Livelihoods Section - Quality Check




### Humanitarian assistanace Section - Quality Check