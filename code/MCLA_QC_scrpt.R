## MCLA 2019 Data Cleaning/QC Script
## REACH Yemen
## 12 June 2019
## V1

#reset workspace
rm(list=ls())

#call in any cleaning we have done before, Koboloader would be housed in this script as it takes the 
#raw data and any structural changes we have made an adapts it so we are not seeing the same problems
#we have fixed over again.
source("MCLA_cleaning_scrpt")

## Install packages
devtools::install_github("mabafaba/xlsformfill", force = T)
require("devtools")
require("xlsformfill")
require("cleaninginspectoR")
require("tidyverse")
require("readr")
require("reachR")
require("xlsx")

# Upload kobo tool and fill it with fake data
# load questionnaire
kobo_questions_v1 <- read.csv("data/tool/mcla_questions.csv", stringsAsFactors = F)
kobo_choices_v1 <- read.csv("data/tool/mcla_choices.csv", stringsAsFactors = F)

# generate fake data with `n` rows
fake_dataset <- xlsform_fill(kobo_questions_v1, kobo_choices_v1, n = 10)

#recincorporate the cleaned data from previous section
#this will pull the most recent file from the cleaned data folder (So dont put new files in there)

#########
#timefile <- file.info(list.files("/data/Cleaned Data", full.names = T))
#data.frame.cleaned<-read.csv(rownames(timefile)[which.max(timefile$mtime)])
#########

#Create dataset that pulls all integer questions into a new data.frame we can run a loop through
kobo_integer_quest<-kobo_questions_v1[kobo_questions_v1[,1]=="integer",]

for(i in seq_along(kobo_integer_quest$name)){
  x<-kobo_integer_quest$name[i]
  sd_x<-sd(fake_dataset[,'x'])
  mean_x<-mean(fake_dataset[,'x'])
  sd_x_max<-sd_x*3
  sd_x_min<-sd_x*-3
  mean_sd_max<-mean_x+sd_x_max
  mean_sd_min<-mean_x+sd_x_min
  for(j in nrow(fake_dataset)){
    if(fake_dataset[j,'x']>mean_sd_max){
      issues_table_MCLA(fake_dataset,x,j,"outlier") 
    }else if(fake_dataset[j,'x']<mean_sd_min){
      issues_table_MCLA(fake_dataset,x,j,"outlier") 
    }
  }
}

#write file to part of the issues table sheet
write.xlsx(issues_table,
           "/data/Issues/Issues_Table_",sys.date,".csv",
           sheet="Integer Issues",
           col.names=T, 
           row.names=T, 
           append=F)

#will have to append of the data issues to this in various sheets.
