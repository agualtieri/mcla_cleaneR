#MCLA Cleaning Script

#install packages
require("dplyr")
require("knitr")
require("data.table")
require("questionr") 
require("reachR")
require("koboloadeR")
require("rlang")

#download koboloadeR
library(koboloadeR)

# Load cleaned dataset from kobo server
datasets <- kobo_datasets(user = "reach_yemen:KOBOyemREACH2017", api = "kobohr") # copy the formid of the dataset you want to process

# download the dataset you want to process
data.frame.raw <- kobo_data_downloader(formid = "360811", user = "reach_yemen:KOBOyemREACH2017", api = "kobohr")
data.frame.raw

# check number of submission
kobo_submission_count("360811", user = "reach_yemen:KOBOyemREACH2017", api = "kobohr")


#WHERE THE CLEANING HAPPENS(I THINK WE SHOULD DISAGGREGATE BY QUESTIONS)

#Q1

#Q2




#Will plug in the data set into cleaned form to be used in the QC script
data.frame.clean<-data.frame.raw
csvtitle<-paste0("/data/Cleaned Data/Cleaned_MCLA_",Sys.time(),".csv")
write.csv(data.frame.clean,file=csvtitle)