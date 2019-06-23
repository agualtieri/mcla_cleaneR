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
devtools::install_github("mabafaba/cleaninginspectoR", force = T)
devtools::install_github("mabafaba/reachR2", force = T)
devtools::install_github("mabafaba/hypegrammaR", force = T)


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
#kobo_integer_quest<-kobo_questions_v1[kobo_questions_v1[,1]=="integer",]

#issues <- for(i in seq_along(kobo_integer_quest$name)){
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
#}


#issues_integer_table <- find_outliers(select(fake_dataset, "B3y_Demographics", "B3m_Demographics", "B5_Demographics", "B713_YearBorn", "B714_MonthBorn",
                         #"B715_Age", "B9_DemographicsMem", "B10_YearBorn", "B10_MonthBorn", "C5_DisplacementStatus", "F5_WASH", "F7_WASH","H8_Health", "H17_Health"))


#write file to part of the issues table sheet
#write.xlsx(issues_integer_table,
          # "/data/Issues/Issues_Table_",sys.date,".csv",
          #sheet="Integer Issues",
          # col.names=T, 
          # row.names=T, 
          # append=F)

#will have to append of the data issues to this in various sheets.


#################################### Metadata Section - Quality Check########################################################################
### To be done when we have real data

master
########################## Demographic Section - Quality Check##################################################################

### Demographic Section - Quality Check


=======

  
### Demographic Section - Quality Check
master
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


#################################### Displacement dynamics Section - Quality Check ####################################

#### Months since they left they place of origin
lenght_displacement <- find_outliers(fake_dataset$C5_DisplacementStatus)

#lenght_displacement_group <- fake_dataset %>% group_by(A3_Metadata) %>% find_outliers(C5_DisplacementStatus)
 
#### Displacement matrix - mouvement intention in the short and long term
table_mouvement_intentions <- select(fake_dataset, "uuid", "A1_Metadata", "A2_Metadata", "A3_Metadata", "A4_Metadata", "A6_Metadata", "C9_DisplacementStatus", "C10_DisplacementStatus")

table_mouvement_int
entions<- mutate(table_mouvement_intentions, check_mouvement = ifelse(table_mouvement_intentions$C9_DisplacementStatus == "c9_10_1" & table_mouvement_intentions$C10_DisplacementStatus == "c9_10_2" |  
                                                                                         table_mouvement_intentions$C9_DisplacementStatus == "c9_10_2" & table_mouvement_intentions$C10_DisplacementStatus == "c9_10_1" |                                                     
                                                                                         table_mouvement_intentions$C9_DisplacementStatus == "c9_10_3" & table_mouvement_intentions$C10_DisplacementStatus == "c9_10_1" |  
                                                                                         table_mouvement_intentions$C9_DisplacementStatus == "c9_10_4" & table_mouvement_intentions$C10_DisplacementStatus == "c9_10_1" |   
                                                                                         table_mouvement_intentions$C9_DisplacementStatus == "c9_10_5" & table_mouvement_intentions$C10_DisplacementStatus == "c9_10_1" |
                                                                                         table_mouvement_intentions$C9_DisplacementStatus == "c9_10_3" & table_mouvement_intentions$C10_DisplacementStatus == "c9_10_2" |
                                                                                         table_mouvement_intentions$C9_DisplacementStatus == "c9_10_4" & table_mouvement_intentions$C10_DisplacementStatus == "c9_10_2" |
                                                                                         table_mouvement_intentions$C9_DisplacementStatus == "c9_10_5" & table_mouvement_intentions$C10_DisplacementStatus == "c9_10_2" |
                                                                                         table_mouvement_intentions$C9_DisplacementStatus == "c9_10_5" & table_mouvement_intentions$C10_DisplacementStatus == "c9_10_3" |   
                                                                                         table_mouvement_intentions$C9_DisplacementStatus == "c9_10_5" & table_mouvement_intentions$C10_DisplacementStatus == "c9_10_4" , 
                                                                                         1, 0))
count_mouvement_intentions <- count(table_mouvement_intentions, check_mouvement)                                                                                           
                                                                                  

#################################### Priority needs Section - Quality Check ###########################################

#### Check that the first ranked is not a "I don't know" or "I don't want to answer"
table_priority_needs <- select(fake_dataset, "uuid", "A1_Metadata", "A2_Metadata", "A3_Metadata", "A4_Metadata", "A6_Metadata", "D1a_PriorityNeeds", "D1b_PriorityNeeds", "D1c_PriorityNeeds")

table_priority_needs <- mutate(table_priority_needs, check_priority_needs = ifelse(table_priority_needs$D1a_PriorityNeeds == "d_18" | table_priority_needs$D1a_PriorityNeeds == "d_19" |
                                                                                   table_priority_needs$D1b_PriorityNeeds == table_priority_needs$D1a_PriorityNeeds |
                                                                                   table_priority_needs$D1c_PriorityNeeds == table_priority_needs$D1b_PriorityNeeds |
                                                                                   table_priority_needs$D1c_PriorityNeeds == table_priority_needs$D1a_PriorityNeeds, 
                                                                                   1, 0))
count_priority_neeeds <- count(table_priority_needs, check_priority_needs)


### Shelter Section - Quality Check
fake_dataset <- fake_dataset %>%
                             new_recoding(source = E5_ShelterNFICCCM,
                                          target = check_shelter_damage) %>%
                                                   recode_to(1, where.selected.any = "e5_3",
                                                             otherwise.to = 0,
                                                             na.to = NA)  %>% end_recoding()



table_shelter_issues <- select(fake_dataset, "uuid", "A1_Metadata", "A2_Metadata", "A3_Metadata", "A4_Metadata", "A6_Metadata", "E1d_ShelterNFICCCM", "E5_ShelterNFICCCM", "check_shelter_damage")

table_shelter_issues <- mutate(table_shelter_issues, check_shelter = ifelse(table_shelter_issues$check_shelter_damage == 1 & table_shelter_issues$E1d_ShelterNFICCCM == "e1d_1" |
                                                                            table_shelter_issues$check_shelter_damage == 1 & table_shelter_issues$E1d_ShelterNFICCCM == "e1d_2" |
                                                                            table_shelter_issues$check_shelter_damage == 1 & table_shelter_issues$E1d_ShelterNFICCCM == "e1d_3", 1, 0))


count_shelter_issues <- count(table_shelter_issues, check_shelter)

 master
#################################### EDUCATION Section - Quality Check ################################################

##G_MCLA01
#skip this section if there are no children in the HH
######################################################
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
=======
### WASH Section - Quality Check
#### Number of Containers
WASH_containers <- find_outliers(fake_dataset$F5_WASH)

#### Distance from water source aligned with type of water source
table_WASH_watercollection <- select(fake_dataset,  "uuid", "A1_Metadata", "A2_Metadata", "A3_Metadata", "A4_Metadata", "A6_Metadata", "F3_WASH", "F6_WASH")

table_WASH_watercollection <- mutate(table_WASH_watercollection, check_WASH = ifelse(table_WASH_watercollection$F3_WASH == "f3_1" & table_WASH_watercollection$F6_WASH == "f6_4" |
                                                                                     table_WASH_watercollection$F3_WASH == "f3_1" & table_WASH_watercollection$F6_WASH == "f6_5" |
                                                                                     table_WASH_watercollection$F3_WASH == "f3_2" & table_WASH_watercollection$F6_WASH == "f6_4" |
                                                                                     table_WASH_watercollection$F3_WASH == "f3_2" & table_WASH_watercollection$F6_WASH == "f6_5" |
                                                                                     table_WASH_watercollection$F3_WASH == "f3_9" & table_WASH_watercollection$F6_WASH == "f6_4" |
                                                                                     table_WASH_watercollection$F3_WASH == "f3_1" & table_WASH_watercollection$F6_WASH == "f6_5",
                                                                                     1, 0))

count_watercollection <- count(table_WASH_watercollection, check_WASH)

### Health Section - Quality Check



master

G_MCLA01<- fake_dataset %>%
  filter(`G2_Education/g2_1` == 'TRUE'|`G2_Education/g2_1` == "FALSE") %>% 
  filter(B715_Age > 25 |B715_Age < 6) %>% #which children arent of school age
  select('uuid', 
         varval1=B715_Age , 
         varval2=`G2_Education/g2_1`)  %>%
  mutate(IssueCode= "G_MCLA01", 
         var1= "B715_Age", 
         var2= "G1_Education")%>%
  subset(select= c(1,4,5,2,6,3)) #reorder columns
#NEEDS TO BE REWRITTEN TO DETECT IF EVERY MEMBER IS NOT OF SCHOOL AGE

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



master
##G_MCLA02
#cross check g2 (reasons for not attending school) with h2 (HH members responses in other sections)
######################################################

#which children faced childhood marriage 
#but didnt use marriage as coping mechanism
G_MCLA02_marriage<- fake_dataset %>%
  filter(fake_dataset$`G2_Education/g2_4` == 'TRUE'& `J7_Livelihood/j7_11`== 'FALSE') %>% 
  select('uuid', 
         varval1=`G2_Education/g2_4` , 
         varval2=`J7_Livelihood/j7_11`)  %>%
  mutate(IssueCode= "G_MCLA02", 
         var1= "G2_Education/g2_4", 
         var2= "J7_Livelihood/j7_11")%>%
  subset(select= c(1,4,5,2,6,3))  %>% #reorder columns
  as_tibble()
fake_dataset$B721_SchoolEnrolment

#edu health issue reported but not 
#in health section
G_MCLA02_illness<- fake_dataset %>%
  filter(`G2_Education/g2_6` == 'TRUE' & `H10_Health/h10_1` =="FALSE") %>% 
  select('uuid', 
         varval1=`G2_Education/g2_6` , 
         varval2=`H10_Health/h10_1`)  %>%
  mutate(IssueCode= "G_MCLA02", 
         var1= "G2_Education/g2_6", 
         var2= "H10_Health/h10_1")%>%
  subset(select= c(1,4,5,2,6,3))  %>% #reorder columns
  as_tibble()

#Psychological distress was reason for not 
#attending edu but not as member information
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

G_MCLA02_psych<- fake_dataset %>%
  filter(`G2_Education/g2_5` == 'TRUE' & B726_PsychologicalAddress =="no") %>% 
  select('uuid', 
         varval1=`G2_Education/g2_5` , 
         varval2=B726_PsychologicalAddress)  %>%
  mutate(IssueCode= "G_MCLA02", 
         var1= "G2_Education/g2_5", 
         var2= "B726_PsychologicalAddress")%>%
  subset(select= c(1,4,5,2,6,3))  %>% #reorder columns
  as_tibble()
#NEEDS TO BE REWRITTEN TO DETECT IF EVERY MEMBER HAD 'no' TO B726

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

G_MCLA02<-rbind(G_MCLA02_marriage, G_MCLA02_illness, G_MCLA02_psych)



##G_MCLA03
#cross check G section responded are any member information indicating school attendance?
######################################################
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

G_MCLA03<- fake_dataset %>%
  filter(`G2_Education/g2_1` == 'TRUE'|`G2_Education/g2_1` == "FALSE") %>% 
  filter( B721_SchoolEnrolment =="b71_enr_3") %>% 
  select('uuid', 
         varval1=`G2_Education/g2_1` , 
         varval2=B721_SchoolEnrolment)  %>%
  mutate(IssueCode= "G_MCLA03", 
         var1= "G2_Education/g2_1", 
         var2= "B721_SchoolEnrolment")%>%
  subset(select= c(1,4,5,2,6,3))  %>% #reorder columns
  as_tibble()
#NEEDS TO BE REWRITTEN TO DETECT IF EVERY MEMBER HAD 'b71_edu_8' TO B721_SchoolEnrolment

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



#################################### WASH Section - Quality Check #####################################################




#################################### Health Section - Quality Check ###################################################




#################################### Protection Section - Quality Check ###############################################

##I_MCLA01
#Check that the protection support services selected as available/accessible were not selected as 
#unavailable/inaccessible in the health questions
######################################################

#'general health' services
I_MCLA01_genhealth<- fake_dataset %>%
  filter(str_detect(H6_Health, pattern="h5_6_1 ")) %>% 
  filter(str_detect(I5_Protection, pattern="i5_2")|str_detect(I4_Protection, pattern="i4_2")) %>%
  #values
  select('uuid',
         varval1=`H6_Health/h5_6_1` , 
         varval2=`I5_Protection/i5_2`,
         varval3=`I4_Protection/i4_2`)  %>%
  #variables
  mutate(IssueCode= "I_MCLA01",
         var1= "H6_Health/h5_6_1", 
         var2= "I5_Protection/i5_2",
         var3= "I4_Protection/i4_2") %>%
  subset(select= c(1,5,6,2,7,3,8,4)) %>% #reorder columns
  as_tibble()


#'mental health' services
I_MCLA01_mental<- fake_dataset %>%
  filter(str_detect(H6_Health, pattern="h5_6_7 ")) %>% 
  filter(str_detect(I5_Protection, pattern="i5_1")|str_detect(I4_Protection, pattern="i4_1")) %>%
  #values
  select('uuid',
         varval1=`H6_Health/h5_6_7` , 
         varval2=`I5_Protection/i5_1`,
         varval3=`I4_Protection/i4_1`)  %>%
  #variables
  mutate(IssueCode= "I_MCLA01",
         var1= "H6_Health/h5_6_7", 
         var2= "I5_Protection/i5_1",
         var3= "I4_Protection/i4_1") %>%
  subset(select= c(1,5,6,2,7,3,8,4)) %>% #reorder columns
  as_tibble()


I_MCLA01<-rbind(I_MCLA01_genhealth, I_MCLA01_mental)

##I_MCLA02
#Check that the protection support services selected as accessed were not selected as 
#unavailable in the health questions or not available
######################################################

#'general health' services 
I_MCLA02_genhealth<- fake_dataset %>%
  filter(str_detect(H5_Health, pattern="h5_6_1 ")) %>% 
  filter(str_detect(I5_Protection, pattern="i5_2", negate=TRUE)|str_detect(I4_Protection, pattern="i4_2", negate = TRUE)) %>% 
  #values
  select('uuid',                        
         varval1=`H5_Health/h5_6_1` , 
         varval2=`I5_Protection/i5_2`,
         varval3=`I4_Protection/i4_2`) %>%
  #variables
  mutate(IssueCode= "I_MCLA02",         
         var1= "H5_Health/h5_6_1", 
         var2= "I5_Protection/i5_2",
         var3= "I4_Protection/i4_2") %>%
  subset(select= c(1,5,6,2,7,3,8,4)) %>% #reorder columns
  as_tibble()


#'mental health' 
I_MCLA02_mental<- fake_dataset %>%
  filter(str_detect(H5_Health, pattern="h5_6_7 ")) %>% 
  filter(str_detect(I5_Protection, pattern="i5_1", negate=TRUE)|str_detect(I4_Protection, pattern="i4_1", negate=TRUE)) %>% 
  #values
  select('uuid',
         varval1=`H5_Health/h5_6_7` , 
         varval2=`I5_Protection/i5_1`,
         varval3=`I4_Protection/i4_1`) %>%
  #variables
  mutate(IssueCode= "I_MCLA02",
         var1= "H5_Health/h5_6_7", 
         var2= "I5_Protection/i5_1",
         var3= "I4_Protection/i4_1") %>%
  subset(select= c(1,5,6,2,7,3,8,4)) %>% #reorder columns
  as_tibble()


I_MCLA02<-rbind(I_MCLA02_genhealth, I_MCLA02_mental)
#################################### Livelihoods Section - Quality Check ##############################################




#################################### Humanitarian assistanace Section - Quality Check #################################
=======
### Livelihoods Section - Quality Check
table_livelihood_needs <- select(fake_dataset, "uuid", "A1_Metadata", "A2_Metadata", "A3_Metadata", "A4_Metadata", "A6_Metadata", "B724_SectorEmployment", "J10_Livelihood")

table_livelihood_needs <- mutate(table_livelihood_needs, check_livelihood = ifelse(table_livelihood_needs$B724_SectorEmployment != "b71_emp_1" & table_livelihood_needs$J10_Livelihood == "j10_1" |
                                                                                   table_livelihood_needs$B724_SectorEmployment != "b71_emp_1" & table_livelihood_needs$J10_Livelihood == "j10_2" |
                                                                                   table_livelihood_needs$B724_SectorEmployment != "b71_emp_1" & table_livelihood_needs$J10_Livelihood == "j10_3" |
                                                                                   table_livelihood_needs$B724_SectorEmployment != "b71_emp_1" & table_livelihood_needs$J10_Livelihood == "j10_4" |
                                                                                   table_livelihood_needs$B724_SectorEmployment != "b71_emp_1" & table_livelihood_needs$J10_Livelihood == "j10_5" |
                                                                                   table_livelihood_needs$B724_SectorEmployment != "b71_emp_1" & table_livelihood_needs$J10_Livelihood == "j10_6",
                                                                                   
                                                                                   1, 0))
count_livelihhod_needs <- count(table_livelihood_needs, check_livelihood)

### Humanitarian assistanace Section - Quality Check
fake_dataset <- fake_dataset %>%
                             new_recoding(source = L5_HummAssist,
                                          target = L5_HummAssist_REC) %>%
                                          recode_to(1, where.selected.any = c("l5_8", "l5_9"),
                                                    otherwise.to = 0) %>% end_recoding()


table_hummassist <- select(fake_dataset, "uuid", "A1_Metadata", "A2_Metadata", "A3_Metadata", "A4_Metadata", "A6_Metadata", "L5_HummAssist", "L5_HummAssist_REC", "L6_HummAssist", "L7_HummAssist")

table_hummassist <- mutate(table_hummassist, check_hummassist = ifelse(table_hummassist$L5_HummAssist_REC == 1 & table_hummassist$L6_HummAssist != "l6_3" & table_hummassist$L7_HummAssist != "yes", 1, 0))

count_hummassist <- count(table_hummassist, check_hummassist)
master
