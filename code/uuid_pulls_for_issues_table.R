uuid_pull<-function(data, issues){
  issues$Governorate<-0
  issues$District<-0
  issues$Subdistrict<-0
  issues$Location<-0
  issues$Enum_ID<-0
  issues$Date<-0
  
    colnum_issues<-which(grepl("uuid",colnames(issues)))
  
  for(i in nrow:issues){
    cross_uuid<-issues[i,colnum_issues]
    row_data<-which(grepl(cross_uuid,data$uuid))
    
    issues[i,"Governorate"]<-fake_dataset[row_data,"A2_Metadata"]
    issues[i,"District"]<-fake_dataset[row_data,"A3_Metadata"]
    issues[i,"Subdistrict"]<-fake_dataset[row_data,"A4_Metadata"]
    issues[i,"Location"]<-fake_dataset[row_data,"A5_Metadata"]
    issues[i,"Enum_ID"]<-fake_dataset[row_data,"A6_Metadata"]
    issues[i,"Date"]<-fake_dataset[row_data,"A7_Metadata"]
    
    
  }
}
  