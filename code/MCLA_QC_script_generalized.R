#' \name{batch_issues_check}
#' \alias{batch_issues_check}
#' \title{Batch Quality Checks}
#' \usage{ 
#'      batch_issues_checks(data, condition_table, meta_to_keep=c())
#'      }
#' The following function reads illegal combination of varibles provided by @param condition_table against the data present inside the @param data
#' and produced and editable output that can be used for data quality checks.
#'      
#'\arguments{}      
#' @param data the dataset one wishes to control for quality
#' @param condition_table as dataframe containinung 
#' }  




## Bach isssue recoding
batch_issue_checks<-function(data, condition_table, meta_to_keep = c()){
  
  if(is.data.frame(data)) {
    data <- as.data.frame(data)
    
  }
  
  if(is.data.frame(condition_table)) {
    condition_table <- as.data.frame(condition_table)  
  
  }
  
  
  
  
  conditions_table<-lapply(conditions_table,as.character) %>% as_tibble
  
  data_with_issues<-data %>% recode_batch(tos = rep(1,nrow(conditions_table)),
                                      wheres = conditions_table$conditions,
                                      targets = conditions_table$check_names) %>%
    end_recoding()
  
  
  unique_targets<-unique(conditions_table$check_names)  
  data_with_issues[,unique_targets]<-lapply(data_with_issues[,unique_targets],function(x){
    x[is.na(x)]<-0
    x
  }) %>% as_tibble
  
  
  
  data_with_issues %>% select(c(meta_to_keep,unique_targets))
  
  
}



conditions_table <- read.csv("data/mcla_issues_sheet2.csv", stringsAsFactors = F)


batch_issue_checks(df = fake_dataset, conditions_table, meta_to_keep = c("uuid", "A7_Metadata", "A6_Metadata", "A1_Metadata", "A2_Metadata", "A3_Metadata", "A4_Metadata", "A5_Metadata")) %>% write.csv("test.csv")
browseURL("test.csv")



