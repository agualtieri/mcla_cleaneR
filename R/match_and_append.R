



match_and_append <- function(df, source_df, df_variable_for_match, source_df_variable_for_match, variable_to_append){
  
  #setNames(variable_for_match, "variable")
  #setNames(variable_to_append, "variable")
  
  
  df$appended_var <- source_df$variable_to_append[match(df$variable_for_match, source_df$variable_for_match)]
  
  df
}




test <- match_and_append(cleaning_log_melt, 
                         conditions_list,
                         "variable",
                         "check_names",
                         "conditions")



cleaning_log_melt$quality_checks <- conditions_list$conditions[match(cleaning_log_melt$variable, conditions_list$check_names)]




rm(test)
