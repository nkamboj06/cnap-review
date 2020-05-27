#' @title Makes dataframe of outcomes
#' @name make_outcome_df
#' @description Extract outcomes into long data frame
#' @export
#' @importFrom dplyr mutate select starts_with
#' @importFrom tidyr pivot_longer drop_na


make_outcome_df <- function(data){
  data %>% 
  mutate(study = study_identifier) %>% 
  select(study, 
         starts_with("sbp"), 
         starts_with("dbp"),
         starts_with("map")) %>% 
  pivot_longer(cols = -study,
               names_to = c("outcome", "cnap", "type","location", "group", "measure"),
               names_sep = "_",
               values_to = "value")  %>% 
  drop_na(value) 
    
}
