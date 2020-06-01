#' @title Makes dataframe of baseline measures
#' @name make_baseline_df
#' @description Extract baseline measures into a long data frame
#' @export
#' @importFrom dplyr mutate select starts_with
#' @importFrom tidyr pivot_longer drop_na separate
#' @importFrom stringr str_replace



make_baseline_df <- function(data){

summary <- data %>% 
  mutate(study = study_identifier) %>% 
  select(study, 
         starts_with("baseline"))

names(summary) <- str_replace(names(summary), "baseline_", "")

# need the same number of components in all the variables names to convert it to long dataframe
names(summary) <- str_replace(names(summary), "sample_", "sample_n_")

summary %>% 
  select(-age_iqr_primary) %>% 
  pivot_longer(cols = -study,
               names_to = c("characteristic", "measure", "group"),
               names_sep = "_",
               values_to = "value")  %>% 
  drop_na(value)

}