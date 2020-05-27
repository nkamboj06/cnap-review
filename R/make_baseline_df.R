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
  mutate(age_iqr_primary = ifelse(is.na(age_iqr_primary),age_iqr_primary_2, age_iqr_primary)) %>% 
  separate(age_iqr_primary, sep = "-", into = c("age_loweriqr_primary", "age_upperiqr_primary"), convert = TRUE) %>% 
  select(-age_iqr_primary_2) %>% 
  pivot_longer(cols = -study,
               names_to = c("characteristic", "measure", "group"),
               names_sep = "_",
               values_to = "value")  %>% 
  drop_na(value)

}