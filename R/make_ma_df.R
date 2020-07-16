#' @title Preprocessing for meta-analysis
#' @name make_ma_df
#' @description Make a data frame for analyses
#' @export
#' @param outcomes_df Filtered outcome_df with selection of studies
#' @param rob Data frame of risk of bias assessments
#' @param study_df Data frame of study characteristics

#' @importFrom stringr str_replace 
#' @importFrom dplyr right_join mutate case_when filter rename
#' @importFrom tidyr pivot_wider
#' @importFrom rlang enexprs
#' 

make_ma_df <- function(outcomes_df = outcomes_df,
                       rob = rob, 
                       study_df = study_df,
                       ...
                       ){
  args <- enexprs(...)
  data <- outcomes_df  %>%
    filter(!!! args)
  
  data <- data %>%
    pivot_wider(names_from = c(outcome,
                               measure
                               ))

names(data) <- str_replace(names(data), "[a-z][a-z][a-z]_", "")

data <- data %>%
  right_join(rob, by = "study") %>%
rename("RoB_selection" = `What was the overall risk of bias associated with the selection of participants?`, 
"RoB_cnap" = `What was the overall risk of bias associated with the conduct or interpretation of CNAP results?`, 
"RoB_comparator" = `What was the overall risk of bias associated with the conduct of interpretation of comparator blood pressure measurements?`, 
"RoB_flow" = `What was the overall risk of bias associated with the participant flow?`) %>%
  mutate(RoB_overall = if_else(RoB_selection == "low" &
                                 RoB_cnap == "low" &
                                 RoB_comparator == "low" &
                                 RoB_flow == "low", "low", "high")) %>%
  right_join(study_df, by = "study") %>% 
  drop_na(cnap)

data <- data %>%
  mutate(c = measurements/participants) %>%
  mutate(corrected = ifelse(
    `Were repeated measurements taken into account in the Bland-Altman analysis?`=="high",
    "Yes",
    "No"))

#variance

data <- data %>%
  mutate(s2 = case_when(corrected == "Yes"  ~ ((upperloa - meanbias)/1.96)^2,
                                      corrected == "No" ~  ((upperloa - meanbias)/1.96)^2*(measurements-1)/(measurements-c)
  )
  )

data <- data %>%
  mutate(V_bias = s2/participants)

data <- data %>%
  mutate(logs2 = log(s2)+1/(participants-1))

data <- data %>%
  mutate(V_logs2 = 2/(participants-1))
  
return(data)
}
