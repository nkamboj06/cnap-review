##' @title Makes a figure of risk of bias across studies
##' @name make_rob_across
##' @importFrom dplyr arrange mutate select if_else
##' @importFrom tidyr separate
##' @importFrom robvis rob_summary
##' @importFrom glue glue

##' @return
##' @export
make_rob_across <- function(rob) {

  
  RoB <- rob %>%
    arrange(study) %>% 
    separate(study, c("Study", "Year"), sep = ", ")  %>% 
    arrange(-as.numeric(Year)) %>%
    mutate(study = glue("{Study}, {Year}"))%>% 
    select(study, 
                  "RoB_selection" = `What was the overall risk of bias associated with the selection of participants?`, 
                  "RoB_cnap" = `What was the overall risk of bias associated with the conduct or interpretation of CNAP results?`, 
                  "RoB_comparator" = `What was the overall risk of bias associated with the conduct of interpretation of comparator blood pressure measurements?`, 
                  "RoB_flow" = `What was the overall risk of bias associated with the participant flow?`) %>%
    mutate(RoB_overall = if_else(RoB_selection == "low" &
                                                 RoB_cnap == "low" &
                                                 RoB_comparator == "low" &
                                                 RoB_flow == "low", "low", "high"))
  
  RoB[RoB == "unclear"] <- "some concerns"
  
  rob_summary(data = RoB, tool = "QUADAS-2", weighted = FALSE)

}
