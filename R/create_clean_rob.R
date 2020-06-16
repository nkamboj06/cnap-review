##' @title Cleans rob dataframe
##' @name create_clean_rob
##' @param rob Risk of bias export from covidence
##' @return
##' @author Aaron Conway
##' @export
create_clean_rob <- function(rob) {

    col_names <- names(read_csv(rob, 
                                n_max = 0))
   rob <-  read_csv(rob, 
                    col_names = col_names, skip = 3)
rob %>% 
  filter(Reviewer == "Consensus") %>% 
  select("study" = `Study Id`,
         contains("overall"),
         contains("bland")) %>% 
  mutate(`What was the overall risk of bias associated with the selection of participants?`=
           ifelse(is.na(`What was the overall risk of bias associated with the selection of participants?`),
                  `What was the overall risk of bias associated with the selection of participants? for All outcomes`,
         `What was the overall risk of bias associated with the selection of participants?`))
}
