##' @title Cleans rob dataframe
##' @name create_clean_rob
##' @param rob Risk of bias export from covidence
##' @return
##' @author Aaron Conway
##' @export
create_clean_rob <- function(rob) {

    col_names <- names(read_csv("data/review_69353_quality_assessment_export.csv", n_max = 0))
   rob <-  read_csv("data/review_69353_quality_assessment_export.csv", col_names = col_names, skip = 3)
rob %>% 
  filter(Reviewer == "Consensus") %>% 
  select("study" = `Study Id`,
         contains("overall"),
         contains("bland"))
}
