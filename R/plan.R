#' @title drake plan
#' @rdname get_analysis_plan
#' @description Targets and functions for analyses
#' @export
#' @importFrom janitor clean_names
#' @importFrom drake drake_plan knitr_in
#' @importFrom here here
#' @importFrom dplyr filter select
#' @importFrom readr read_csv

#'
#'
get_analysis_plan <- function(){
  drake_plan(
    # Prisma figure 
    prisma = make_prisma(),
    
    # Import data
    data = read_csv(
      here("data/covidenceexport.csv")) %>% 
      clean_names(),
    rob = create_clean_rob(
      here("data/review_69353_quality_assessment_export.csv")),
    study_df = data %>% select(
      "study" = study_identifier,
      "sponsor" = sponsorship_source,
      country,
      setting
    ),
    
    # Risk of bias figures
    rob_within = make_rob_within(rob),
    rob_across = make_rob_across(rob),
    
    # Meta-analysis
    baseline_df = make_baseline_df(data),
    outcomes_df = make_outcome_df(data),
    nexfin_df = make_ma_df(outcomes_df = outcomes_df,
                           rob = rob, 
                           study_df = study_df,
     #add in filters below to select subset of studies (e.g. filter())
                           outcome == "sbp",
                           cnap == "nexfin" | cnap == "tline",
                           type == "invasive",
                           location == "radial",
                           group == "primary",
                           ),
    # won't work properly until extraction is completed (too much missing data)
    nexfin_results = meta_analysis(nexfin_df),
    results_list = list(nexfin_results),
    results_flextable = make_results_flextable(results_list,
                                       names = c("Nexfin")),
   # Renders the manuscript
    # manuscript_word = target(
    #   command = {
    #     rmarkdown::render(knitr_in("manuscript/index.Rmd"))
    #     file_out("manuscript/index.docx")
    #   }
    # )
  )
}
    