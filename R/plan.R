#' @title drake plan
#' @rdname get_analysis_plan
#' @description Targets and functions for analyses
#' @export
#' @importFrom drake drake_plan knitr_in
#' @importFrom here here
#' @importFrom dplyr filter select
#' @importFrom readr read_csv

#'
#'
get_analysis_plan <- function(){
  drake_plan(
    # Prisma figure 
    prisma = make_prisma(retrieved = 1710, included = 19, 
                         duplicates = 858, full_text = 48, 
                         wrong_setting = 20,
                         wrong_population= 7,
                         wrong_outcome = 2),
    
    # Import data
    importdata = read_csv(
      here("data/review_69353_extracted_data_csv_20200601072844.csv")),
    data = cleandata(importdata),
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
    
    # Study characteristics table
    
    characteristics_table = make_characteristics_table(study_df,
                                                       baseline_df,
                                                       outcomes_df),
    
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
                           location == "radial" | location == "femoralradial" | location =="radialfemoral"
                           ),
    
    # won't work properly until extraction is completed (too much missing data)
    nexfin_results = meta_analysis(nexfin_df),
    
    # making dataframe for primary analysis
    primary_map_df = make_ma_df(outcomes_df = outcomes_df,
                            rob = rob,
                            study_df = study_df,
                            #add in filters below to select subset of studies (e.g. filter())
                            case_when(study!="Ameloot 2014" ~ outcome == "map" & type == "invasive",
                                      study=="Ameloot 2014" ~ outcome == "map" & type == "invasive" & location == "femoral"
                            )
    ),
    
    # making dataframe for finapres analysis
    finapres_df= make_ma_df(outcomes_df = outcomes_df,
                            rob = rob,
                            study_df = study_df,
                            
     #add in filters below to select subset of studies (e.g. filter())
                            outcome == "sbp",
                            cnap == "finapres" | cnap == "tline",
                            type == "invasive",
                            location == "radial" | location == "femoralradial" | location =="radialfemoral"
                            ),
    
    #meta-analysis for finapres
    finapres_results = meta_analysis(finapres_df),
    
    # meta-analysis for primary analysis
    primary_map_results = meta_analysis(primary_map_df),
    results_list = list(nexfin_results, primary_map_results, finapres_results),
    results_flextable = make_results_flextable(results_list,
                                       names = c("Nexfin",
                                                 "Primary",
                                                 "Finapres")),
    
   # Renders the manuscript - use drake::r_make() to render not knit
    manuscript_word = target(
      command = {
        rmarkdown::render(knitr_in("manuscript/index.Rmd"))
        file_out("manuscript/index.docx")
      }
    )
  )
}