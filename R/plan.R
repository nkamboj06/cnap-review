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
    
    # dataframe for nexfin analysis
    nexfin_df = make_ma_df(outcomes_df = outcomes_df,
                           rob = rob, 
                           study_df = study_df,
                           #add in filters below to select subset of studies (e.g. filter())
                           outcome == "sbp",
                           cnap == "nexfin",
                           type == "invasive",
                           location == "radial" | location == "femoralradial" | location =="radialfemoral" | location == "femoral",
                           case_when(study!="Ameloot 2014" ~ outcome == "sbp" & type == "invasive",
                                     study=="Ameloot 2014" ~ outcome == "sbp" & type == "invasive" & location == "femoral"
                           )
    ),
    
    nexfin_dbp_df = make_ma_df(outcomes_df = outcomes_df,
                           rob = rob, 
                           study_df = study_df,
                           #add in filters below to select subset of studies (e.g. filter())
                           outcome == "dbp",
                           cnap == "nexfin",
                           type == "invasive",
                           location == "radial" | location == "femoralradial" | location =="radialfemoral" | location == "femoral",
                           case_when(study!="Ameloot 2014" ~ outcome == "dbp" & type == "invasive",
                                     study=="Ameloot 2014" ~ outcome == "dbp" & type == "invasive" & location == "femoral"
                           )
    ),
    
   
    nexfin_map_df = make_ma_df(outcomes_df = outcomes_df,
                               rob = rob, 
                               study_df = study_df,
                               #add in filters below to select subset of studies (e.g. filter())
                               outcome == "map",
                               cnap == "nexfin",
                               type == "invasive",
                               location == "radial" | location == "femoralradial" | location =="radialfemoral" | location == "femoral",
                               case_when(study!="Ameloot 2014" ~ outcome == "map" & type == "invasive",
                                         study=="Ameloot 2014" ~ outcome == "map" & type == "invasive" & location == "femoral"
                               )
    ),
    
    # meta-analysis for nexfin
    nexfin_results = meta_analysis(nexfin_df),
    nexfin_dbp_results = meta_analysis(nexfin_dbp_df),
    nexfin_map_results = meta_analysis(nexfin_map_df),
    
    # making dataframe for primary analysis
    primary_map_df = make_ma_df(outcomes_df = outcomes_df,
                                rob = rob,
                                study_df = study_df,
                                #add in filters below to select subset of studies (e.g. filter())
                                case_when(study!="Ameloot 2014" ~ outcome == "map" & type == "invasive",
                                          study=="Ameloot 2014" ~ outcome == "map" & type == "invasive" & location == "femoral"
                                )
    ),
    
   primary_map_df_lowrisk = primary_map_df %>%
     filter(RoB_overall == "low"),
   
   primary_map_df_nofunding = primary_map_df %>%
     filter(sponsor == "No funding recieved." | sponsor == "No funding source." | sponsor == "No funding received." | sponsor == "No funding was received."| sponsor == "No funding indicated."),
   
    primary_sbp_df = make_ma_df(outcomes_df = outcomes_df,
                                rob = rob,
                                study_df = study_df,
                                #add in filters below to select subset of studies (e.g. filter())
                                outcome == "sbp",
                                type == "invasive",
                                location == "radial" | location == "radialfemoral" | location == "radialfemoral" | location == "femoral"
                                ),

   primary_sbp_df_lowrisk = primary_sbp_df %>%
     filter(RoB_overall == "low"),
   
   primary_sbp_df_nofunding = primary_sbp_df %>%
     filter(sponsor == "No funding recieved." | sponsor == "No funding source." | sponsor == "No funding received." | sponsor == "No funding was received."| sponsor == "No funding indicated."),
   
    primary_dbp_df = make_ma_df(outcomes_df = outcomes_df,
                                rob = rob,
                                study_df = study_df,
                                #add in filters below to select subset of studies (e.g. filter())
                                outcome == "dbp",
                                type == "invasive",
                                location == "radial" | location == "radialfemoral" | location == "radialfemoral" | location == "femoral"
                                ),
    
   primary_dbp_df_lowrisk = primary_dbp_df %>%
     filter(RoB_overall == "low"),
   
   primary_dbp_df_nofunding = primary_dbp_df %>%
     filter(sponsor == "No funding recieved." | sponsor == "No funding source." | sponsor == "No funding received." | sponsor == "No funding was received."| sponsor == "No funding indicated."),
   
    # make dataframe for clinically available devices
   clinicallyavailable_sbp_df= make_ma_df(outcomes_df = outcomes_df,
                          rob = rob,
                          study_df = study_df,
                          #add in filters below to select subset of studies (e.g. filter())
                          outcome == "sbp",
                          cnap == "cnap" | cnap == "nexfin" | cnap =="tline",
                          type == "invasive",
                          location == "radial" | location == "radialfemoral" | location == "radialfemoral" | location == "femoral"
                          
   ), 
   
   clinicallyavailable_dbp_df= make_ma_df(outcomes_df = outcomes_df,
                                                   rob = rob,
                                                   study_df = study_df,
                                                   #add in filters below to select subset of studies (e.g. filter())
                                                   outcome == "dbp",
                                                   cnap == "cnap" | cnap == "nexfin" | cnap =="tline",
                                                   type == "invasive",
                                                   location == "radial" | location == "radialfemoral" | location == "radialfemoral" | location == "femoral"
   ),
   
   clinicallyavailable_map_df= make_ma_df(outcomes_df = outcomes_df,
                                                   rob = rob,
                                                   study_df = study_df,
                                                   #add in filters below to select subset of studies (e.g. filter())
                                                   outcome == "map",
                                                   cnap == "cnap" | cnap == "nexfin" | cnap =="tline",
                                                   type == "invasive",
                                                   location == "radial" | location == "radialfemoral" | location == "radialfemoral" | location == "femoral",
                                                   case_when(study!="Ameloot 2014" ~ outcome == "map" & type == "invasive",
                                                             study=="Ameloot 2014" ~ outcome == "map" & type == "invasive" & location == "femoral"
                                                   )
   ),
   
   # meta-analysis for clinically available devices analysis
   clinicallyavailable_sbp_results = meta_analysis(clinicallyavailable_sbp_df),
   clinicallyavailable_dbp_results = meta_analysis(clinicallyavailable_dbp_df),
   clinicallyavailable_map_results = meta_analysis(clinicallyavailable_map_df),
   
     # meta-analysis for primary analysis
    primary_map_results = meta_analysis(primary_map_df),
    primary_sbp_results = meta_analysis(primary_sbp_df),
    primary_dbp_results = meta_analysis(primary_dbp_df),
    primary_sbp_lowrisk_results = meta_analysis(primary_sbp_df_lowrisk),
    primary_map_lowrisk_results = meta_analysis(primary_map_df_lowrisk),
    primary_dbp_lowrisk_results = meta_analysis(primary_dbp_df_lowrisk),
    primary_sbp_nofunding_results = meta_analysis(primary_sbp_df_nofunding),
    primary_map_nofunding_results = meta_analysis(primary_map_df_nofunding),
    primary_dbp_nofunding_results = meta_analysis(primary_dbp_df_nofunding),
    results_list = list(
     primary_sbp_results, primary_dbp_results, primary_map_results, primary_sbp_lowrisk_results, primary_dbp_lowrisk_results, primary_map_lowrisk_results, primary_sbp_nofunding_results, primary_dbp_nofunding_results, primary_map_nofunding_results, clinicallyavailable_sbp_results, clinicallyavailable_dbp_results, clinicallyavailable_map_results, vc_sbp_results, vc_dbp_results, vc_map_results, aat_sbp_results, aat_dbp_results, aat_map_results, nexfin_results, nexfin_dbp_results, nexfin_map_results, tline_sbp_results, tline_dbp_results, tline_map_results, cnap_sbp_results, cnap_dbp_results, cnap_map_results, femoral_sbp_results, femoral_dbp_results, femoral_map_results, radial_sbp_results, radial_dbp_results, radial_map_results),
    results_flextable = make_results_flextable(results_list,
                                       names = c("Primary SBP",
                                                 "Primary DBP",
                                                 "Primary MAP",
                                                 "Low risk studies SBP",
                                                 "Low risk studies DBP",
                                                 "Low risk studies MAP",
                                                 "Studies not funded SBP",
                                                 "Studies not funded DBP",
                                                 "Studies not funded MAP",
                                                 "Clinically Available Devices SBP",
                                                 "Clinically Available Devices DBP",
                                                 "Clinically Available Devices MAP",
                                                 "Volume Clamp SBP",
                                                 "Volume Clamp DBP",
                                                 "Volume Clamp MAP",
                                                 "AAT SBP",
                                                 "AAT DBP",
                                                 "AAT MAP",
                                                 "Nexfin SBP",
                                                 "Nexfin DBP",
                                                 "Nexfin MAP",
                                                 "Tline SBP",
                                                 "Tline DBP",
                                                 "Tline MAP",
                                                 "CNAP SBP",
                                                 "CNAP DBP",
                                                 "CNAP MAP",
                                                 "Femoral SBP",
                                                 "Femoral DBP",
                                                 "Femoral MAP",
                                                 "Radial SBP",
                                                 "Radial DBP",
                                                 "Radial MAP")),
   
   
   #making dataframe for primary low risk analysis
   low_sbp_df= make_ma_df(outcomes_df = outcomes_df,
                           rob = rob,
                           study_df = study_df,
                           #add in filters below to select subset of studies (e.g. filter())
                           outcome == "sbp",
                           cnap == "cnap",
                           type == "invasive",
                           location == "radial" | location == "radialfemoral" | location == "radialfemoral" | location == "femoral"
   ), 
   #making dataframe for cnap sbp analysis
    cnap_sbp_df= make_ma_df(outcomes_df = outcomes_df,
                        rob = rob,
                        study_df = study_df,
                        #add in filters below to select subset of studies (e.g. filter())
                        outcome == "sbp",
                        cnap == "cnap",
                        type == "invasive",
                        location == "radial" | location == "radialfemoral" | location == "radialfemoral" | location == "femoral"
    ),
    
     cnap_dbp_df= make_ma_df(outcomes_df = outcomes_df,
                            rob = rob,
                            study_df = study_df,
                            #add in filters below to select subset of studies (e.g. filter())
                            outcome == "dbp",
                            cnap == "cnap",
                            type == "invasive",
                            location == "radial" | location == "radialfemoral" | location == "radialfemoral" | location == "femoral"
    ),
    
    cnap_map_df= make_ma_df(outcomes_df = outcomes_df,
                            rob = rob,
                            study_df = study_df,
                            #add in filters below to select subset of studies (e.g. filter())
                            outcome == "map",
                            cnap == "cnap",
                            type == "invasive",
                            location == "radial" | location == "radialfemoral" | location == "radialfemoral" | location == "femoral"
    ),
    
     # meta-analysis for cnap analysis
    cnap_sbp_results = meta_analysis(cnap_sbp_df),
    cnap_dbp_results = meta_analysis(cnap_dbp_df),
    cnap_map_results = meta_analysis(cnap_map_df),
    
    #making dataframe for tline analysis
    tline_sbp_df= make_ma_df(outcomes_df = outcomes_df,
                        rob = rob,
                        study_df = study_df,
                        #add in filters below to select subset of studies (e.g. filter())
                        outcome == "sbp",
                        cnap == "tline",
                        type == "invasive",
                        location == "radial" | location == "radialfemoral" | location == "radialfemoral" | location == "femoral"
    ),
    
    tline_dbp_df= make_ma_df(outcomes_df = outcomes_df,
                             rob = rob,
                             study_df = study_df,
                             #add in filters below to select subset of studies (e.g. filter())
                             outcome == "dbp",
                             cnap == "tline",
                             type == "invasive",
                             location == "radial" | location == "radialfemoral" | location == "radialfemoral" |location == "femoral"
    ),
    
    tline_map_df= make_ma_df(outcomes_df = outcomes_df,
                             rob = rob,
                             study_df = study_df,
                             #add in filters below to select subset of studies (e.g. filter())
                             outcome == "map",
                             cnap == "tline",
                             type == "invasive",
                             location == "radial" | location == "radialfemoral" | location == "radialfemoral" | location =="femoral"
    ),
    
    # meta-analysis for tline analysis
    tline_sbp_results = meta_analysis(tline_sbp_df),
    tline_dbp_results = meta_analysis(tline_dbp_df),
    tline_map_results = meta_analysis(tline_map_df),
   
   # making dataframe for VC analysis
   vc_sbp_df= make_ma_df(outcomes_df = outcomes_df,
                             rob = rob,
                             study_df = study_df,
                             #add in filters below to select subset of studies (e.g. filter())
                             cnap == "cnap" | cnap ==  "nexfin" | cnap == "finapres",
                             outcome == "sbp",
                             type == "invasive",
                             location == "radial" | location == "radialfemoral" | location == "radialfemoral"| location == "femoral",
   ),
  
   vc_dbp_df= make_ma_df(outcomes_df = outcomes_df,
                             rob = rob,
                             study_df = study_df,
                             #add in filters below to select subset of studies (e.g. filter())
                             cnap == "cnap" | cnap ==  "nexfin" | cnap == "finapres",
                             outcome == "dbp",
                             type == "invasive",
                             location == "radial" | location == "radialfemoral" | location == "radialfemoral" | location == "femoral"
   ),
   
   vc_map_df= make_ma_df(outcomes_df = outcomes_df,
                             rob = rob,
                             study_df = study_df,
                             #add in filters below to select subset of studies (e.g. filter())
                             cnap == "cnap" | cnap ==  "nexfin" | cnap == "finapres",
                             outcome == "map",
                             type == "invasive",
                             location == "radial" | location == "radialfemoral" | location == "radialfemoral" | location == "femoral",
                             case_when(study!="Ameloot 2014" ~ outcome == "map" & type == "invasive",
                                       study=="Ameloot 2014" ~ outcome == "map" & type == "invasive" & location == "femoral"
                             )
   ),
   
   # meta-analysis for VC analysis
   vc_sbp_results = meta_analysis(vc_sbp_df),
   vc_dbp_results = meta_analysis(vc_dbp_df),
   vc_map_results = meta_analysis(vc_map_df),
   
   # making dataframe for AAT analysis
   aat_sbp_df= make_ma_df(outcomes_df = outcomes_df,
                          rob = rob,
                          study_df = study_df,
                          #add in filters below to select subset of studies (e.g. filter())
                          cnap == "ncat" | cnap ==  "tline",
                          outcome == "sbp",
                          type == "invasive",
                          location == "radial" | location == "femoral"
   ),
  
   
   aat_dbp_df= make_ma_df(outcomes_df = outcomes_df,
                          rob = rob,
                          study_df = study_df,
                          #add in filters below to select subset of studies (e.g. filter())
                          cnap == "ncat" | cnap ==  "tline",
                          outcome == "dbp",
                          type == "invasive",
                          location == "radial" | location == "femoral"
   ),
  
   aat_map_df= make_ma_df(outcomes_df = outcomes_df,
                         rob = rob,
                         study_df = study_df,
                         #add in filters below to select subset of studies (e.g. filter())
                         cnap == "ncat" | cnap ==  "tline",
                         outcome == "map",
                         type == "invasive",
                         location == "radial" | location == "femoral"
   ),
  
   
   # meta-analysis for AAT analysis
   aat_sbp_results = meta_analysis(aat_sbp_df),
   aat_dbp_results = meta_analysis(aat_dbp_df),
   aat_map_results = meta_analysis(aat_map_df),
   
   # making dataframe for femoral analysis
   femoral_sbp_df= make_ma_df(outcomes_df = outcomes_df,
                              rob = rob,
                              study_df = study_df,
                              #add in filters below to select subset of studies (e.g. filter())
                              outcome == "sbp",
                              type == "invasive",
                              location == "femoral"
   ),
   
   femoral_dbp_df= make_ma_df(outcomes_df = outcomes_df,
                              rob = rob,
                              study_df = study_df,
                              #add in filters below to select subset of studies (e.g. filter())
                              outcome == "dbp",
                              type == "invasive",
                              location == "femoral"
   ),
   
   femoral_map_df= make_ma_df(outcomes_df = outcomes_df,
                              rob = rob,
                              study_df = study_df,
                              #add in filters below to select subset of studies (e.g. filter())
                              outcome == "map",
                              type == "invasive",
                              location == "femoral",
                              case_when(study!="Ameloot 2014" ~ outcome == "map" & type == "invasive",
                                        study=="Ameloot 2014" ~ outcome == "map" & type == "invasive" & location == "femoral"
                              )
   ),
   
   # meta-analysis for femoral analysis
   femoral_sbp_results = meta_analysis(femoral_sbp_df),
   femoral_dbp_results = meta_analysis(femoral_dbp_df),
   femoral_map_results = meta_analysis(femoral_map_df),
   
   # making dataframe for femoral analysis
   radial_sbp_df= make_ma_df(outcomes_df = outcomes_df,
                             rob = rob,
                             study_df = study_df,
                             #add in filters below to select subset of studies (e.g. filter())
                             outcome == "sbp",
                             type == "invasive",
                             location == "radial"
   ),
   
   radial_dbp_df= make_ma_df(outcomes_df = outcomes_df,
                             rob = rob,
                             study_df = study_df,
                             #add in filters below to select subset of studies (e.g. filter())
                             outcome == "dbp",
                             type == "invasive",
                             location == "radial"
   ),
   
   radial_map_df= make_ma_df(outcomes_df = outcomes_df,
                             rob = rob,
                             study_df = study_df,
                             #add in filters below to select subset of studies (e.g. filter())
                             outcome == "map",
                             type == "invasive",
                             location == "radial",
                             case_when(study!="Ameloot 2014" ~ outcome == "map" & type == "invasive",
                                       study=="Ameloot 2014" ~ outcome == "map" & type == "invasive" & location == "femoral"
                             )
   ),
   
   # meta-analysis for radial analysis
   radial_sbp_results = meta_analysis(radial_sbp_df),
   radial_dbp_results = meta_analysis(radial_dbp_df),
   radial_map_results = meta_analysis(radial_map_df),
   
   # Renders the manuscript - use drake::r_make() to render not knit
    manuscript_word = target(
      command = {
        rmarkdown::render(knitr_in("manuscript/index.Rmd"))
        file_out("manuscript/index.docx")
      }
    )
  )
}