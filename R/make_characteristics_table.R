##' @title Table of study characteristics
##' @name make_characteristics_table
##' @param study_df data frame
##' @param baseline_df data frame
##' @param outcomes_df data frame
##' @return
##' @export
##' 
#' @importFrom countrycode countrycode
#' @importFrom dplyr rename mutate case_when select 
#' right_join arrange
#' @importFrom flagon flags
#' @importFrom flextable flextable compose 
#' as_paragraph minibar as_image lollipop merge_v 
#' merge_at fontsize fix_border_issues add_header_row 
#' align bg color empty_blanks set_header_labels 
#' border_remove hline hline_bottom autofit
#' @importFrom glue glue
#' @importFrom officer fp_border
#' @importFrom tidyr pivot_wider separate
#' @importFrom here here

make_characteristics_table <- function(study_df, baseline_df, outcomes_df) {


  
  baseline <-   baseline_df %>%
    pivot_wider(names_from = c(characteristic,
                               measure
    )) %>% 
    rename( sample = sample_n,
            female = female_n,
            male = male_n)

data <-   outcomes_df %>%
    pivot_wider(names_from = c(outcome,
                               measure
    )) %>% 
      mutate(measurements = case_when(
        is.na(sbp_measurements & dbp_measurements) ~ map_measurements,
        TRUE ~ sbp_measurements
      ),
            participants = case_when(
              is.na(sbp_participants & dbp_participants) ~ map_participants,
              TRUE ~ sbp_participants
            )) %>% 
    select(study, cnap, type, location, group, participants, measurements) %>% 
    right_join(study_df) %>% 
    select(-sponsor) %>% 
    right_join(baseline) %>% 
  mutate(code = countrycode(country, origin = 'country.name', destination = 'iso3c') 
)
    
    


    
  
  ft <- data %>% 
    # bind_rows(data_SL) %>%
    #   bind_rows(data_NPA) %>%
    arrange(study) %>% 
    separate(study, c("Study", "Year"), sep = " ")  %>% 
    arrange(-as.numeric(Year)) %>% 
    mutate(perc_male = round((male/(female+male))*100, 1)) %>% 
    mutate(age = case_when(
      !is.na(age_mean) & !is.na(age_sd)~ glue("{age_mean} ({age_sd})"),
      !is.na(age_mean) & !is.na(age_min)~ glue("{age_mean} [{age_min} to {age_max}]"),
      !is.na(age_median) & !is.na(age_min)~ glue("{age_median} [{age_min} to {age_max}]"),
      is.na(age_median) & is.na(age_median) & !is.na(age_min)~ glue("[{age_min} to {age_max}]"),
      !is.na(age_median) & !is.na(age_loweriqr) ~ glue("{age_median} [{age_loweriqr}, {age_upperiqr})"),
      !is.na(age_mean) & !is.na(age_loweriqr) ~ glue("{age_mean} [{age_loweriqr}, {age_upperiqr})")
      ),
                  sbp = case_when(
      !is.na(sbp_mean) & !is.na(sbp_sd)~ glue("{sbp_mean} ({sbp_sd})")
                  ),
      dbp = case_when(
        !is.na(dbp_mean) & !is.na(dbp_sd)~ glue("{dbp_mean} ({dbp_sd})")
      ),
      map = case_when(
        !is.na(map_mean) & !is.na(map_sd)~ glue("{map_mean} ({map_sd})")
      )) %>% 
    mutate(n1=participants) %>% 
    mutate(N1=measurements) %>% 
    select(Year, Study, code,  perc_male, age, group, 
                  setting, cnap, type, location, participants,n1,
           # didn't include columns for sbp, dbp and map because 
           # it takes up a lot of room and only 4 studies reported anyway
                  measurements,N1, -sbp, -dbp, -map) %>%
    mutate(group = case_when(group == "primary" ~ "",
                             group == "afib" ~ "Atrial fibrillation",
                             group == "sr" ~ "Sinus Rhythm")) %>% 
    flextable() %>% 
    compose( j="perc_male", value = as_paragraph(
    minibar(value =   perc_male , max=100,
            barcol = "#8edaff",
            bg = "#ff8ea2",)
  ),
  part = "body") %>% 
  
  compose(i=~code=="AUS", j = "code",
                value = as_paragraph(as_image( 
                  src = flags("AU"),
                  width = .20, height = .15))
  ) %>% 
  
  compose( i=~code=="NLD", j = "code",
                value = as_paragraph(as_image( 
                  src = flags("NL"),
                  width = .20, height = .15))
  ) %>% 
  
   compose( i=~code=="DEU", j = "code",
                value = as_paragraph(as_image( 
                  src = flags("DE"),
                  width = .20, height = .15))
  ) %>% 
  
  compose(i=~code=="FRA", j = "code",
                value = as_paragraph(as_image( 
                  src = flags("FR"), 
                  width = .20, height = .15))
  ) %>% 
  
  compose(i=~code=="AUT", j = "code",
                value = as_paragraph(as_image( 
                  src = flags("AT"), 
                  width = .20, height = .15))
  ) %>% 
  
  compose(i=~code=="BEL", j = "code",
                value = as_paragraph(as_image( 
                  src = flags("BE"), 
                  width = .20, height = .15))
  ) %>% 
  
  
  compose( i=~code=="CAN", j = "code",
                value = as_paragraph(as_image( 
                  src = flags("CA"), 
                  width = .20, height = .15))
  ) %>% 
  
  compose(j = "n1",   value = as_paragraph(
    minibar(value = participants, barcol = "#930093")
  ),
  part = "body") %>% 
  
  compose(j = "N1",   value = as_paragraph(
    lollipop(rangecol = "white",positivecol ="#930093", value = log(measurements), 
             min=0, max=log(max(measurements)))
  ),
  part = "body") %>% 
  
  compose(j = "perc_male",
                value = as_paragraph(as_image(
                  src = here("inst/sex.png"),
                  width = .4, height = .3)),
                part="header"
  ) %>% 
  
    merge_v(part = "body", j = 1:2) %>% 
  
    merge_at(part = "body", i=~Study=="Berkelmans", j = "code") %>% 
   merge_at(part = "body", i=~Study=="Berkelmans", j = "setting") %>% 
  merge_at(part = "body", i=~Study=="Berkelmans", j = "cnap") %>% 
  merge_at(part = "body", i=~Study=="Berkelmans", j = "type") %>% 
  merge_at(part = "body", i=~Study=="Berkelmans", j = "location") %>% 

  merge_at(part = "body", i=~Study=="Lakhal", j = "code") %>% 
    merge_at(part = "body", i=~Study=="Lakhal", j = "perc_male") %>%
    merge_at(part = "body", i=~Study=="Lakhal", j = "age") %>% 
    merge_at(part = "body", i=~Study=="Lakhal", j = "cnap") %>% 
    merge_at(part = "body", i=~Study=="Lakhal", j = "group") %>% 
    merge_at(part = "body", i=~Study=="Lakhal", j = "setting") %>% 
    merge_at(part = "body", i=~Study=="Lakhal", j = "participants") %>% 
    merge_at(part = "body", i=~Study=="Lakhal", j = "n1") %>% 
    merge_at(part = "body", i=~Study=="Lakhal", j = "measurements") %>% 
    merge_at(part = "body", i=~Study=="Lakhal", j = "N1") %>% 
    # merge_at(part = "body", i=~Study=="Lakhal", j = "sbp") %>% 
    # merge_at(part = "body", i=~Study=="Lakhal", j = "dbp") %>% 
    # merge_at(part = "body", i=~Study=="Lakhal", j = "map")  %>% 
    
    
    merge_at(part = "body", i=~Study=="Ameloot", j = "code") %>% 
    merge_at(part = "body", i=~Study=="Ameloot", j = "perc_male") %>%
    merge_at(part = "body", i=~Study=="Ameloot", j = "age") %>% 
    merge_at(part = "body", i=~Study=="Ameloot", j = "cnap") %>% 
    merge_at(part = "body", i=~Study=="Ameloot", j = "group") %>% 
    merge_at(part = "body", i=~Study=="Ameloot", j = "setting") %>% 
    # merge_at(part = "body", i=~Study=="Ameloot", j = "sbp") %>% 
    # merge_at(part = "body", i=~Study=="Ameloot", j = "dbp") %>% 
    # merge_at(part = "body", i=~Study=="Ameloot", j = "map")  %>% 
    
    merge_at(part = "body", i=~Study=="Hofhuizen", j = "code") %>% 
    merge_at(part = "body", i=~Study=="Hofhuizen", j = "perc_male") %>%
    merge_at(part = "body", i=~Study=="Hofhuizen", j = "age") %>% 
    merge_at(part = "body", i=~Study=="Hofhuizen", j = "cnap") %>%
    merge_at(part = "body", i=~Study=="Hofhuizen", j = "type") %>% 
    
    merge_at(part = "body", i=~Study=="Hofhuizen", j = "group") %>% 
    merge_at(part = "body", i=~Study=="Hofhuizen", j = "setting")  %>% 
    merge_at(part = "body", i=~Study=="Hofhuizen", j = "participants") %>% 
    merge_at(part = "body", i=~Study=="Hofhuizen", j = "n1") %>% 
    merge_at(part = "body", i=~Study=="Hofhuizen", j = "measurements") %>% 
    merge_at(part = "body", i=~Study=="Hofhuizen", j = "N1") %>% 
  
    merge_at(part = "body", i=~Study=="Martina", j = "code") %>% 
    merge_at(part = "body", i=~Study=="Martina", j = "perc_male") %>%
    merge_at(part = "body", i=~Study=="Martina", j = "age") %>% 
    merge_at(part = "body", i=~Study=="Martina", j = "cnap") %>% 
    merge_at(part = "body", i=~Study=="Martina", j = "type") %>% 
    merge_at(part = "body", i=~Study=="Martina", j = "group") %>% 
    merge_at(part = "body", i=~Study=="Martina", j = "setting")  %>% 
    merge_at(part = "body", i=~Study=="Martina", j = "participants") %>% 
    merge_at(part = "body", i=~Study=="Martina", j = "n1") %>% 
    merge_at(part = "body", i=~Study=="Martina", j = "measurements") %>% 
    merge_at(part = "body", i=~Study=="Martina", j = "N1")  %>% 
    
    merge_at(part = "body", i=~Study=="Meidert" & Year==2014, j = "code") %>% 
    merge_at(part = "body", i=~Study=="Meidert" & Year==2014, j = "perc_male") %>%
    merge_at(part = "body", i=~Study=="Meidert" & Year==2014, j = "age") %>% 
    merge_at(part = "body", i=~Study=="Meidert" & Year==2014, j = "type") %>% 
    merge_at(part = "body", i=~Study=="Meidert" & Year==2014, j = "group") %>% 
    merge_at(part = "body", i=~Study=="Meidert" & Year==2014, j = "setting")  %>% 
    merge_at(part = "body", i=~Study=="Meidert" & Year==2014, j = "participants") %>% 
    merge_at(part = "body", i=~Study=="Meidert" & Year==2014, j = "n1") %>% 
    merge_at(part = "body", i=~Study=="Meidert" & Year==2014, j = "measurements") %>% 
    merge_at(part = "body", i=~Study=="Meidert" & Year==2014, j = "N1") %>% 
  
    fontsize(size = 10) %>% 
    fix_border_issues() %>% 
    add_header_row(values = c("Study","Participants", "Blood pressure measurements"),  
                   colwidths = c(3, 4 , 7)) %>% 
    align(align = "center", part = "header") %>% 
    bg(i=1, j=1:3, part="header", bg="#0080ff") %>% 
    color(i=1, j=1:3, part="header", color="white") %>% 
    bg(i=1, j=4:7, part="header", bg="#ee721a") %>% 
    color(i=1, j=4:7, part="header", color="white")  %>% 
    bg(i=1, j=8:14, part="header", bg="#930093") %>% 
    color(i=1, j=8:14, part="header", color="white")  %>% 
    empty_blanks() %>% 
    set_header_labels(Year ="", Study = "", code = "",
                      setting = "", age= "Age (years)",
                      cnap = "CNAP device", 
                      type = "Comparator",
                      location = "", n1 = "Participants",
                      N1 = "Measurements", group = "",
                      participants="", measurements="") %>% 
    border_remove()  %>% 
    hline(i =~Year=="2018", part = "body", 
          border = fp_border(color="black", width = 1)) %>% 
    
    hline( i =~Year=="2016", part = "body", 
           border = fp_border(color="black", width = 1)) %>% 
    
    hline(i =~Year=="2015", part = "body", 
          border = fp_border(color="black", width = 1)) %>% 
    
    hline( i =~Year=="2014", part = "body", 
           border = fp_border(color="black", width = 1)) %>% 
    
    hline(i =~Year=="2013", part = "body", 
          border = fp_border(color="black", width = 1)) %>% 
    
    hline( i =~Year=="2012", part = "body", 
           border = fp_border(color="black", width = 1))  %>%
    hline( i =~Year=="1994", part = "body", 
           border = fp_border(color="black", width = 1))  %>% 
    hline( i =~Year=="1993", part = "body", 
           border = fp_border(color="black", width = 1))  %>% 
    bg(j=11:14, bg="lightgray") %>% 
    hline( i =2, part = "header", 
           border = fp_border(color="black", width = 1)) %>% 
    hline_bottom(part = "body", 
                 border = fp_border(color="black", width = 1))  %>% 
    fix_border_issues() %>% 
  
autofit(
    # use these width functions to set specific column widths for the 
  # word document
  # %>% 
  #   width(j="Year",width = 0.2) %>%
  #   width(j="Study",width = 0.20)  %>%
  #   width(j="code",width = 0.10)  %>%
  #   width(j="perc_male",width = 0.80) %>%
  #   width(j="age",width = 0.75) %>%
  #   width(j="patients",width = 0.30)  %>%
  #   width(j="comparison",width = 0.50) %>%
  #   width(j="n",width = 0.10) %>%
  #   width(j="n1",width = 0.70) %>%
  #   width(j="N",width = 0.30) %>%
  #   width(j="N1",width = 0.6) %>% 
  #   footnote(i =2, j = 6,
  #            value = as_paragraph(
  #              "mean (standard deviation) or median [interquartile range]"
  #            ),
  #            ref_symbols = "a",
  #            part = "header")
  #
) 
}