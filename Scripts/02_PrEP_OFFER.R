# PROJECT: PrEP Cascade  
# PURPOSE: Munge and Analysis of Custom Indicators 
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  7a305f81
# LICENSE: MIT
# DATE:   2024-05-15
# NOTES:   

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(glitr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(systemfonts)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(glue)
    library(gt)
    library(gtExtras)
    library(cascade) 
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
      pattern = "PSNU_IM.*Eswatini")
      
  # Grab metadata
   metadata <- get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "7a305f81"
    
  # Functions  
    cntry_uid <- pepfar_country_list %>% 
      filter(country == "Eswatini") %>% 
      pull(country_uid)
    
# LOAD DATA ============================================================================  

  #df_genie <- read_csv("Data/Genie-SITE_IM-Eswatini_Clean.csv") %>% 
   #   janitor::clean_names()
    
  df_msd <- read_psd(file_path) %>% 
      clean_agency() #%>%  names()
      #filter(indicator %in% c("HTS_TST_NEG", "PrEP_CT", "PrEP_NEW")
        #funding_agency == "USAID"
       # ) %>% View()
    
  df_prep <- read_csv("Data/PrEP_OFFER_CI_Clean.csv") %>% janitor::clean_names()

# MUNGE ============================================================================
  
  prep_gen <- df_prep %>% 
    select(-sitename) %>% 
    filter(#results_or_targets == "quarterly results",
      fy_q == "FY24 Q2", 
      indicator %in% c("PREP_OFFER"), 
      standardizeddisaggregate == "Total Numerator") %>% 
    mutate(period = gsub(" ", "", fy_q), #create period column from fy_q 
           fiscal_year = str_sub(fy, 3,4) %>% as.integer() + 2000) %>%  #create fiscal_year column
    group_by(indicator, period, #orgunituid, partner_name
             ) %>%
    summarise(value = sum(values,na.rm= T), .groups = "drop") #collapse values across orgunituid and partner_name
  
  prep_men <- df_prep %>% 
    select(-sitename) %>% 
    filter(fy_q == "FY24 Q2", 
      indicator %in% c("PREP_OFFER"), 
      standardizeddisaggregate == "Age/Sex",
      sex == "Male",
      ageasentered %in% c("30-34")) %>% 
    mutate(period = gsub(" ", "", fy_q), #create period column from fy_q 
           fiscal_year = str_sub(fy, 3,4) %>% as.integer() + 2000) %>%  #create fiscal_year column
    group_by(indicator, period, 
             #ageasentered, sex
             ) %>%
    summarise(value = sum(values,na.rm= T), .groups = "drop") 
    
  prep_agyw <- df_prep %>% 
    select(-sitename) %>% 
    filter(fy_q == "FY24 Q2", 
      indicator %in% c("PREP_OFFER"), 
      standardizeddisaggregate == "Age/Sex",
      sex == "Female",
      ageasentered %in% c("15-19","20-24")) %>% 
    mutate(period = gsub(" ", "", fy_q), #create period column from fy_q 
           fiscal_year = str_sub(fy, 3,4) %>% as.integer() + 2000) %>%  #create fiscal_year column
    group_by(indicator, period, #partner_name
             #ageasentered, sex
             )%>%
    summarise(value = sum(values,na.rm= T), .groups = "drop") 
  
  
  prep_preg <- df_prep %>% 
    select(-sitename) %>% 
    filter(fy_q == "FY24 Q2", 
      indicator %in% c("PREP_OFFER"), 
      standardizeddisaggregate == "Sex/PregnantBreastfeeding",
      ) %>% 
    mutate(period = gsub(" ", "", fy_q), #create period column from fy_q 
           fiscal_year = str_sub(fy, 3,4) %>% as.integer() + 2000) %>%  #create fiscal_year column
    group_by(indicator, period,  #orgunituid, partner_name
             #ageasentered, sex
    )%>%
    summarise(value = sum(values,na.rm= T), .groups = "drop") 
  
  prep_sdc <- df_prep %>% 
    select(-sitename) %>% 
    filter(fy_q == "FY24 Q2", 
           indicator %in% c("PREP_OFFER"), 
           standardizeddisaggregate == "SDC",
    ) %>% 
    mutate(period = gsub(" ", "", fy_q), #create period column from fy_q 
           fiscal_year = str_sub(fy, 3,4) %>% as.integer() + 2000) %>%  #create fiscal_year column
    group_by(indicator, period, #orgunituid, partner_name
             #ageasentered, sex
    )%>%
    summarise(value = sum(values,na.rm= T), .groups = "drop") 
    
  
  #MSD
  #return_cascade(df_msd, 1)
  #return_cascade_plot(df_msd, F)
  
  msd_prep <- df_msd %>%
    filter(fiscal_year == metadata$curr_fy,
           indicator %in% c("HTS_TST_NEG", "PrEP_NEW", "PrEP_CT"),
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(indicator, fiscal_year, #standardizeddisaggregate, ageasentered, 
             ) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") 
  
  men_prep <- df_msd %>%
    filter(fiscal_year == metadata$curr_fy,
           indicator %in% c("HTS_TST_NEG", "PrEP_NEW", "PrEP_CT"),
           standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Age/Sex"), 
           sex == "Male",
           ageasentered %in% c("30-34")) %>% 
    group_by(indicator, fiscal_year, #standardizeddisaggregate, #ageasentered, sex 
    ) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") 
  
  agyw_prep <- df_msd %>%
    filter(fiscal_year == metadata$curr_fy,
           indicator %in% c("HTS_TST_NEG", "PrEP_NEW", "PrEP_CT"),
           standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Age/Sex"), 
           sex == "Female",
           ageasentered %in% c("15-19","20-24")) %>% 
    group_by(indicator, fiscal_year, #standardizeddisaggregate, ageasentered, sex
    ) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") 
  
  preg_prep <- df_msd %>%
    filter(fiscal_year == metadata$curr_fy,
           indicator %in% c(#"HTS_TST_NEG",
                            "PrEP_NEW", "PrEP_CT"),
           standardizeddisaggregate %in% c(
             "Sex/PregnantBreastfeeding")) %>% 
    group_by(indicator, fiscal_year, #ageasentered, #sex
    ) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") 
  
  
  #Reshape MSD's
  prep_cas <- msd_prep  %>% 
    reshape_msd(include_type = F) %>%  #creates period column
    filter(period == "FY24Q2") 
  
  prep_men_cas <- men_prep %>% 
    reshape_msd(include_type = F) %>% 
    filter(period == "FY24Q2")
  
  prep_agyw_cas <- agyw_prep %>%
    reshape_msd(include_type = F) %>% 
    filter(period == "FY24Q2")
  
  prep_preg_cas <- preg_prep %>% 
    reshape_msd(include_type = F) %>% 
    filter(period == "FY24Q2")
    

# VIZ ============================================================================
    
  #Append Data sets 
  combined_df <- 
    bind_rows(prep_cas, prep_gen) 
  
  combined_men_df <- 
    bind_rows(prep_men_cas, prep_men)
  
  combined_agyw_df <-
    bind_rows(prep_agyw_cas, prep_agyw)
  
  combined_preg_df <-
    bind_rows(prep_preg_cas, prep_preg)
  
  #PrEP Cascade by Population: snapshot of TST_NEG, PrEP_NEW, PrEP_CT
  #x-axis: indicators 
  #y-axis: value 
  
  #Gen Pop
    combined_df %>% 
    arrange(desc(value)) %>% 
    mutate(indc_order = fct_reorder(indicator, value, .desc = TRUE)) %>% #sort the indicators by values 
    ggplot(aes(x = indc_order, y = value)) + 
    geom_col(fill = glitr::denim, alpha = 0.9) + 
    geom_text(aes(y = value,
                  label = comma(value)), size = 12/.pt,
              family = "Source Sans Pro", vjust = -.25) + 
    scale_y_continuous()+ 
    si_style_ygrid()+
    labs(x = NULL, y = NULL,
         title = glue(" {metadata$curr_pd} | PrEP Cascade: General Population"),
         caption = glue::glue("PREP Custom Indicators & {metadata$source} |
                              Ref id: {ref_id}"))
    
    si_save("Graphics/PrEPCascade_GenPop_FY24Q2.png")
    
    
    #AGYW: 15-24 yrs old (ageasentered: "15-19" "20-24", standardizeddisaggregate ***)
  combined_agyw_df %>% 
    arrange(desc(value)) %>% 
    mutate(indc_order = fct_reorder(indicator, value, .desc = TRUE)) %>% #sort the indicators by values 
    ggplot(aes(x = indc_order, y = value)) + 
    geom_col(fill = glitr::denim, alpha = 0.9) + 
    geom_text(aes(y = value,
                  label = comma(value)), size = 12/.pt,
              family = "Source Sans Pro", vjust = -.25) + 
    scale_y_continuous()+ 
    si_style_ygrid()+
    labs(x = NULL, y = NULL,
         title = glue(" {metadata$curr_pd} | PrEP Cascade: AGYW 15-24 yrs old"),
         caption = glue::glue("PREP Custom Indicators & {metadata$source} |
                              Ref id: {ref_id}"))
  
  si_save("Graphics/PrEPCascade_AGYW_FY24Q2.png")
  
    #Men: 30-34 yrs old (ageasentered: "30-34", standardizeddisaggregate ***)
  combined_men_df %>% 
    arrange(desc(value)) %>% 
    mutate(indc_order = fct_reorder(indicator, value, .desc = TRUE)) %>% #sort the indicators by values 
    ggplot(aes(x = indc_order, y = value)) + 
    geom_col(fill = glitr::denim, alpha = 0.9) + 
    geom_text(aes(y = value,
                  label = comma(value)), size = 12/.pt,
              family = "Source Sans Pro", vjust = -.25) + 
    scale_y_continuous()+ 
    si_style_ygrid()+
    labs(x = NULL, y = NULL,
         title = glue(" {metadata$curr_pd} | PrEP Cascade: Men 30-34 yrs old"),
         caption = glue::glue("PREP Custom Indicators & {metadata$source} |
                              Ref id: {ref_id}"))
  
  si_save("Graphics/PrEPCascade_Men_FY24Q2.png")
  
    #PBFW - can't use HTS_NEG bc no PBWF disagg
  combined_preg_df %>% 
    arrange(desc(value)) %>% 
    mutate(indc_order = fct_reorder(indicator, value, .desc = TRUE)) %>% #sort the indicators by values 
    ggplot(aes(x = indc_order, y = value)) + 
    geom_col(fill = glitr::denim, alpha = 0.9) + 
    geom_text(aes(y = value,
                  label = comma(value)), size = 12/.pt,
              family = "Source Sans Pro", vjust = -.25) + 
    scale_y_continuous()+ 
    si_style_ygrid()+
    labs(x = NULL, y = NULL,
         title = glue(" {metadata$curr_pd} | PrEP Cascade: Pregnant & Breastfeeding Women"),
         caption = glue::glue("PREP Custom Indicators & {metadata$source} |
                              Ref id: {ref_id}"))
  
  si_save("Graphics/PrEPCascade_PBFW_FY24Q2.png")
  
  #SDC - solo since no SDC equivalent in the MSD (Subpop not Keypop)
  prep_sdc %>% 
    ggplot(aes(x = indicator, y = value)) + 
    geom_col(fill = glitr::denim, alpha = 0.9, width = 0.65) + 
    geom_text(aes(y = value,
                  label = comma(value)), size = 12/.pt,
              family = "Source Sans Pro", vjust = -.25) + 
    scale_y_continuous()+ 
    si_style_ygrid() + 
    labs(x = NULL, y = NULL,
         #subtitle = "SDC = seronegative persons in serodifferent partnerships (Non-KP)",
                         title = glue(" {metadata$curr_pd} | PrEP OFFER: Serodifferent Couples"),
                         caption = glue::glue("PREP Custom Indicators | Ref id: {ref_id} 
                         SDC = seronegative persons in serodifferent partnerships (Non-KP & not represented in MER)
                                              "))
  
  si_save("Graphics/PrEPOffer_SDC_FY24Q2.png")
    
  
# SPINDOWN ============================================================================

  #Reduce size of genie file 
  #genie_prep <- df_genie %>% 
  # filter(fy_q %in% c("FY24 Q2"), 
  #       ageasentered %in% c("15-19", "20-24","25-29",
  #                          30-34, "35-39", "40-44",
  #                         "45-49", "45-49", "50+"), 
  #indicator %in% c("HTS_TST_NEG", "PrEP_NEW", "PrEP_CT")) %>% 
  #select(fy, fy_q, indicator, standardizeddisaggregate, otherdisaggregate, otherdisaggregate_sub,
  #      orgunituid, psnu,
  #     partner_name, #sitename,
  #    ageasentered, sex, results_or_targets, values)
