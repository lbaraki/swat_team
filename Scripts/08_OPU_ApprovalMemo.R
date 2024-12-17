# PROJECT: OPU Approval Memo
# PURPOSE: Munge and Analysis of Targets Shifts
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  47ef6111
# LICENSE: MIT
# DATE:   2024-12-16
# NOTES:   

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
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
    library(readxl)
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
      pattern = "PSNU_IM_FY22-25.*Eswatini")
    
    old_psnuim_filepath <- glamr::return_latest("Data", "updated 2024")
    psnuim_filepath <- glamr::return_latest("Data", "PSNUxIM_Eswatini")
      
  # Grab metadata
   meta <- get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "47ef6111"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  df_psnuim_old <- read_psd(file_path) %>% 
      filter(fiscal_year %in% c(2025)) %>% 
      clean_agency()
    
    #df_psnuim_new <- read_excel("Data/PSNUxIM_Eswatini_20240307_172957_OPU 20241212.xlsx", 2)
    
    df_old_dp_mech <- tame_dp(old_psnuim_filepath, type = "PSNUxIM", map_names = F)
    df_dp_mech <- tame_dp(psnuim_filepath, type = "PSNUxIM", map_names = F)

# MUNGE ============================================================================
    level_fac <- c( "HTS_RECENT","HTS_SELF", "HTS_TST", "HTS_TST_POS",
                    "PrEP_NEW",
                    "TX_CURR","TX_NEW",#"TX_PVLS_D",
                    "TX_PVLS")
  #Agency Table 
    
    agency_table <- df_psnuim_old %>% 
      clean_indicator() %>% 
      filter(standardizeddisaggregate == "Total Numerator",
        indicator %in% c("TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS",
                               "HTS_SELF", "HTS_TST", "HTS_TST_POS","HTS_RECENT", "HTS_INDEX", 
                               "TB_STAT", "TB_ART",
                               "TB_PREV", "TX_TB_D",
                               "PrEP_NEW", "PrEP_CT")) %>% 
      mutate(trendscoarse = case_when(ageasentered %in% c("<01", "01-09", "10-14") ~ "<15",
                                      ageasentered %in% c("15-24", "25-34", "35-49", "50+") ~ "15+",
                                      TRUE ~ ageasentered)) %>% 
      group_by(fiscal_year, indicator, #trendscoarse
               funding_agency) %>% 
      summarise(across(c(targets), sum, na.rm = TRUE)) %>% 
      ungroup() %>% 
      pivot_wider(names_from = "funding_agency", values_from = "targets") %>% 
      arrange(factor(indicator, levels = level_fac)) 
    
      View(agency_table)
    
    #Partner Table
      #MSD from Pano
    partner_table <- df_psnuim_old %>% 
      #clean_indicator() %>% 
      filter(fiscal_year == 2025,
             standardizeddisaggregate %in% c("Total Denominator","Total Numerator"),
             #funding_agency == "USAID",
            indicator %in% c(#"TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS"
                             "HTS_RECENT","HTS_SELF", "HTS_TST", "HTS_TST_POS",#"HTS_INDEX"
                             "PrEP_NEW",
                             "TX_CURR","TX_NEW", "TX_PVLS"
           )) %>% 
      #mutate(trendscoarse = case_when(ageasentered %in% c("<01", "01-09", "10-14") ~ "<15",
       #                              ageasentered %in% c("15-24", "25-34", "35-49", "50+") ~ "15+",
        #                            TRUE ~ ageasentered)) %>% 
      group_by(indicator ,#trendscoarse,
               funding_agency, prime_partner_name, mech_code) %>% 
      summarise(across(c(targets, cumulative), sum, na.rm = TRUE)) %>% 
      arrange(factor(indicator, levels = level_fac)) %>%
      ungroup() %>% 
      #unite("indicator", indicator:trendscoarse, sep = "_", na.rm =  TRUE) %>% 
      pivot_wider(names_from = "indicator", values_from = "targets") %>% 
      select(-cumulative)
    
    View(partner_table)
    
      #PSNUxIM tool submission from March
    old_partner_table <- df_old_dp_mech %>% 
      #clean_indicator() %>% 
      filter(fiscal_year == 2025,
             #funding_agency == "USAID",
             str_detect(standardizeddisaggregate, "KeyPop", negate = T),
             indicator %in% c(#"TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS"
               "HTS_RECENT","HTS_SELF", "HTS_TST", "HTS_TST_POS",#"HTS_INDEX"
               "PrEP_NEW",
               "TX_CURR","TX_NEW", "TX_PVLS"
             )) %>% 
      #mutate(trendscoarse = case_when(ageasentered %in% c("<01", "01-09", "10-14") ~ "<15",
       #                               ageasentered %in% c("15-24", "25-34", "35-49", "50+") ~ "15+",
        #                              TRUE ~ ageasentered)) %>% 
      group_by(indicator,
               funding_agency, prime_partner_name, mech_code) %>% 
      summarise(across(c(targets, cumulative), sum, na.rm = TRUE)) %>% 
      arrange(factor(indicator, levels = level_fac)) %>%
      ungroup() %>% 
      #unite("indicator", indicator:trendscoarse, sep = "_", na.rm =  TRUE) %>% 
      pivot_wider(names_from = "indicator", values_from = "targets") %>% 
      select(-cumulative)
    
    View(old_partner_table)
    
      
  #new partner table from upated PSNUxIM tool in December 
   new_partner_table <- df_dp_mech %>% 
      #clean_indicator() %>% 
      filter(fiscal_year == 2025,
             #funding_agency == "USAID",
             str_detect(standardizeddisaggregate, "KeyPop", negate = T),
             indicator %in% c(#"TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS"
               "HTS_RECENT","HTS_SELF", "HTS_TST", "HTS_TST_POS",#"HTS_INDEX"
               "PrEP_NEW",
               "TX_CURR","TX_NEW", "TX_PVLS"
             )) %>% 
      mutate(trendscoarse = case_when(ageasentered %in% c("<01", "01-09", "10-14") ~ "<15",
                                      ageasentered %in% c("15-24", "25-34", "35-49", "50+") ~ "15+",
                                      TRUE ~ ageasentered)) %>% 
      group_by(indicator, #trendscoarse,
               funding_agency, prime_partner_name, mech_code) %>% 
      summarise(across(c(targets, cumulative), sum, na.rm = TRUE)) %>% 
      arrange(factor(indicator, levels = level_fac)) %>%
      ungroup() %>% 
      #unite("indicator", indicator:trendscoarse, sep = "_", na.rm =  TRUE) %>% 
      pivot_wider(names_from = "indicator", values_from = "targets") %>% 
      select(-cumulative)
  
   View(new_partner_table)
# VIZ ============================================================================
    
    #Old PSNUxIM snapshot from Pano
    partner_table %>% 
      filter(funding_agency == "USAID",
             #mech_code %in% c(81928, 85553)
             ) %>%
     arrange(desc(mech_code)) %>% 
      #slice(2:3) %>% 
      gt() %>% 
     tab_style(
       style = list(
         cell_fill(color = "yellow"),
         cell_text(weight = "bold")),
       locations = cells_body(
         rows = mech_code%in% c(85553,81928)
       )) %>% 
      gt_theme_nytimes()
    
    #Old vs New PSNUxIM snapshot from tools 
      #note: expect to see a shift in targets from FHI EpiC to TLC 
   
   
   old_partner_table %>% 
     arrange(desc(mech_code)) %>%
     filter(mech_code %in% c(85555, 85553, 81935,81932, 81928, 16204)) %>% 
     gt() %>% 
     tab_style(
       style = list(
         cell_fill(color = "yellow"),
         cell_text(weight = "bold")),
       locations = cells_body(
         rows = mech_code%in% c(85553,81928)
       )) %>% 
     gt_theme_nytimes() 
   
   new_partner_table %>% 
     arrange(desc(mech_code)) %>%
     filter(mech_code %in% c(85555, 85553, 81935,81932, 81928, 16204)) %>% 
     gt() %>% 
     tab_style(
       style = list(
         cell_fill(color = "yellow"),
         cell_text(weight = "bold")),
       locations = cells_body(
         rows = mech_code%in% c(85553)
       )) %>% 
     gt_theme_nytimes() 
# SPINDOWN ============================================================================

