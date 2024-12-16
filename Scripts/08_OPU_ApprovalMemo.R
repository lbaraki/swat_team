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
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
      pattern = "PSNU_IM_FY22-25.*Eswatini")
      
  # Grab metadata
   meta <- get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "47ef6111"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  df_psnuim_old <- read_psd(file_path) %>% 
      filter(fiscal_year %in% c(2025)) %>% 
      clean_agency()

# MUNGE ============================================================================
    level_fac <- c( "HTS_RECENT","HTS_SELF", "HTS_TST", "HTS_TST_POS",
                    "PREP_NEW",
                    "TX_CURR","TX_NEW","TX_PVLS_D", "TX_PVLS")
  #Agency Table 
    
    agency_table <- df_psnuim_old %>% 
      clean_indicator() %>% 
      filter(indicator %in% c("TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS",
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
      arrange(factor(indicator, levels = level_fac)) %>% 
      View()
    
    #Partner Table
    
    partner_table <- df_psnuim_old %>% 
      clean_indicator() %>% 
      filter(fiscal_year == 2025,
             #funding_agency == "USAID",
            indicator %in% c(#"TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS"
                             "HTS_RECENT","HTS_SELF", "HTS_TST", "HTS_TST_POS",#"HTS_INDEX"
                             "PREP_NEW",
                             "TX_CURR","TX_NEW","TX_PVLS_D", "TX_PVLS"
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
      select(-cumulative) #%>% 
      #View()
    
      
      
  
# VIZ ============================================================================
    
    #Old PSNUxIM snapshot 
    partner_table %>% 
      filter(funding_agency == "USAID") %>%
      gt() %>% 
      gt_theme_nytimes()
    
    #New PSNUxIM snapshot
      #note: expect to see a shift in targets from FHI EpiC to TLC 

# SPINDOWN ============================================================================

