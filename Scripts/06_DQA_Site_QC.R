# PROJECT: QC Tally Sheets 
# PURPOSE: Munge and Analysis of DQA Data 
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  c3157c1f
# LICENSE: MIT
# DATE:   2024-10-06
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
    file_path <- return_latest("Data", "Genie-SiteByIMs-Eswatini-Daily-2024-10-06.zip")
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "c3157c1f"
    
  # Functions  
  

# LOAD DATA ============================================================================  

    df_site <- read_psd(file_path) %>% 
      clean_agency() %>% 
      filter(funding_agency == "USAID",
        sitename %in% c("Mahwalala Clinic",
                             "Mgazini Nazarene clinic",
                             "Nyonyane Clinic"))

# MUNGE ============================================================================
  
  #FY24Q3 data for 3 clinics
      #include standard disaggs: Age/Sex/HIVStatus + Modality/Age/Sex/Result + Age/Sex/CD4/HIVStatus
    df_site %>% 
      filter(str_detect(sitename, "Mahwalala Clinic"),
             str_detect(psnu, "Hhohho"),
             sex == "Male",
             standardizeddisaggregate %in% c("Age/Sex/HIVStatus",
                                             "Modality/Age/Sex/Result",
                                             "Age/Sex/CD4/HIVStatus")
             ) %>%
      #distinct(sitename, indicator, sex,ageasentered,qtr3) %>% 
      group_by(sitename, indicator, standardizeddisaggregate, sex, ageasentered, qtr3) %>% 
      summarise(cumulative = sum(qtr3, na.rm = T), .groups = "drop") %>% 
      pivot_wider(names_from = indicator, 
                  values_from = qtr3) %>% 
      select(-cumulative) %>% 
      View()
      
      
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

