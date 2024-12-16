# PROJECT: DQA Review
# PURPOSE: Munge and Analysis of Site Level Data
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  cb373d60
# LICENSE: MIT
# DATE:   2024-08-30
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
                               pattern = "Site_IM_FY22-25.*Eswatini")
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "cb373d60"
    
  # Functions  
  

# LOAD DATA ============================================================================  

    df_site <- read_psd(file_path) %>% 
      clean_agency() %>% 
      filter(indicator %in% c("TX_CURR"))
    
    glimpse(df_site)
    names(df_site)

# MUNGE ============================================================================
  
  #How many USAID sites for TX_CURR? 
  #How many clients at target sites? 
  #How many clients with target IPs? (EGPAF, TLC)
    
    limit_df <- 
      df_site %>%
      filter(funding_agency == "USAID", 
             fiscal_year == 2024,
             standardizeddisaggregate == "Age/Sex/HIVStatus"
      ) %>% 
      group_by(fiscal_year, indicator, standardizeddisaggregate, funding_agency, sitename, orgunituid) %>% 
      summarise(across(c("cumulative"), sum, na.rm = TRUE),
                .groups = "drop") %>%
      count(sitename, cumulative) 
    
    unique(limit_df$funding_agency)
    unique(limit_df$sitename)
  
    
    
    
    df_site %>% 
      filter(funding_agency == "USAID", 
             fiscal_year == 2024,
             #str_detect(prime_partner_name, "Elizabeth Glaser Pediatric Aids Foundation"),
             #str_detect(psnu, "***")
      ) %>% 
      distinct(orgunituid, sitename) %>% View()
    
    df_site %>% 
      filter(funding_agency == "USAID", 
             #orgunituid == "",
             prime_partner_name == "Elizabeth Glaser Pediatric Aids Foundation",
             fiscal_year %in% c(2024)) %>% 
      count(sitename, orgunituid)


# SPINDOWN ============================================================================

