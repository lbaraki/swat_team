# PROJECT: PEP Cascade
# PURPOSE: Munge and Analysis of Custom Indicators
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  70051a2e
# LICENSE: MIT
# DATE:   2024-06-06
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
    ref_id <- "70051a2e"
    
  # Functions  
  

# LOAD DATA ============================================================================  

    #df_msd <- read_psd(file_path) %>% 
     #clean_agency()
    
    df_pep <- read_csv("Data/FY24_Q2_PEP_ELIGIBLE.csv") %>%
      janitor::clean_names()

# MUNGE ============================================================================
  
  #Clean files - sort by disaggregates 
    
    #Gen Population
    pep_gen <- df_pep %>% 
      filter(fy == metadata$curr_fy_lab, 
             #indicator %in% c("PEP_ELGIBLE"), 
             standardizeddisaggregate == "Sex") %>% 
      mutate(fiscal_year = str_sub(fy, 3,4) %>% as.integer() + 2000) %>% #create fiscal_year column
      group_by(indicator, period, 
               #partner_name, sex, psnu
      ) %>% 
      summarise(value = sum(values,na.rm= T), .groups = "drop") 
    
    #Sex
    pep_sex <- df_pep %>%
      filter(fy == metadata$curr_fy_lab, 
             #indicator %in% c("PEP_ELGIBLE"), 
             standardizeddisaggregate == "Sex") %>% 
        mutate(fiscal_year = str_sub(fy, 3,4) %>% as.integer() + 2000) %>% #create fiscal_year column
      group_by(indicator, period, sex
               #partner_name, sex, psnu
               ) %>% 
      summarise(value = sum(values,na.rm= T), .groups = "drop") %>% 
      pivot_wider(names_from = sex, values_from = value) %>% 
      mutate(cumulative = Male + Female)
    
    #AGYW
    pep_agyw <- df_pep %>% #glimpse() names()
      filter(fy == metadata$curr_fy_lab, 
             #indicator %in% c("PEP_ELGIBLE"), 
             standardizeddisaggregate == "AGYW") %>% 
      mutate(fiscal_year = str_sub(fy, 3,4) %>% as.integer() + 2000) %>% #create fiscal_year column
      group_by(indicator, period, 
               #partner_name, psnu
      ) %>% 
      summarise(value = sum(values,na.rm= T), .groups = "drop")
  
# VIZ ============================================================================

  #PEP Cascade (screen, eligible, offered) by disaggregates 
    #Sex
    #ggplot2::annotate --> female(moody_blue) + male(genoa)
    
    pep_sex %>% 
      mutate(indc_order = fct_reorder(indicator, cumulative, .desc = TRUE)) %>% #sort by values
      ggplot(aes(x = indc_order, fill = cumulative)) + 
      #geom_col(fill = glitr::denim, alpha = 0.9, width = 0.65) + 
      geom_col(aes(y= Female + Male), fill = moody_blue) +
      geom_col(aes(y=Male), fill = genoa) +
      geom_text(aes(y = cumulative,
                    label = comma(cumulative)), size = 12/.pt,
                family = "Source Sans Pro", vjust = -.25) + 
      scale_y_continuous()+ 
      si_style_ygrid() + 
      theme(plot.title = ggtext::element_markdown())+
      labs(x = NULL, y = NULL,
           title = glue("{metadata$curr_pd} | PEP Cascade: <span style = 'color:{genoa}'>Males</span> and <span style = 'color:{moody_blue}'>Females</span>"),
           caption = glue::glue("GEND_GBV Custom Indicators | Ref id: {ref_id}"))
    
    si_save("Graphics/PEPCascade_Sex_FY24Q2.png")
    
    #AGYW
    pep_agyw %>% 
      mutate(indc_order = fct_reorder(indicator, value, .desc = TRUE)) %>% #sort by values
      ggplot(aes(x = indc_order, y = value)) + 
      geom_col(fill = glitr::denim, alpha = 0.9, width = 0.65) + 
      geom_text(aes(y = value,
                    label = comma(value)), size = 12/.pt,
                family = "Source Sans Pro", vjust = -.25) + 
      scale_y_continuous()+ 
      si_style_ygrid() + 
      labs(x = NULL, y = NULL,
           #subtitle = "AGYW is a subset of the Sex disaggregate",
           title = glue(" {metadata$curr_pd} | PEP Cascade: AGYW 15-29 yrs old"),
           caption = glue::glue("GEND_GBV Custom Indicators | Ref id: {ref_id}"))
    
    si_save("Graphics/PEPCascade_AGYW_FY24Q2.png")

# SPINDOWN ============================================================================

