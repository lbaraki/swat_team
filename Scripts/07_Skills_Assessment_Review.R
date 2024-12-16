# PROJECT: RHO TEC
# PURPOSE: Munge and Analysis of Skills Assessment Exercise
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  716fb92d
# LICENSE: MIT
# DATE:   2024-10-23
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
      
  # Grab metadata
   get_metadata()
  
  # REF ID for plots
    ref_id <- "716fb92d"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  #Load Skills Assessment
    skills_df <- read_excel("Data/Skills Assessment Test_SI_MEL RHO.xlsx", 3) %>% 
      janitor::clean_names()

# MUNGE ============================================================================
  
    #Create a clean dataframe 
    
    clean_df <- 
      skills_df %>% 
        #step 1: pivot long to get indicators in rows
      pivot_longer(cols = hts_tst_result: proxy_linkage,
                   names_to = "indicator") %>%
        #step 2: identify indicator types 
      mutate(indicator_type = case_when(
        str_detect(indicator,"result") ~ "result",
        str_detect(indicator, "target") ~ "target",
        TRUE ~ NA_character_
      )) %>% 
      arrange(year_quarter) %>% 
        #step 3: pivot quarters wide 
      select(-year_quarter) %>% 
      pivot_wider(names_from = quarter,
                  values_from = value) %>% 
      rename(targets = `NA`) %>% 
        #step 4: calculate cumulative results  
    mutate(cumulative = rowSums(across(starts_with("Q")), na.rm = TRUE)
           )
  
    final_df <- clean_df %>% 
      mutate(across(
        where(is.numeric),  # Apply to all numeric columns
        ~ case_when(
          indicator == "proxy_linkage" ~ round(., 2),  # Round to 2 decimal for proxy_linkage
          TRUE ~ round(., .2)  # Round to whole number for all other numeric values
        )
      )) 
        
View(final_df)

#Ask:
#Pivot table for testing and intiations by year/quarter
#HTS_TST (testing) & TX_NEW (initiations) by year and quarter 
skills_df %>% 
  select(fiscal_year, quarter, hts_tst_result, tx_new_result, facility) %>% 
  View()

pivot_table <- final_df %>% 
  filter(indicator %in% c("hts_tst_result", "tx_new_result")) %>% 
  select(indicator, fiscal_year, Q1:Q4, psnu, facility) %>% 
  pivot_longer(cols = Q1:Q4,
               names_to = "quarter") %>%
  group_by(indicator, fiscal_year, quarter) %>% 
  summarise(total = sum(value, na.rm = T), .groups = "drop") %>% 
  arrange(fiscal_year)

View(pivot_table)
  
# VIZ ============================================================================

  #District Performance: compare district-level (PSNU) of ART initiation in FY17
      #review distinct psnus and aggregate as needed 
    unique(final_df$psnu) #27 psnus (health districts)
    unique(final_df$facility) #~3.3k 
  
    
    #collapse data to PSNU level and summarize tx_new results
    df_bar <- 
    final_df %>% 
      filter(fiscal_year == "FY2017",
             indicator == "tx_new_result") %>% 
      summarise(TX_NEW = sum(cumulative, na.rm = T), .by = c("psnu")) %>% 
      arrange(desc(TX_NEW))
    
    #create a sorted bar graph
    df_bar %>% 
      mutate(psnu = str_remove_all(psnu, "Municipality"), 
             psnu = fct_reorder(psnu, TX_NEW)) %>% 
      ggplot(aes(x = TX_NEW, y = psnu)) + 
      geom_col(fill = glitr::burnt_sienna) + 
      labs(title = "DISTRICT PERFORMANCE",
        subtitle = "eThekwini District leads in ART initiations in FY17", 
           x = NULL, y = NULL) +
      scale_x_continuous(labels = comma) +
      #scale_y_discrete(labels = abbreviate) +
      si_style_xgrid()
    
      si_save("Images/District_Performance.png")
    
  #Proxy Linkage: Table of proxy linkage table for all facilities by district in FY18
      #quarter was not specified --> Q4 
      #create a district column (separate province vs district)
    link_df <- final_df %>% 
      select(-c(indicator_type,targets)) %>% 
      filter(fiscal_year =="FY2018",
             indicator == "proxy_linkage") %>% 
      pivot_longer(cols = Q1:Q4, 
                   names_to = "quarter",
                   values_to = "value") %>% 
      select(-cumulative) %>% 
      filter(quarter == "Q4")
    
    #link_tb <- 
        link_df %>%
          filter(str_starts(psnu, "nw")) %>% 
        mutate(psnu = str_remove_all(psnu, "Municipality"),
               value = value/100) %>% 
        arrange(desc(value)) %>% 
          group_by(psnu) %>% 
          slice_max(value, n = 5) %>% 
        gt(groupname_col = "psnu") %>% 
        cols_hide(c(indicator, fiscal_year, quarter)) %>% 
        gtExtras::gt_theme_nytimes() %>% 
        tab_options(data_row.padding = px(1),
                  table.font.size = px(10),
                  row_group.padding = px(1),
                  row_group.font.weight = "bold") %>% 
        tab_header(title = "PROXY LINKAGE",
                   subtitle = "North West | Top 5 facilities in FY18Q4") %>% 
        fmt_percent(columns = value, decimals = 1) %>% 
        gtsave_extra("Images/Proxy_Linkage.png")
    
  #Western Cape: HTS_TST vs HTS_TST_POS vs TX_NEW for 5 facilities in Western Cape in FY18
      #plot 5 facilities with largest ART treatment share (TX_CURR)
    wc_df <- 
    final_df %>% 
      select(-contains("Q"), -targets) %>% 
      filter(fiscal_year == "FY2018", 
             str_starts(psnu, "wc"),
             indicator_type == "result") %>% 
      pivot_wider(names_from = indicator,
                  values_from = cumulative) %>% 
      arrange(desc(tx_curr_result)) 
    
    
    indic_order <- c("tx_new_result","hts_tst_pos_result","hts_tst_result")
    fac_order <- c("Facility 3260", "Facility 3299", "Facility 3214",
                   "Facility 3291", "Facility 3224")
      
    wc_df %>% 
      select(-c(fiscal_year, indicator_type)) %>% 
      slice_max(tx_curr_result, n = 5) %>% #filtered for 5 facilities 
      pivot_longer(cols = hts_tst_result:tx_new_result, 
                   names_to = "indicator") %>% #pivot indicators long again
      mutate(indic_val = factor(indicator, levels = indic_order),
             facility = factor(facility, levels = fac_order)) %>% 
      filter(indicator %ni% ("tx_curr_result")) %>% 
      ggplot(aes(y = indic_val, x = value)) +
        geom_col(aes(fill = indic_val)) + 
      facet_wrap(~ facility, nrow = 2, scales = "free_x") +
      scale_fill_si(palette = "tango_d", discrete = T) + 
      scale_x_continuous(labels = comma) +
      scale_y_discrete(labels = c("TX_NEW","HTS_TST_POS","HTS_TST")) + 
      si_style_xgrid() + 
      theme(legend.position = "none") + 
      labs(title = "WESTERN CAPE",
           subtitle = "Testing & treatment results for 5 facilities with largest TX_CURR in FY18",
           x = NULL, y = NULL)
    
    si_save("Images/Western_Cape.png")
      
    
  #TX_CURR Performance: Results vs Targets for TX_CURR in Eastern Cape by facility (plot bottom 10)
    df_txcurr <-
      final_df %>% 
        select(-contains("Q")) %>% 
      filter(str_starts(psnu, "ec"),
             indicator %in% c("tx_curr_result", "tx_curr_target")) %>% 
        mutate(indicator = "tx_curr") %>% 
        pivot_wider(names_from = indicator_type, 
                    values_from = c(targets, cumulative),
                    names_prefix = "indicator_") %>% 
        rename(targets = targets_indicator_target,
               cumulative = cumulative_indicator_result) %>% 
        select(-c(targets_indicator_result, cumulative_indicator_target)) %>%
        mutate(achv = cumulative/targets) %>%
        filter(!is.na(achv) & achv !=0) %>% 
        arrange(achv)
      
      
      #stacked bar graph showing proportions 
    df_txcurr %>% 
      filter(fiscal_year == "FY2018")%>% #need to specify a year 
      slice_min(achv, n = 10) %>% #bottom 10 
      mutate(benchmark = .5) %>% 
      mutate(fac_achv = fct_reorder(facility, achv, .desc = T)) %>%  #sort the facility by achv
      ggplot(aes(x = indicator, y = achv)) + 
      geom_col(aes(y = benchmark), fill = grey20k, alpha = 0.5) + 
      geom_col(fill = glitr::midnight_blue)+ 
      geom_hline(yintercept = 1, color = grey40k, linetype = "dotted") + 
      geom_text(aes(label = percent(achv, 1)),
                vjust = -.25) + 
      facet_wrap(~fac_achv, ncol = 5,
                 nrow = 2) + 
      si_style_ygrid(facet_space = 0.5) + 
      scale_y_continuous(labels = percent, limits = c(0,.5)) +
      theme(axis.text.x = element_blank()) + 
      labs(x = NULL, y = NULL, 
           title = str_to_upper("TX_CURR Performance"),
           subtitle = "Bottom 10 performing facilities in the Eastern Cape from FY18")
    
    si_save("Images/TX_CURR_Performance.png")
      

# SPINDOWN ============================================================================

    
    