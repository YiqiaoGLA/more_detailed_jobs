  #_____________________________________________________________________________
  # 01. IMPORT wide DATASETS ----
  #_____________________________________________________________________________
  
  # The SRS ONS team has declared that it is easier to clear data in wide format
  # rather than long. Hence we reshape those data to also have a long format
  
  #*****************************************************************************
  ## Import and export ----
  #*****************************************************************************
  
  # !!-  This can be turned into a loop if more datasets are converted  -!!
  data_path_vec <- c("2018_rev"=paste0("SRS outputs\\","20220217 AL Pub 1006953\\","Stata_output BRES2018_rev.xlsx"),
                     "2020_rev_v2"=paste0("SRS outputs\\","20221220 NJ Pub 1006953\\More detailed jobs (13-12-22)\\","Stata_output BRES2020_rev_v2.xlsx"),
                     "2021_prov_v3"=paste0("SRS outputs\\","20221220 NJ Pub 1006953\\More detailed jobs (13-12-22)\\","Stata_output BRES2021_prov_v3.xlsx"))
  
  for (dta in names(data_path_vec)) {
    
    # Load data
    tmp_data_wide <- readxl::read_excel(path = here("INPUT",data_path_vec[dta]), sheet = "output_wide") %>%
      clean_names() 
    
    # Split into separate levels
    tmp_data_1 <- tmp_data_wide %>%
      select(section,employee_count_level_1) %>% 
      distinct( .keep_all = TRUE) %>% 
      pivot_longer(cols = starts_with("employee_count"),names_prefix = "employee_count_level_", names_to = "level_num",values_to = "employee_count") %>% 
      pivot_longer(cols = "section", names_to = "level_name",values_to = "name") 
    
    tmp_data_2 <- tmp_data_wide %>%
      select(division,employee_count_level_2) %>% 
      distinct( .keep_all = TRUE) %>% 
      pivot_longer(cols = starts_with("employee_count"),names_prefix = "employee_count_level_", names_to = "level_num",values_to = "employee_count") %>% 
      pivot_longer(cols = "division", names_to = "level_name",values_to = "name")
    
    tmp_data_3 <- tmp_data_wide %>%
      select(group,employee_count_level_3) %>% 
      distinct( .keep_all = TRUE) %>% 
      pivot_longer(cols = starts_with("employee_count"),names_prefix = "employee_count_level_", names_to = "level_num",values_to = "employee_count") %>% 
      pivot_longer(cols = "group", names_to = "level_name",values_to = "name")
    
    tmp_data_4 <- tmp_data_wide %>%
      select(class,employee_count_level_4) %>% 
      distinct( .keep_all = TRUE) %>% 
      pivot_longer(cols = starts_with("employee_count"),names_prefix = "employee_count_level_", names_to = "level_num",values_to = "employee_count") %>% 
      pivot_longer(cols = "class", names_to = "level_name",values_to = "name")
    
    # Combine into long format
    tmp_data_long <- tmp_data_1 %>% 
      rbind(tmp_data_2,tmp_data_3,tmp_data_4) %>% 
      relocate(level_name,level_num,name,employee_count)
    
    #...........................................................................
    # Export all data to single workbook
    #...........................................................................
    
    empty_frame <- c("")
    tmp_data_list <- list("Output->" = empty_frame,"output_wide" = tmp_data_wide, "output_long" = tmp_data_long)
    write.xlsx(tmp_data_list, file = here("INPUT",paste0("Stata_output BRES",dta,".xlsx")), append=TRUE)
    
    # Delete tmp files
    rm(tmp_data_1,tmp_data_2,tmp_data_3,tmp_data_4,tmp_data_long,tmp_data_wide,tmp_data_list)
    
  }
  
  #_____________________________________________________________________________
  # 02. Perform split of section G in WFJ data ----
  #_____________________________________________________________________________
  
  # The process involves several steps:
  
  # - Load raw WFJ SA data for each industry on quarterly basis
  # - Calculate the yearly average using the quarters for each sector
  # - Calculate the total employee jobs in each quarter EXCLUDING section T
  
  # - Load the WFJ NSA split data for section G
  # - Calculate the ratio split between G1 and G2 within that NSA dataset
  # - Merge with SA data and use the NSA ratios to calculate the split in G using SA data
  
  # - Format the data according to how exisiting script expects it

  
  # Load the full WFJ dataset for London
  wfj_stats_raw_mdj <- wfj_stats %>% 
    filter(geography_name=="London" & item_name=="employee jobs" & industry_name_simple!="Total" & measures_name=="Value") %>% 
    select(date_day,industry_code,industry_name_simple,obs_value) %>% 
    mutate(year=lubridate::year(date_day)) %>% 
    group_by(year,industry_code,industry_name_simple) %>% 
    summarise(obs_value_yr = mean(obs_value))
  
  # Make the totals excl. T
  wfj_stats_raw_mdj_totals <- wfj_stats_raw_mdj %>% 
    filter(industry_code!="T") %>% 
    mutate(industry_code = "Total (excl. T)",
           industry_name_simple = "Total (excl. T)") %>% 
    group_by(year,industry_code,industry_name_simple) %>% 
    summarise(obs_value_yr=sum(obs_value_yr))
  
  # Load the NSA split in G
  # NB: ADJUST THIS RANGE TO INCLUDE NEW YEARS 
  wfj_stats_mdj <- readxl::read_excel(path = paste0(INPUT,"londonjobseriesretailwholesaleempstatus1996to2021.xls"),
                                        sheet = "Section G, Emp SE breakdown",
                                        range="A4:AB10") %>%
    clean_names() %>% 
    rename(x2021=x20211) %>% 
    mutate(industry_name_simple = case_when(x2=="45+46: Wholesale and motor trades" ~ "Wholesale and motor repair",
                                            x2=="47: Retail trade, except of motor vehicles and motorcycles" ~ "Retail"),
           industry_code="G") %>% 
    select(industry_name_simple,industry_code,matches("x\\d{4}")) %>% 
    filter(industry_name_simple %in% c("Retail","Wholesale and motor repair")) %>% 
    pivot_longer(cols=matches("x\\d{4}"),names_to = "year",values_to = "obs_value_yr",names_prefix = "x") %>% 
    group_by(year) %>% 
    mutate(split_share = obs_value_yr/sum(obs_value_yr),
           year=as.numeric(year)) %>% 
    full_join(x=select(.,-c(obs_value_yr)),y=wfj_stats_raw_mdj,by=c("year","industry_code")) %>% 
    mutate(industry_name_simple=case_when(!is.na(industry_name_simple.x) ~ industry_name_simple.x,
                                            TRUE ~ industry_name_simple.y),
           obs_value_yr = case_when(!is.na(split_share) ~ obs_value_yr*split_share,
                                    TRUE ~ obs_value_yr),
           industry_code = case_when (industry_name_simple == "Retail" ~ "G1",
                                     industry_name_simple == "Wholesale and motor repair" ~ "G2",
                                     TRUE ~ industry_code )) %>% 
    select(-matches("industry_name_simple\\.\\w"),-split_share) %>% 
    rbind(wfj_stats_raw_mdj_totals)
  
  # The existing code uses a wide WFJ series, so comply with that
  wfj_stats_mdj_wide <- wfj_stats_mdj %>% 
    pivot_wider(names_from = year,values_from = obs_value_yr,names_prefix = "wfj_") %>% 
    arrange(industry_code)
  
    
  