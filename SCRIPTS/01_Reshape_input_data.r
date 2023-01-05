  ############################################################################## 
  # 01. IMPORT wide DATASETS
  ############################################################################## 
  
  # The SRS ONS team has declared that it is easier to clear data in wide format
  # rather than long. Hence we reshape those data to also have a long format
  
  #_____________________________________________________________________________
  # 01.1. 2018
  #_____________________________________________________________________________
  
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
  