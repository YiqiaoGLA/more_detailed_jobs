  ################################################################################ 
  # 00. LOAD LIBRARIES AND SET UP PATHS
  ################################################################################ 
  

  library("here")
  library("tidyverse")
  library("ggplot2")
  library("ggthemes")
  library("nomisr")
  library("devtools")
  library("remotes")
  library("scales")
  library("gglaplot")
  library("data.table")
  library("janitor")
  library("lubridate")
  library("readr")
  library("ggrepel")
  library("plotly")
  library("magrittr")
  library("zoo")
  library("openxlsx")

  #.............................................................................
  
  ### Paths
  INPUT <- paste0(here("INPUT"),"/")
  INTERMEDIATE <- paste0(here("INTERMEDIATE"),"/")
  OUTPUT <- paste0(here("OUTPUT"),"/")
  
  ############################################################################## 
  # 01. IMPORT wide DATASETS
  ############################################################################## 
  
  # The SRS ONS team has declared that it is easier to clear data in wide format
  # rather than long. Hence we reshape those data to also have a long format
  
  #_____________________________________________________________________________
  # 01.1. 2018
  #_____________________________________________________________________________
  
  # !!-  This can be turned into a loop if more datasets are converted  -!!
  
  # Load data
  bres_2018_wide <- readxl::read_excel(path = here("INPUT","SRS outputs","20220217 AL Pub 1006953","Stata_output BRES2018_rev.xlsx"), sheet = "output_wide") %>%
    clean_names() 
  
  # Split into separate levels
  bres_2018_1 <- bres_2018_wide %>%
    select(section,employee_count_level_1) %>% 
    distinct( .keep_all = TRUE) %>% 
    pivot_longer(cols = starts_with("employee_count"),names_prefix = "employee_count_level_", names_to = "level_num",values_to = "employee_count") %>% 
    pivot_longer(cols = "section", names_to = "level_name",values_to = "name") 
  
  bres_2018_2 <- bres_2018_wide %>%
    select(division,employee_count_level_2) %>% 
    distinct( .keep_all = TRUE) %>% 
    pivot_longer(cols = starts_with("employee_count"),names_prefix = "employee_count_level_", names_to = "level_num",values_to = "employee_count") %>% 
    pivot_longer(cols = "division", names_to = "level_name",values_to = "name")
  
  bres_2018_3 <- bres_2018_wide %>%
    select(group,employee_count_level_3) %>% 
    distinct( .keep_all = TRUE) %>% 
    pivot_longer(cols = starts_with("employee_count"),names_prefix = "employee_count_level_", names_to = "level_num",values_to = "employee_count") %>% 
    pivot_longer(cols = "group", names_to = "level_name",values_to = "name")
  
  bres_2018_4 <- bres_2018_wide %>%
    select(class,employee_count_level_4) %>% 
    distinct( .keep_all = TRUE) %>% 
    pivot_longer(cols = starts_with("employee_count"),names_prefix = "employee_count_level_", names_to = "level_num",values_to = "employee_count") %>% 
    pivot_longer(cols = "class", names_to = "level_name",values_to = "name")
    
  # Combine into long format
  bres_2018_long <- bres_2018_1 %>% 
    rbind(bres_2018_2,bres_2018_3,bres_2018_4) %>% 
    relocate(level_name,level_num,name,employee_count)

  
  ############################################################################# 
  # 02. EXPORT EXCEL FILES WITH BOTH WIDE AND LONG
  ############################################################################## 
  
  #_____________________________________________________________________________
  # 02.1. 2018
  #_____________________________________________________________________________

  # Export all data to single workbook
  empty_frame <- c("")
  data2018_list <- list("Output->" = empty_frame,"output_wide" = bres_2018_wide, "output_long" = bres_2018_long)
  write.xlsx(data2018_list, file = here("INPUT","Stata_output BRES2018_rev.xlsx"), append=TRUE)
      