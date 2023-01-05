  #_____________________________________________________________________________
  # 01. IMPORT DATASETS ----
  #_____________________________________________________________________________
  
  # NOTES: 
  # Newer series have aggregated classes to correspond to old codes. This applies to:
  # 172_39	Sum of 1723 and 1729
  # 282_49	Sum of 2824 and 2829
  # 284_19	Sum of 2841 and 2849
  # 352_123	Sum of 3521, 3522 and 3523
  # 639_19	Sum of 6391 and 6399
  # 821_19	Sum of 8211 and 8219
  # 829_129	Sum of 8291, 8292 and 8299
  

  #.............................................................................
  ## 01.1. Import SIC mapping ----
  #.............................................................................
  
  # Load mapping for aggregating classes
  aggr_codes_mapping <- readxl::read_excel(path = here("INPUT","SIC code aggregation.xlsx"), sheet = "Mapping") %>%
    clean_names() %>% 
    rename(code_name=sic_class_code)
  
  # Create vector with new codes
  new_aggr_codes <- aggr_codes_mapping %>% 
    distinct(agg_class_code, .keep_all = TRUE) %>%  #remove duplicates
    pull(agg_class_code)
  
  # Create vector with old codes to be dropped
  old_aggr_codes <- aggr_codes_mapping %>% 
    distinct(code_name, .keep_all = TRUE) %>%  #remove duplicates
    pull(code_name)
  
  #.............................................................................
  
  # Import mapping between levels within SIC hierarchy
  
  readxl::read_excel(path = here("INPUT","BRES working (1998-2015).xlsx"), sheet = "SIC lookup", range = "A2:D618") %>%
    clean_names() %>% # Excluded 2016 data on purpose
    saveRDS(file=paste0(INTERMEDIATE,"sic_mapping",".Rda"))
  
  # Adjust mapping to have split G section
  # The aggregated classes already exist
  sic_mapping <- readRDS(paste0(INTERMEDIATE,"sic_mapping",".Rda")) %>% 
    mutate(section = case_when( division=="47" ~ "G1",
                                division %in% c("45","46") ~ "G2",
                                TRUE ~ section)) 
  
  sic_mapping_long <- sic_mapping %>% 
    pivot_longer(cols=c(division,group,class), names_to = "level_name", values_to = "code_name") %>% 
    mutate(level_num = case_when( code_name %in% new_aggr_codes ~ 4L,
                                  TRUE ~ str_length(code_name))) %>% 
    arrange(level_num,code_name) %>% 
    distinct(level_name,code_name, .keep_all = TRUE) #remove duplicates
  
  #.............................................................................
  
  # Import descriptions of all SIC levels
  sic_descriptions <- readxl::read_excel(path = here("INPUT","SIC code descriptions.xlsx"), sheet = "SIC descriptions") %>%
    clean_names() %>% 
    rename(level_num=level) %>% 
    filter(!(code_name %in% old_aggr_codes)) # Drop class codes which are aggregated
  
  # Check that all code names are uniquely identified
  stopifnot(identical(sic_descriptions[,"code_name"], unique(sic_descriptions[,"code_name"])))
  
  #.............................................................................
  ## 01.2. Import WFJ series ----
  #.............................................................................
  
  
  # - NOTE: should automate the procedure for this as well,
  # but since already have clear data in Excel, just import it instead
  
  readxl::read_excel(path = here("INPUT","WFJ employee jobs 2022.xlsx"), sheet = "Final table wide") %>%
    clean_names() %>%
    saveRDS(file=paste0(INTERMEDIATE,"wfj_series",".Rda"))
    
    wfj_series <- readRDS(paste0(INTERMEDIATE,"wfj_series",".Rda")) %>% 
      rename_with(~ stringr::str_replace(.x, 
                                       pattern = "x", 
                                       replacement = "wfj_"))
  
  
  #.............................................................................
  ## 01.3. Import BRES data for years 1998-2015 ----
  #.............................................................................
  
  
  
  #.............................................................................
  # CLASS
  #.............................................................................
  
  readxl::read_excel(path = here("INPUT","BRES working (1998-2015).xlsx"), sheet = "Class output", range = "A1:S617") %>%
    clean_names() %>% # Excluded 2016 data on purpose
     saveRDS(file=paste0(INTERMEDIATE,"bres_9815_class_raw",".Rda"))
  
  bres_9815_class <- readRDS(paste0(INTERMEDIATE,"bres_9815_class_raw",".Rda")) %>% 
    rename(code_name = code) %>% 
    rename_with(~ stringr::str_replace(.x, 
                                       pattern = "x", 
                                       replacement = "employee_jobs_")) %>% 
    mutate(level_num = 4,
           level_name = "class",
           across(starts_with("employee_jobs_"), ~na_if(.,0))) %>% 
    relocate(c(level_name,level_num,code_name))
  
  #.............................................................................
  # GROUP
  #.............................................................................
  
  readxl::read_excel(path = here("INPUT","BRES working (1998-2015).xlsx"), sheet = "Group output", range = "A1:S274") %>%
    clean_names() %>% # Excluded 2016 data on purpose
    saveRDS(file=paste0(INTERMEDIATE,"bres_9815_group_raw",".Rda"))
  
  bres_9815_group <- readRDS(paste0(INTERMEDIATE,"bres_9815_group_raw",".Rda")) %>% 
    rename(code_name = code) %>% 
    rename_with(~ stringr::str_replace(.x, 
                                       pattern = "x", 
                                       replacement = "employee_jobs_")) %>% 
    mutate(level_num = 3,
           level_name = "group",
           across(starts_with("employee_jobs_"), ~na_if(.,0))) %>% 
    relocate(c(level_name,level_num,code_name))
  
  #.............................................................................
  # DIVISION
  #.............................................................................
  
  readxl::read_excel(path = here("INPUT","BRES working (1998-2015).xlsx"), sheet = "Division output", range = "A1:S89") %>%
    clean_names() %>%  # Excluded 2016 data on purpose
    saveRDS(file=paste0(INTERMEDIATE,"bres_9815_division_raw",".Rda"))
  
  bres_9815_division <- readRDS(paste0(INTERMEDIATE,"bres_9815_division_raw",".Rda")) %>% 
    rename(code_name = code) %>% 
    rename_with(~ stringr::str_replace(.x, 
                                       pattern = "x", 
                                       replacement = "employee_jobs_")) %>% 
    mutate(level_num = 2,
           level_name = "division",
           across(starts_with("employee_jobs_"), ~na_if(.,0))) %>% 
    relocate(c(level_name,level_num,code_name))
  
  #.............................................................................
  # SECTION
  #.............................................................................
    
  readxl::read_excel(path = here("INPUT","BRES working (1998-2015).xlsx"), sheet = "Inq6 adjusted section", range = "A1:S22") %>%
    clean_names() %>% # Excluded 2016 data on purpose
    saveRDS(file=paste0(INTERMEDIATE,"bres_9815_section_raw",".Rda"))
  
  bres_9815_section <- readRDS(paste0(INTERMEDIATE,"bres_9815_section_raw",".Rda")) %>% 
    rename(code_name = section) %>% 
    rename_with(~ stringr::str_replace(.x, 
                                       pattern = "x", 
                                       replacement = "employee_jobs_")) %>% 
    mutate(level_num = 1,
           level_name = "section",
           across(starts_with("employee_jobs_"), ~na_if(.,0))) %>% 
    relocate(c(level_name,level_num,code_name))
  
  #.............................................................................
  # COMBINE
  #.............................................................................
  
  # Append datasets
  bres_9815_temp <- bres_9815_section %>% 
    rbind(bres_9815_division) %>% 
    rbind(bres_9815_group) %>% 
    rbind(bres_9815_class) %>% 
    arrange(level_num, code_name)
  
  # Create split in G
  bres_9815_gsplit <- bres_9815_temp %>% 
    filter((level_num==2 & code_name=="47") | (level_num==2 & code_name %in% c("45","46"))) %>% 
    mutate( level_num = 1,
            level_name = "section",
            code_name = case_when( code_name=="47" ~ "G1",
                                   code_name %in% c("45","46") ~ "G2")) %>% 
    group_by(level_name,level_num,code_name)  %>% 
    summarise(across(starts_with("employee_jobs_"), ~ sum(.)))
  
  # Merge back in after removing section G
  bres_9815 <- bres_9815_temp %>% 
    filter(!(code_name %in% c("G",old_aggr_codes))) %>% # Drop G and now aggregated codes
    rbind(bres_9815_gsplit) %>% 
    mutate(exist_in_9815=TRUE) %>% #To check which rows exist in merging later on
    arrange(level_num, code_name)
  
  # Check that all code names are uniquely identified
  stopifnot(identical(bres_9815[,"code_name"], unique(bres_9815[,"code_name"])))
  
  #.............................................................................
  ## 01.4. Import data for years post 2016 ----
  #.............................................................................
  
  
  #.............................................................................
  # IMPORT 2016
  #.............................................................................
  
  # 2016: old long format with break in rows. Has aggregated class codes.

  readxl::read_excel(path = here("INPUT","BRES weighted 2016 revised.xlsx"), sheet = "aggregated") %>%
    clean_names() %>%
    saveRDS(file=paste0(INTERMEDIATE,"bres_2016_rev_raw",".Rda"))
  
  bres_2016_rev <- readRDS(paste0(INTERMEDIATE,"bres_2016_rev_raw",".Rda")) %>%
    select(-c("x3","x4","new_section_code","aggregation")) %>% 
    rename(code_name=x1, employee_jobs_2016=employee_jobs) %>% 
    filter(!is.na(code_name) & !(code_name %in% old_aggr_codes)) %>%  # Removing the blank rows
    mutate(employee_jobs_2016 = na_if(employee_jobs_2016,"..")) %>%  # Redefine as NA
    mutate(employee_jobs_2016 = as.numeric(employee_jobs_2016)) %>% # convert to numbers
    mutate(level_num = case_when( code_name %in% c("G1","G2") ~ 1L,
                                  code_name %in% new_aggr_codes ~ 4L,
                                  TRUE ~ str_length(code_name)),  # Create level number based on code string
           level_name = case_when( level_num == 1 ~ "section",
                                   level_num == 2 ~ "division",
                                   level_num == 3 ~ "group",
                                   level_num == 4 ~ "class"),
           exist_in_2016=TRUE) %>% 
    relocate(c(level_name,level_num,code_name))
  
  # Check that all code names are uniquely identified
  stopifnot(identical(bres_2016_rev[,"code_name"], unique(bres_2016_rev[,"code_name"])))
  
  #.............................................................................
  # IMPORT 2017
  #.............................................................................

  # 2017: old long format with break in rows. Has aggregated class codes.
  
  readxl::read_excel(path = here("INPUT","BRES weighted 2017 revised.xlsx"), sheet = "aggregated") %>%
    clean_names() %>%
    saveRDS(file=paste0(INTERMEDIATE,"bres_2017_rev_raw",".Rda"))
  
  # NOTE: there is an error in 2017 raw data which has additional non-duplicate row for group 960, which should be removed.
  bres_2017_rev <- readRDS(paste0(INTERMEDIATE,"bres_2017_rev_raw",".Rda")) %>%
    select(-c("x3","new_section_code","aggregation")) %>% 
    rename(code_name=x1, employee_jobs_2017=employee_jobs) %>% 
    filter(!is.na(code_name) & 
             !(code_name==960 & employee_jobs_2017==44843.03000) &
             !(code_name %in% old_aggr_codes)) %>%  # Removing the blank rows & buggy group 960 & agg. codes
    mutate(employee_jobs_2017 = na_if(employee_jobs_2017,"..")) %>%  # Redefine as NA
    mutate(employee_jobs_2017 = as.numeric(employee_jobs_2017)) %>% # convert to numbers
    mutate(level_num = case_when( code_name %in% c("G1","G2") ~ 1L,
                                  code_name %in% new_aggr_codes ~ 4L,
                                  TRUE ~ str_length(code_name)),  # Create level number based on code string
           level_name = case_when( level_num == 1 ~ "section",
                                   level_num == 2 ~ "division",
                                   level_num == 3 ~ "group",
                                   level_num == 4 ~ "class"),
           exist_in_2017=TRUE) %>% 
    relocate(c(level_name,level_num,code_name))
  
  # Check that all code names are uniquely identified
  stopifnot(identical(bres_2017_rev[,"code_name"], unique(bres_2017_rev[,"code_name"])))
  
  #.............................................................................
  # IMPORT 2018-2020
  #.............................................................................
  
  # Newer format with more columns already.
  # Needs to perform split in section G and aggregate certain classes
  
  for (year_data in c("2018_rev","2019_rev_v3","2020_prov")) {
    
    year <- substr(year_data,1,4)
    
    # newer format with more descriptors in columns
    
    readxl::read_excel(path = paste0(INPUT,"Stata_output BRES",year_data,".xlsx"), sheet = "output_long") %>%
      clean_names() %>%
      saveRDS(file=paste0(INTERMEDIATE,"bres_",year_data,"_raw",".Rda"))
    
    bres_year_temp <-  readRDS(paste0(INTERMEDIATE,"bres_",year_data,"_raw",".Rda")) %>% 
      rename(code_name = name)  %>% 
      mutate(code_name = case_when( (level_num==2 & str_length(code_name)==1) ~ paste0("0",code_name),
                                    (level_num==3 & str_length(code_name)==2) ~ paste0("0",code_name),
                                    (level_num==4 & str_length(code_name)==3) ~ paste0("0",code_name),
                                    TRUE ~ code_name)) # adding leading zeroes
  
    # Split Section G into division G1=47 and G2=45+46  
    bres_year_temp_gsplit <- bres_year_temp %>% 
      filter((level_num==2 & code_name=="47") | (level_num==2 & code_name %in% c("45","46"))) %>% 
      mutate( level_num = 1,
              level_name = "section",
              code_name = case_when( code_name=="47" ~ "G1",
                                     code_name %in% c("45","46") ~ "G2")) %>% 
      group_by(level_name,level_num,code_name)  %>% 
      summarise(employee_count = sum(employee_count)) 
    
    # Aggregate select classes to conform with previous classification
    bres_year_temp_agg <- bres_year_temp %>% 
      merge(aggr_codes_mapping, by="code_name", all = FALSE) %>% #only keep mutual matches
      mutate(code_name = agg_class_code) %>%  #replace names
      group_by(level_name,level_num,code_name)  %>% 
      summarise(employee_count = sum(employee_count)) 
    
    # Merge back in after removing section G
    bres_year <- bres_year_temp %>% 
      filter(!(code_name %in% c("G",old_aggr_codes))) %>% # Filter out changed rows
      rbind(bres_year_temp_gsplit) %>% 
      rbind(bres_year_temp_agg) %>% 
      rename_with(~ paste0("employee_jobs_",substr(year,1,4)),.cols = employee_count) %>% # assign year to count
      mutate(!!paste0("exist_in_",year) := TRUE) %>% 
      arrange(level_num, code_name)
    
    # Check that all code names are uniquely identified
    stopifnot(identical(bres_year[,"code_name"], unique(bres_year[,"code_name"])))
    
    assign(paste0("bres_",year_data),bres_year) #give dynamic name
    assign(paste0("bres_",year_data,"_gsplit"),bres_year_temp_gsplit) #give dynamic name
    remove(bres_year)
    remove(bres_year_temp)
    remove(bres_year_temp_gsplit)
    remove(bres_year_temp_agg)
  }
  
  #.............................................................................
  ## 01.5. COMBINE ALL BRES DATA ----
  #.............................................................................
  
  bres_combined_data <- tibble("code_name" = character(),
                               "level_num" = numeric(),
                               "level_name" = character())
  
  for (year_data in c("bres_9815","bres_2016_rev","bres_2017_rev", "bres_2018_rev", "bres_2019_rev_v3", "bres_2020_prov")) {
    bres_temp <- eval(as.name(year_data))
    year <- substr(year_data,6,4)
    
    bres_combined_data <- bres_combined_data %>% 
      merge(bres_temp, by = c("code_name","level_num","level_name"),all = TRUE) %>% 
      relocate(starts_with("exist_in_"),.after=last_col()) %>% 
      arrange(level_num,code_name)
      
  }
  
  #_____________________________________________________________________________
  # 02. CALCULATE AND APPLY WFJ CONSTRAINT FACTORS ----
  #_____________________________________________________________________________
  
  # Compare the section-level jobs data from BRES and WFJ for each year
  # Calculate an 'uplift' factor to transform BRES data into WFJ levels
  # Then apply the factor for all levels
  
  #.............................................................................
  ## 02.1. CALCULATE FACTORS AT SECTOR LEVEL ----
  #.............................................................................
  
  bres_combined_sector <- bres_combined_data %>% 
    filter(level_num == 1) %>% 
    select(-c(starts_with("exist_in")))
  
  bres_wfj_sectors <- bres_combined_sector %>% 
    merge(wfj_series, by.x = "code_name", by.y = "section", all= TRUE) 
  
  # Note on R code: using !!sym(.) tells R to evaluate the function immediately, so that it realises the string refers to an actual column
  # This is similar to using local macros within loops in Stata.
  
  for (year in 1998:2020) {
    bres_wfj_sectors <- bres_wfj_sectors %>% 
      mutate(!!sym(paste0("factor_",year)) := !!sym(paste0("wfj_",year))/!!sym(paste0("employee_jobs_",year))) 
  }
  
  bres_wfj_factors <-bres_wfj_sectors %>% 
    select(c("code_name",contains("factor_"),contains("wfj_"))) %>% # Need the WFJ to substitute into section T (which is missing in BRES)
    filter(code_name != "Total (excl. T)") %>% 
    mutate(across(starts_with("factor"), ~ case_when(is.na(.) ~ 1,
                                                     TRUE ~ .))) %>% # Where there are no factors, set to 1
    rename(section = code_name) #To merge on long SIC mapping
  
  #.............................................................................
  ## 02.2. APPLY FACTORS IN FULL BRES DATASET ----
  #.............................................................................
  
  # First needs to assign the correct section at each level using mapping
  bres_combined_mapped <- bres_combined_data %>% 
    merge(sic_mapping_long ,by = c("code_name","level_num","level_name"), all.x = TRUE) %>% 
    mutate(section = case_when(is.na(section) & level_num==1 ~ code_name,
                               TRUE ~ section)) 
  
  # Merge in the factors
  bres_combined_constrained <- bres_combined_mapped %>% 
    merge(bres_wfj_factors, by = "section") %>% 
    arrange(level_num,code_name)
  
  # The loop below creates the constrained version of job numbers by multiplying factors with jobs in each relevant year
  for (year in 1998:2020) {
    bres_combined_constrained <- bres_combined_constrained %>% 
      mutate(!!sym(paste0("employee_jobs_c_",year)) := !!sym(paste0("employee_jobs_",year)) * !!sym(paste0("factor_",year))) %>% 
      mutate(!!sym(paste0("employee_jobs_c_",year)) :=  # Replace constrained sum with WFJ in section T
               case_when((code_name=="T" & !!sym(paste0("wfj_",year)) != 0) ~ !!sym(paste0("wfj_",year)),
                                                                 TRUE ~ !!sym(paste0("employee_jobs_c_",year))))
  }
  
    
  #_____________________________________________________________________________
  # 03. Export  ----
  #_____________________________________________________________________________
  
  # Export versions for error checking at full precision, but also for publication
  # where values are rounded. At section level, round to nearest 250, and 
  # to nearest 100 at all other levels.
  
  # NOTE: there is no way to only overwrite single sheets when exporting to Excel
  # Hence the data is exported to a file, but then needs to be copied over to the analysis workbooks
  # into the relevant sheet "data".
  
  #.............................................................................
  ## 03.1. Create final tables and export full precision ----
  #.............................................................................
  
  # Calculate row with totals before rounding
  bres_total_row <- bres_combined_constrained %>%
    filter(level_num==1) %>% #avoid double-counting and missing values due to suppression
    summarise(across(starts_with("employee_jobs"), sum,na.rm = TRUE)) %>% 
    mutate(code_name= "Total")
  
  bres_full_precision <- bres_combined_constrained %>% 
    select(-c(section,starts_with("exist_in"),starts_with("wfj_"))) %>% 
    bind_rows(bres_total_row) %>% 
    mutate(row_rank = case_when(code_name=="Total" ~ 2,
                                TRUE ~ 1)) # ensure total row is always at bottom
    

  bres_full_precision %>% 
    arrange(row_rank, level_num,code_name) %>% 
    select(-row_rank) %>% 
    write.xlsx( file = here("OUTPUT","Detailed jobs, full precision DATA.xlsx"),sheetName = "data", append=TRUE)
  
  #.............................................................................
  ## 03.2. Create output tables with rounded values ----
  #.............................................................................
  
  # Merge with level descriptions
  bres_out <- bres_full_precision %>% 
    merge(sic_descriptions, by = c("code_name","level_num"), all=TRUE) %>% # Keep all possible categories
    relocate(code_name, level_num, level_name, description)
  
  # At section level, round to 250
  bres_out_1 <- bres_out %>% 
    filter(level_num==1 | code_name=="Total") %>% 
    arrange(row_rank, level_num,code_name) %>% 
    select(code_name,description,starts_with("employee_jobs_c")) %>%
    mutate(across(starts_with("employee_jobs_c"),~round(./250,digits=0)*250))
  
  # At other levels, round to 100
  for (n in 2:4) {
    bres_out_n <- bres_out %>% 
      filter(level_num==n) %>% arrange(row_rank, level_num,code_name) %>% 
      select(code_name,description,starts_with("employee_jobs_c")) %>%
      mutate(across(starts_with("employee_jobs_c"),~round(./100,digits=0)*100))
    
    assign(paste0("bres_out_",n), bres_out_n)
    remove(bres_out_n)
  }
  
  # Export all data to single workbook
  data_list <- list("section_dat" = bres_out_1, "division_dat" = bres_out_2,"group_dat" = bres_out_3, "class_dat" = bres_out_4)
  write.xlsx(data_list, file = here("OUTPUT","Detailed jobs, publication DATA.xlsx"), append=TRUE,keepNA = TRUE, na.string="...")
      