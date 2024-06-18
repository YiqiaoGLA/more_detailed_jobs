#_______________________________________________________________________________
###  YC May 2024 RUN ALL SCRIPTS ----
#_______________________________________________________________________________
  
  # Put in your Nomis API key (NB: THE BELOW IS YC'S KEY!!)
  Sys.setenv(NOMIS_API_KEY = "0x5f83cbe42eb6376a0f515a4bdc4f01fb2ee000fd")

  # ACTION: set whether to re-download all datasets, even if already exists
  redownload_all <- FALSE

  # HERE package needed for dynamic pathfinding
  library("here") 
  

#...............................................................................
#### Run preparation scripts ----
#...............................................................................

  # Create paths as strings
  source(here("SCRIPTS","SUBSCRIPTS","GLAE_paths.r"))
  
  # Data packages
  source(paste0(SUBSCRIPTS,"GLAE_packages_load",".r"))

  # Inputs such as borough codes in Nomis
  source(paste0(SUBSCRIPTS,"GLAE_data_presets",".r"))
  
  # Run the subscripts necessary 
  source(paste0(SUBSCRIPTS,"GLAE_functions_load",".r"))

#...............................................................................
#### Run scripts with WFJ to constrain BRES data ----
#...............................................................................
  # Load WFJ data
  source(paste0(SUBSCRIPTS,"GLAE_wfj_dataload",".r"))
  # Run MDJ scripts
  source(paste0(SCRIPTS,"01_Prepare_input_data",".r"))
  source(paste0(SCRIPTS,"02_MDJ_table_production",".r"))
  source(paste0(SCRIPTS,"03_blog_figure",".r"))
  # THEN add results to the previous release in EXCEL
  