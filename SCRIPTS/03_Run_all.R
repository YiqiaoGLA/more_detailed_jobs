#_______________________________________________________________________________
###  RUN ALL SCRIPTS ----
#_______________________________________________________________________________
  
  # Put in your Nomis API key (NB: THE BELOW IS AMMAR'S KEY!!)
  Sys.setenv(NOMIS_API_KEY = "0x01a88c6659d20042f087de2e585cdf3a07708983")

  # ACTION: set whether to re-download all datasets, even if already exists
  redownload_all <- FALSE

  # HERE package needed for dynamic pathfinding
  library("here") 
  

#...............................................................................
#### Run scripts ----
#...............................................................................

  # Create paths as strings
  source(here("SCRIPTS","SUBSCRIPTS","GLAE_paths.r"))
  
  # Data packages
  source(paste0(SUBSCRIPTS,"GLAE_packages_load",".r"))

  # Inputs such as borough codes in Nomis
  source(paste0(SUBSCRIPTS,"GLAE_data_presets",".r"))
  
  # Run the subscripts necessary 
  source(paste0(SUBSCRIPTS,"GLAE_functions_load",".r"))

  
  # Run MDJ scripts
  source(paste0(SCRIPTS,"01_Reshape_input_data",".r"))
  source(paste0(SCRIPTS,"02_MDJ_table_production",".r"))