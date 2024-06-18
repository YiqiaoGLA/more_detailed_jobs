# for tree mapping for 2022, create an "other" category within each higher level to contain jobs left out from below
## First create 'other' rows for each higher level to append
collapse_level <- function(data=NULL,
                           lownum=NULL,
                           datayear=2022) {
  
  if (lownum == 2) lowlevel <-  "division"
  if (lownum == 3) lowlevel <-  "group"
  if (lownum == 4) lowlevel <-  "class"
  
  if (lowlevel=="division") highlevels <- c("section")
  if (lowlevel=="group") highlevels <- c("division","section")
  if (lowlevel=="class") highlevels <- c("group","division","section")
  
  higher_num <- lownum-1 
  
  desc_high <- c(paste0("description_",highlevels))
  
  desc_low <- paste0("description_",lowlevel)
  
  # Data names
  
  ldata <- paste0("bres_out_",lownum)
  hdata <- paste0("bres_out_",higher_num)
  highlevel <- highlevels[1]
  
  emp_jobs_low <- paste0("employee_jobs_c_",datayear,"_",lowlevel)
  emp_jobs_high <- paste0("employee_jobs_c_",datayear,"_",highlevel)
  
  # first create the bin collector row
  other_row_data <- data %>% 
    mutate(!!desc_low:=paste0("Other ",lowlevel)) %>% 
    group_by(across(all_of(c(highlevels,desc_high)))) %>% 
    filter(row_number()==1) %>% 
    select(all_of(c(highlevels,desc_high,desc_low)))
  
  # Combine extra row with data and calculate remainder
  collapse_data <- data %>% 
    group_by(across(all_of(c(highlevels,desc_high,desc_low,lowlevel)))) %>% 
    filter(row_number()==1) %>% 
    select(all_of(c(highlevels,desc_high,lowlevel,desc_low))) %>% 
    bind_rows(other_row_data) %>% 
    left_join(get(ldata),
              by=setNames("code_name",lowlevel)) %>% 
    left_join(get(hdata),
              by=setNames("code_name",highlevel),
              suffix=c(paste0("_",lowlevel),paste0("_",highlevel))) %>% 
    group_by(across(all_of(highlevel))) %>%
    mutate(highlevel_sum = sum(!!sym(emp_jobs_low),na.rm = TRUE),
           highlevel_diff=!!sym(emp_jobs_high)-highlevel_sum,
           !!sym(emp_jobs_low) := case_when(!!sym(desc_low) == paste0("Other ",lowlevel) ~ highlevel_diff,
                                            TRUE ~ !!sym(emp_jobs_low))) %>% 
    ungroup() %>% 
    select(all_of(c(highlevels,desc_high,lowlevel,desc_low,emp_jobs_low))) %>% 
    arrange(across(all_of(c(highlevels,lowlevel))))
  
  return(collapse_data)
}

section_emp_jobs <- bres_out_1 %>% 
  rename(employee_jobs_c_2022_section=employee_jobs_c_2022,
         description_section=description,
         section=code_name) %>% 
  select(section,description_section,employee_jobs_c_2022_section)

division_emp_jobs <- collapse_level(sic_descriptions_mapped,2)

group_emp_jobs <- collapse_level(sic_descriptions_mapped,3)

class_emp_jobs <- collapse_level(sic_descriptions_mapped,4)

# Final data for tree map
bres_out_2022_tree <- class_emp_jobs %>% 
  full_join(group_emp_jobs,by=c("group","division","section",
                                "description_group","description_division","description_section")) %>% 
  full_join(division_emp_jobs,by=c("division","section",
                                   "description_division","description_section")) %>% 
  full_join(section_emp_jobs,by=c("section",
                                  "description_section")) %>% 
  mutate(employee_jobs_c_2022_class = case_when(description_division == "Other division" ~ employee_jobs_c_2022_division,
                                                description_group == "Other group" ~ employee_jobs_c_2022_group,
                                                TRUE ~ employee_jobs_c_2022_class),
         employee_jobs_c_2022_group = case_when(description_division == "Other division" ~ employee_jobs_c_2022_division,
                                                TRUE ~ employee_jobs_c_2022_group)) %>% 
  arrange(section,division,group,class) %>% 
  relocate(section,division,group,class,
           description_section,description_division,description_group,description_class,
           employee_jobs_c_2022_section,employee_jobs_c_2022_division,employee_jobs_c_2022_group,employee_jobs_c_2022_class)

#write.xlsx(bres_out_2022_tree, file = paste0(DATA_OUT,"Blog/","Detailed jobs, tree diagram.xlsx"), append=TRUE)


###section growth
section_emp_growth <- bres_out_1 %>% 
  rename(employee_jobs_c_2022_section=employee_jobs_c_2022,
         employee_jobs_c_2021_section=employee_jobs_c_2021,
         description_section=description,
         section=code_name) %>% 
  select(section,description_section,employee_jobs_c_2021_section,employee_jobs_c_2022_section) %>%
  mutate(add_section = employee_jobs_c_2022_section-employee_jobs_c_2021_section,
         growth_section = (employee_jobs_c_2022_section-employee_jobs_c_2021_section)/employee_jobs_c_2021_section)

write.xlsx(section_emp_growth, file = paste0(DATA_OUT,"Blog/","Detailed jobs, section comparison.xlsx"), append=TRUE)

###full comparison table
collapse_level <- function(data=NULL,
                           lownum=NULL,
                           datayear=2021) {
  
  if (lownum == 2) lowlevel <-  "division"
  if (lownum == 3) lowlevel <-  "group"
  if (lownum == 4) lowlevel <-  "class"
  
  if (lowlevel=="division") highlevels <- c("section")
  if (lowlevel=="group") highlevels <- c("division","section")
  if (lowlevel=="class") highlevels <- c("group","division","section")
  
  higher_num <- lownum-1 
  
  desc_high <- c(paste0("description_",highlevels))
  
  desc_low <- paste0("description_",lowlevel)
  
  # Data names
  
  ldata <- paste0("bres_out_",lownum)
  hdata <- paste0("bres_out_",higher_num)
  highlevel <- highlevels[1]
  
  emp_jobs_low <- paste0("employee_jobs_c_",datayear,"_",lowlevel)
  emp_jobs_high <- paste0("employee_jobs_c_",datayear,"_",highlevel)
  
  # first create the bin collector row
  other_row_data <- data %>% 
    mutate(!!desc_low:=paste0("Other ",lowlevel)) %>% 
    group_by(across(all_of(c(highlevels,desc_high)))) %>% 
    filter(row_number()==1) %>% 
    select(all_of(c(highlevels,desc_high,desc_low)))
  
  # Combine extra row with data and calculate remainder
  collapse_data <- data %>% 
    group_by(across(all_of(c(highlevels,desc_high,desc_low,lowlevel)))) %>% 
    filter(row_number()==1) %>% 
    select(all_of(c(highlevels,desc_high,lowlevel,desc_low))) %>% 
    bind_rows(other_row_data) %>% 
    left_join(get(ldata),
              by=setNames("code_name",lowlevel)) %>% 
    left_join(get(hdata),
              by=setNames("code_name",highlevel),
              suffix=c(paste0("_",lowlevel),paste0("_",highlevel))) %>% 
    group_by(across(all_of(highlevel))) %>%
    mutate(highlevel_sum = sum(!!sym(emp_jobs_low),na.rm = TRUE),
           highlevel_diff=!!sym(emp_jobs_high)-highlevel_sum,
           !!sym(emp_jobs_low) := case_when(!!sym(desc_low) == paste0("Other ",lowlevel) ~ highlevel_diff,
                                            TRUE ~ !!sym(emp_jobs_low))) %>% 
    ungroup() %>% 
    select(all_of(c(highlevels,desc_high,lowlevel,desc_low,emp_jobs_low))) %>% 
    arrange(across(all_of(c(highlevels,lowlevel))))
  
  return(collapse_data)
}

section_emp_jobs <- bres_out_1 %>% 
  rename(employee_jobs_c_2021_section=employee_jobs_c_2021,
         description_section=description,
         section=code_name) %>% 
  select(section,description_section,employee_jobs_c_2021_section)

division_emp_jobs <- collapse_level(sic_descriptions_mapped,2)

group_emp_jobs <- collapse_level(sic_descriptions_mapped,3)

class_emp_jobs <- collapse_level(sic_descriptions_mapped,4)

# Final data for tree map
bres_out_2021_tree <- class_emp_jobs %>% 
  full_join(group_emp_jobs,by=c("group","division","section",
                                "description_group","description_division","description_section")) %>% 
  full_join(division_emp_jobs,by=c("division","section",
                                   "description_division","description_section")) %>% 
  full_join(section_emp_jobs,by=c("section",
                                  "description_section")) %>% 
  mutate(employee_jobs_c_2021_class = case_when(description_division == "Other division" ~ employee_jobs_c_2021_division,
                                                description_group == "Other group" ~ employee_jobs_c_2021_group,
                                                TRUE ~ employee_jobs_c_2021_class),
         employee_jobs_c_2021_group = case_when(description_division == "Other division" ~ employee_jobs_c_2021_division,
                                                TRUE ~ employee_jobs_c_2021_group)) %>% 
  arrange(section,division,group,class) %>% 
  relocate(section,division,group,class,
           description_section,description_division,description_group,description_class,
           employee_jobs_c_2021_section,employee_jobs_c_2021_division,employee_jobs_c_2021_group,employee_jobs_c_2021_class)

bres_out_2021_tree$employee_jobs_c_2022_section <- bres_out_2022_tree$employee_jobs_c_2022_section
bres_out_2021_tree$employee_jobs_c_2022_division <- bres_out_2022_tree$employee_jobs_c_2022_division
bres_out_2021_tree$employee_jobs_c_2022_group <- bres_out_2022_tree$employee_jobs_c_2022_group
bres_out_2021_tree$employee_jobs_c_2022_class <- bres_out_2022_tree$employee_jobs_c_2022_class
bres_out_tree <- bres_out_2021_tree %>%
  mutate(add_section = employee_jobs_c_2022_section - employee_jobs_c_2021_section,
         growth_section = add_section/employee_jobs_c_2021_section,
         add_division = employee_jobs_c_2022_division - employee_jobs_c_2021_division,
         growth_division = add_division/employee_jobs_c_2021_division,
         add_group = employee_jobs_c_2022_group - employee_jobs_c_2021_group,
         growth_group = add_group/employee_jobs_c_2021_group,
         add_class = employee_jobs_c_2022_class - employee_jobs_c_2021_class,
         growth_class = add_class/employee_jobs_c_2021_class)
         
write.xlsx(bres_out_tree, file = paste0(DATA_OUT,"Blog/","Detailed jobs, FULL comparison.xlsx"), append=TRUE)

###tree maps

treemap_sections <- section_emp_jobs %>%
  filter(!section %in% c("Total","U")) %>%
  plot_ly(
    type = "treemap",
    labels = ~description_section,
    parents = ~"",
    values = ~employee_jobs_c_2022_section,
    hoverinfo = "label+value"
  ) %>%
  layout(title = "Treemap of Section Jobs") 

treemap_sections

treemap_divisions <- division_emp_jobs %>%
  left_join(section_emp_jobs) %>%
  filter(!section %in% c("Total","U"))%>%
  filter(!is.na(division)) %>%
  plot_ly(
    type = "treemap",
    labels = ~description_division,
    parents = ~"description_section",
    values = ~employee_jobs_c_2022_division,
    hoverinfo = "label+value"
  ) %>%
  layout(title = "Treemap of Division Jobs") 

treemap_divisions

# Create dataset for Sankey diagram, i.e. with one class isolated from section. 
## Choose Class 9312
sankey_data <- bres_out_2022_tree %>% 
  filter(section=="R") %>% 
  mutate(description_class = case_when(group == "931" & class != "9312" ~ "All other classes",
                                       class == "9312" ~ paste0(": ",description_class),
                                       TRUE ~ ""),
         class = case_when(class == "9312" ~ class,
                           TRUE ~ ""),
         description_group = case_when(division == "93" & group != "931" ~ "All other groups",
                                       group == "931" ~ paste0(": ",description_group),
                                       TRUE ~ ""),
         group = case_when(group == "931" ~ group,
                           TRUE ~ ""),
         description_division = case_when(division == "93" ~ paste0(": ",description_division),
                                          TRUE ~ "All other divisions"),
         division = case_when(division == "93" ~ division,
                              TRUE ~ "")) %>% 
  group_by(section,division,group,class,
           description_section,description_division,description_group,description_class) %>% 
  summarise(employee_jobs_c_2022_class = sum(employee_jobs_c_2022_class)) %>% 
  ungroup() %>% 
  mutate(section = paste0(section,": ",description_section),
         division = paste0(division,description_division),
         group = paste0(group,description_group),
         class = paste0(class,description_class)) %>% 
  select(section,division,group,class,employee_jobs_c_2022_class)   %>% 
  pivot_longer(cols=c(section,division,group,class),names_to = "type",values_to = "type_name")%>% 
  group_by(type,type_name) %>% 
  summarise(employee_jobs_c_2022_class=sum(employee_jobs_c_2022_class)) %>% 
  mutate(from_type_name = case_when(type=="class" ~ "931: Sports activities",
                                    type=="group" ~ "93: Sports activities and amusement and recreation activities",
                                    type=="division" ~ "R: Arts, entertainment and recreation  ",
                                    TRUE ~ NA_character_),
         type_name_2 = gsub(".*: ","",type_name),
         from_type_name_2 = gsub(".*: ","",from_type_name)) %>% 
  filter(from_type_name!="" & type_name!="") %>% 
  relocate(type,from_type_name,type_name,from_type_name_2,type_name_2)


write.xlsx(sankey_data, file = paste0(DATA_OUT,"Blog/","Detailed jobs, Sankey.xlsx"), append=TRUE)
