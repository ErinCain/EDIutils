library(tidyverse)
library(EDIutils)
library(readxl)

# clean the data 
path_to_data <- system.file("extdata/metadata-creation-template/example",
                            "hannon_example_physical_data.csv", 
                            package = "EDIutils")

raw_data <- read_csv(path_to_data) %>% select(-starts_with("X"))

raw_data %>% 
  glimpse()

# clean up:
# 1. remove: month, year, visibility_feet
# 2. rename: all to snake_case, remove units
# 3. 

# fix columns
data_cols_fixed <- raw_data %>% 
  select(-Month, -Year, Visibility_feet) %>% # remove Month and Year, and Visibility_feet
  rename_with(~snakecase::to_snake_case(.x)) %>% 
  rename(
    # catch counts columns
    lfr_count = lf_rcount,
    wr_count = w_rcount,
    sr_count = s_rcount,
    fr_count = f_rcount,
    trout_count = troutcount, 
    length = length_meters, # fix len in meters
  )

# fix values
data_cols_fixed

# read in eml components

path_to_metadata_sheet <- system.file("extdata/metadata-creation-template/example",
                                      "example-metadata.xlsx", 
                                      package = "EDIutils")
path_to_abstract <- system.file("extdata/metadata-creation-template/example",
                                "example_abstract.docx", 
                                package = "EDIutils")


# START -------------------------
parent_element <- list() 

# ADD PUBLICATION DATE ------------------------------
parent_element <- add_pub_date(parent_element = parent_element, date = NULL)


# ADD PERSONNEL --------------------------------------
personnel_table <- read_excel(path_to_metadata_sheet, sheet = "personnel")

for (i in 1:nrow(personnel_table)) {
  current <- personnel_table[i, ]
  parent_element <- add_personnel(parent_element = parent_element, 
                                  first_name = current$first_name, 
                                  last_name = current$last_name, 
                                  email = current$email, 
                                  role = current$role, 
                                  orcid = current$orcid)
}

# TITLE AND SHORT NAME --------------------------
title <- read_excel(path_to_metadata_sheet, sheet = "title")
parent_element <- add_title(parent_element = parent_element, title = title$title, 
                            short_name = title$short_name)



# KEYWORD SET ------------------------------------
keyword_set <- read_excel(path_to_metadata_sheet, sheet = "keyword_set")
thesaurus <- unique(keyword_set$keywordThesaurus)
for (i in 1:length(thesaurus)) {
  keywords <- keyword_set$keyword[keyword_set$keywordThesaurus == thesaurus[i]]
  parent_element <- add_keyword_set(parent_element = parent_element, 
                                    keyword_set = list(keyword = keywords, 
                                                       keywordThesaurus = thesaurus[i]))
}


# ABSTRACT ------------------------------------------------
parent_element <- add_abstract(parent_element = parent_element, 
                               abstract = path_to_abstract)


# LICENSE -----------------------
license <- read_excel(path_to_metadata_sheet, sheet = "license")
parent_element <- add_license(parent_element = parent_element, default_license = license$default_license)


# FUNDING -----------------------------
funding <- read_excel(path_to_metadata_sheet, sheet = "funding")
parent_element <- add_funding(parent_element = parent_element, 
                              funder_name = funding$funder_name,
                              funder_identifier = funding$funder_identifier,
                              award_number = funding$award_number,
                              award_title = funding$award_title,
                              award_url = funding$award_url,
                              funding_description = funding$funding_description)


# MAINTENANCE --------------------------------
maintenance <- read_excel(path_to_metadata_sheet, sheet = "maintenance")
parent_element <- add_maintenance(parent_element = parent_element, status = maintenance$status,
                                  update_frequency = maintenance$update_frequency)


# METHODS ------------------------------------
method_table <- read_excel("data-raw/template/template.xlsx", sheet = "methods")
for (i in 1:nrow(method_table)) {
  current <- method_table[i, ]
  new_method <- add_method(parent_element = parent_element, methods_file = current$methods_file,
                           instrumentation = current$instrumentation)
  if (is.null(parent_element$methods)) {
    parent_element$methods <- new_method
  } else {
    parent_element$methods <- list(parent_element$methods, new_method)
  }
}



