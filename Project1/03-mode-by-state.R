# Code to repeat for all states
library(dplyr)

# 1. download all state 5year files
# 2. unzip all state 5year files
# 3. cut out columns of interest
# 4. read in a file, produce summary, and concatenate

state_names <- tolower(state.abb)

# 1. download zip files
base_url <- "http://www2.census.gov/acs2012_5yr/pums/csv_p"

get_acs_pfile <- function(state_name){
  url <- paste(base_url, state_name, ".zip", sep = "")
  dest <- paste("data/csv_p", state_name, ".zip", sep = "")
  download.file(url, destfile = dest)
}

status <- lapply(state_names, 
  failwith("failed to download", get_acs_pfile))

any(status == "failed to download")
