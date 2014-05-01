# Code to repeat for all states
library(dplyr)

# 1. download all state 5year files
# 2. unzip all state 5year files
# 3. cut out columns of interest
# 4. read in a file, produce summary, and concatenate

state_names <- c(tolower(state.abb), "dc")

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


# 2. & 3. Combine to extract, cut, then remove big file

extract_and_cut <- function(state_name){
  unzip(paste("data/csv_p", state_name, ".zip", sep =""), exdir = "data/", overwrite = TRUE)
  cmd <- paste("cut -d, -f13,35,37,75 data/ss12p", state_name, ".csv > data/", state_name ,".csv", sep = "")
  system(cmd)
  cmd2 <- paste("rm data/ss12p", state_name, ".csv", sep = "")
  system(cmd2)
}

status_cut <- lapply(state_names, 
  failwith("failed to extract", extract_and_cut))

any(status_cut == "failed to extract")

# 4. read in and summarise
JWTR_codes <- c("1" = "Car, truck, or van",
  "2" = "Bus or trolley bus",
  "3" = "Streetcar or trolley car",
  "4" = "Subway or elevated",
  "5" = "Railroad",
  "6" = "Ferryboat",
  "7" = "Taxicab",
  "8" = "Motorcycle",
  "9" = "Bicycle",
  "10" = "Walked",
  "11" = "Worked at home",
  "12" = "Other method")

read_and_summarise <- function(state_name){
  dat <- read.csv(paste("data/", state_name, ".csv", sep = ""),
    stringsAsFactors = FALSE)
  
  dat_df <- tbl_df(dat)
  
  # need to match transit type to code
  # codes from data dictionary 
  dat_df <- mutate(dat_df, transport_type = JWTR_codes[JWTR])
  
  # group by type of transport and summarise
  trans_type <- group_by(dat_df, JWTR)
  time_summary <- summarise(trans_type, 
    transport_type = first(transport_type),
    avg_time = mean(JWMNP, na.rm = TRUE),
    med_time = median(JWMNP, na.rm = TRUE),
    min_time = min(JWMNP, na.rm = TRUE),
    max_time = max(JWMNP, na.rm = TRUE),
    q25_time = quantile(JWMNP, probs = 0.25, na.rm = TRUE),
    q75_time = quantile(JWMNP, probs = 0.75, na.rm = TRUE),
    n = n(),
    n_missing = sum(is.na(JWMNP)))
  time_summary$state <- state_name
  
  rm(dat)
  time_summary
}

library(plyr)
library(dplyr)
state_summaries <- llply(state_names, read_and_summarise, .progress = "text")
state_sums_df <- do.call(rbind, state_summaries)

# 5. Save out my summary
saveRDS(state_sums_df, file = "commute_mode_state.rds")

