# Code to download Oregon pop data 2012.
# part functional part pedagoical

# === download file === #
# download OR 2012_5yr
download.file("http://www2.census.gov/acs2012_5yr/pums/csv_por.zip", 
  destfile = "data/csv_por.zip")

# === unzip and get file === #
# look into others function: unz, unzip, system('unzip ...'), 
# check files in the zip archive without extracting
unzip("data/csv_por.zip", list = TRUE)

# extract and read in one go
or_sm <- read.csv(unz("data/csv_por.zip", "ss12por.csv"), nrows = 10,  # notice just 10 lines! why?
  stringsAsFactors = FALSE)
str(or_sm)

# or extract and read in parts
unzip("data/csv_por.zip", exdir = "data/")
or_sm <- read.csv("data/ss12por.csv", nrows = 10,
  stringsAsFactors = FALSE)
str(or_sm)

# ====  ASIDE ==== #
system.time(or1 <- read.csv("data/ss12por.csv",
  stringsAsFactors = FALSE))

# Specifying column types sometimes speeds things up. 
# This is for illustration only, I cheated because
# I read the data in first to find out the types, obviously
# not saving any time.
types <- sapply(or1, class)
system.time(or2 <- read.csv("data/ss12por.csv",
  stringsAsFactors = FALSE, colClasses = types))

# you can also use NULL to not read a column at all, doesn't save any time
types2 <- ifelse(names(or_sm) %in% c( "JWMNP", "WAGP", "JWTR", "COW"), "integer", "NULL")

system.time(or3 <- read.csv("data/ss12por.csv",
      stringsAsFactors = FALSE, colClasses = types2))
dim(or3)

# cut out columns using a command line tool - did you do last week's readings?
system.time(system("cut -d, -f13,35,37,75 data/ss12por.csv > data/ss12por-cut.csv"))
system.time(or4 <- read.csv("data/ss12por-cut.csv",
  stringsAsFactors = FALSE))

# other strategies
# - read chunks and process out what you need (nlines arg. to read.csv)
# - bigmemory package
# - have data in data.base
