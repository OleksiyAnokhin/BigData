library(dplyr)
library(ggplot2)

# ==== read in OR data ==== #

or <- read.csv("data/ss12por.csv",
  stringsAsFactors = FALSE)

or_df <- tbl_df(or)

# === How does the time to commute vary for people === #
#        that commute via different methods? 

select(or_df, JWMNP, WAGP, JWTR, COW)

# need to match transit type to code
# codes from data dictionary 
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
or_df <- mutate(or_df, transport_type = JWTR_codes[JWTR])

# group by type of transport and summarise
trans_type <- group_by(or_df, JWTR)
time_summary <- summarise(trans_type, 
  transport_type = first(transport_type),
  avg_time = mean(JWMNP, na.rm = TRUE),
  med_time = median(JWMNP, na.rm = TRUE),
  n = n(),
  n_missing = sum(is.na(JWMNP)))
mutate(time_summary, prop = n/sum(n))
# 56% missing, 
#     "not a worker--not in the labor force, 
#      including persons under 16 years; unemployed; 
#      employed, with a job but not at work; 
#      Armed Forces, with a job but not at work"
# people who work at home have missing commute time (really 0 mins?)
# most people (~ 82% ) go by "car, truck or van", then "worked from home"
# people who walk have short commutes (that's probably why they walk)

# TODO:
#  - look into distributions, shape, spread etc. boxplots, 5 number summary
#  - is only 56% reasonable for number of workers or are there other reasons this is missing?
#  - split car/truck/van users by number of people in vehicle (JWRIP)
#  - how do these numbers vary by state? year?

# === How does the time to commute vary for people === #
#        that commute via different methods? 

# look at some plots, but just use a sample to keep things quick
samp <- sample(nrow(or_df), size = 1000)

qplot(JWMNP, data = or_df[samp, ])
qplot(WAGP, data = or_df[samp, ])

summarise(or_df, mean(is.na(WAGP)), mean(WAGP == 0, na.rm = TRUE))
# 17% missing,  42% of non-missing earn $0

qplot(log(WAGP), JWMNP, data = or_df[samp, ])
qplot(JWMNP, log(WAGP), data = or_df[samp, ])

qplot(factor(JWMNP), log(WAGP), data = or_df[samp, ], geom = "boxplot")

qplot(factor(JWMNP), log(WAGP), data = or_df, geom = "boxplot")
# slight positive relationship between income and mean/median? commute time for commute
# times below 15 minutes? think about better plot
# worry about $0 earners
