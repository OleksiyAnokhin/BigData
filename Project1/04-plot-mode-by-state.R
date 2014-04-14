library(ggplot2)
library(dplyr)

# Load my summary
state_sums <- readRDS(state_sums_df, file = "commute_mode_state.rds")

by_state <- group_by(state_sums, state)

by_state %.%
  summarise(count = sum(n), count_miss = sum(n_missing)) %.%
  arrange(count) %.%
  mutate(prop_miss = count_miss/count)

# drop missing types
by_state <- filter(by_state, !is.na(transport_type))
by_state <- mutate(by_state, state_n = sum(n), prop = prop)

qplot(state, prop, data = by_state) + 
  facet_wrap(~ transport_type, scale = "free_y")

qplot(prop, reorder(state, prop), data = filter(by_state, 
    transport_type == "Car, truck, or van") )
# new york and alaska have the fewest "drive to works", alambama highest.
# oregon surprisingly low

qplot(prop, reorder(state, prop), data = filter(by_state, 
  transport_type == "Bicycle") )
# we win! ;)

qplot(prop, reorder(state, prop), data = filter(by_state, 
  transport_type == "Walked") )

qplot(transport_type, prop, data = by_state, geom = "line", 
  group = state)
# ? oh some states have missing for ferryboat?
select(filter(by_state, transport_type == "Ferryboat"), state, prop)
# TODO: fix so that every state has every mode, 0 if no one reported it.

bike <- filter(by_state, transport_type == "Bicycle")
bike <- rename(bike, c("state" = "region", "prop" = "value"))
library(choroplethr)
choroplethr(bike, lod = "state") # ugh lot's of things I don't like about this plot

# ideas:
# chloropleth for each mode
# profile plots for each state
# parallel coordinate (profile plots on top of each other)
# rankings
# summaries (ratio of machine power to human power or public to private?)



qplot(state_n, prop, data = filter(by_state, 
  transport_type == "Car, truck, or van") )




tail(select(mutate(by_state, sum(n)), state, transport_type, n, `sum(n)`))
