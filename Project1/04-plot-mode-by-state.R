library(ggplot2)
library(scales)
library(dplyr)

# Load my summary
state_sums <- readRDS(file = "commute_mode_state.rds")

by_state <- group_by(state_sums, state)

by_state %.%
  summarise(count = sum(n), count_miss = sum(n_missing)) %.%
  arrange(count) %.%
  mutate(prop_miss = count_miss/count)

# drop missing types
by_state <- filter(by_state, !is.na(transport_type))
by_state <- mutate(by_state, state_n = sum(n), prop = n/state_n)

qplot(state, prop, data = by_state) + 
  facet_wrap(~ transport_type)

qplot(state, prop, data = by_state) + 
  facet_wrap(~ transport_type, scale = "free_y")

qplot(prop, reorder(state, prop), data = filter(by_state, 
    transport_type == "Car, truck, or van") )
# DC, new york and alaska have the fewest "drive to works", alambama highest.
# oregon surprisingly low

qplot(prop, reorder(state, prop), data = filter(by_state, 
  transport_type == "Bicycle") )
# we win! ;), but not now there is dc :(

# polish a little for illustration
# better state labels
state_names <-  c(state.name, "District of Columbia")
names(state_names) <- tolower(c(state.abb, "dc"))
by_state <- mutate(by_state, state_name = state_names[state])

ggplot(filter(by_state, transport_type == "Bicycle"), 
  aes(prop, reorder(state_names, prop))) + 
  geom_segment(aes(xend = -Inf, yend = reorder(state_names, prop)), colour = "grey80") +
  geom_point() + 
  scale_x_continuous(labels = percent) +
  xlab("") + ylab("") +
  theme_bw() + theme(panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank())
ggsave("bike-states.png", width = 3, height = 7)

# ideas:
# chloropleth for each mode
# profile plots for each state
# parallel coordinate (profile plots on top of each other)
# rankings
# summaries (ratio of machine power to human power or public to private?)

qplot(prop, reorder(state, prop), data = filter(by_state, 
  transport_type == "Walked") )

qplot(transport_type, prop, data = by_state, geom = "line", 
  group = state)
# ? oh some states have missing for ferryboat?
select(filter(by_state, transport_type == "Ferryboat"), state, prop)
# TODO: fix so that every state has every mode, 0 if no one reported it.

# == profile and pcp plots == #

# profile plot, looking for unusual states
qplot(reorder(transport_type, prop), prop, data = filter(by_state, transport_type != "Car, truck, or van"), geom = "line", 
  group = state,
  colour = state %in% c("ny", "ak"))

# parallel coordinates plot
by_type <- group_by(by_state, "transport_type", add = FALSE)

by_type <- mutate(by_type, 
  prop_sc = rescale(prop),
  prop_rank = rank(prop))

# classic parallel coordinates plot
qplot(reorder(transport_type, prop_sc), prop_sc, data = by_type, geom = "line", 
  group = state)

# with ranks instead?
qplot(reorder(transport_type, prop_sc), prop_rank, data = by_type, geom = "line", 
  group = state)

# == choropleth plots == #

bike <- filter(by_state, transport_type == "Bicycle")

usa_df <- readRDS("usa-state-map_all.rds")
bike_st <- inner_join(bike, usa_df, by = "state")

qplot(x, y, data = bike_st, 
  geom = "polygon", group = group, fill = prop) +
  coord_equal()

# for all types
library(reshape2)
props <- melt(select(by_state, transport_type, state, prop))
props_st <- inner_join(props, usa_df, by = "state")

plots <- plyr::dlply(props_st, "transport_type", function(df){
  qplot(x, y, data = df, 
    geom = "polygon", group = group, fill = value) +
    facet_wrap(~ transport_type) +
    coord_fixed() +
    theme_minimal()  
})

library(gridExtra)

pdf("prop-maps.pdf", width = 11, height = 8)
  do.call(grid.arrange, plots)
dev.off()

