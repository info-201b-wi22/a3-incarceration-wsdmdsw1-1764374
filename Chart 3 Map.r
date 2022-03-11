chart_3 <- function(incarceration_data){
 state_prop_black <- incarceration_data %>%
    filter(year == 2018) %>%
    select(state, black_jail_pop, total_jail_pop) %>%
    na.omit() %>%
      group_by(state) %>%
        summarise(Proportion = mean(black_jail_pop/total_jail_pop, na.rm = TRUE))
 us <- map_data("state")
  statedf <- data.frame(state = state.abb, region = tolower(state.name))
  state_prop_black <- state_prop_black %>%
    left_join(statedf)
  ggplot() + 
    geom_map(data = us, map = us, aes(x = long, y = lat, map_id = region)) +
    geom_map(data = state_prop_black, map = us, aes(fill = Proportion, map_id = region)) +
    coord_map("albers", lat0 = 39, lat1 = 45) +
    labs(title = "Percentage of Jail Population that are Black in each State", x = "Longitude", y = "Latitutde" ) +
    theme_minimal()
  
  
}
