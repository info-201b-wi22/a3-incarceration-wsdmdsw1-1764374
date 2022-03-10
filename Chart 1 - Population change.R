#The first chart that you will create and include will show the trend over time of your variable/topic. 
#Think carefully about what you want to communicate to your user 
#(you may have to find relevant trends in the dataset first!). 
#Here are some requirements to help guide your design:
  
#-Show more than one, but fewer than ~10 trends
#-This may mean showing the same measure for different locations or different racial groups. 
# Think carefully about a meaningful comparison of locations (e.g., the top 10 counties in a state, top 10 states, etc.)
#- You must have clear x and y axis labels
#- The chart needs a clear title 
#- You need a legend for your different line colors and a clear legend title

#When we say "clear" or "human readable" titles and labels, that means that you should not just display the variable name.

chart_1 <- function(incarceration_data){
 largest_five <- incarceration_data %>%
    filter(year == max(year)) %>%
    arrange(female_jail_pop) %>%
    select(fips, female_jail_pop) %>%
    na.omit() %>%
    pull(fips) %>%
    tail(5)
   incarceration_data %>%
     filter(fips %in% largest_five) %>%
     select(year, County = county_name, female_jail_pop) %>%
     na.omit() %>%
     ggplot(aes(year, female_jail_pop, color = County))+
     geom_line()+
     labs(x = "Year", y = "Female Jail Population", title = "Female Jail Population Over Time", subtitle = "Largest 5 Counties")
    
  }