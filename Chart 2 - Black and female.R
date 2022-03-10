#Include a chart. Make sure to describe why you included the chart, and what patterns emerged

#The second chart that you will create and include will show how two different (continuous) variables are 
#related to one another. Again, think carefully about what such a comparison means and what you want to 
#communicate to your user (you may have to find relevant trends in the dataset first!). Here are some 
#requirements to help guide your design:
  
# - You must have clear x and y axis labels
#- The chart needs a clear title 
#- If you choose to add a color encoding (not required), you need a legend for your different color and a clear legend title

Chart_2 <- function(incarceration_data){
  largest_hundred <- incarceration_data %>%
    filter(year == max(year)) %>%
    arrange(female_jail_pop) %>%
    select(fips, female_jail_pop) %>%
    na.omit() %>%
    pull(fips) %>%
    tail(1000)
  incarceration_data %>%
   filter(fips %in% largest_hundred) %>%
    filter(year == max(year)) %>%
    select(black_jail_pop, Area = urbanicity, female_jail_pop) %>%
    na.omit() %>%
    ggplot(aes(black_jail_pop, female_jail_pop, color = Area))+
    geom_point()+
    scale_x_log10()+
    scale_y_log10()+
    labs(x = "Black Jail Population", y = "Female Jail Population", title = "Comparing Black and Female Jail Populations", subtitle = "Largest One Thousand Counties")
  
}