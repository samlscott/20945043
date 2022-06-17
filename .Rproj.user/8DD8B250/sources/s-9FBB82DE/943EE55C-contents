
# fix the date
london_weather$date <- as.Date(as.character(london_weather$date),format = "%Y%m%d")

library(dplyr)
lond_sunny = london_weather %>%
    group_by(date) %>%
    select(max_temp, sunshine) %>%
    na.omit(.)

library(ggplot2)
ggplot(lond_sunny, aes(x=sunshine, y=max_temp)) +
    geom_point( alpha = 0.3,
               size = 0.5, colour = "light blue")+
    geom_smooth(method=lm)+
    theme_bw() + theme(legend.position = "bottom") + labs(x = "Amount of sun",
                                                          y = "Max temp", title = "Max temp according to sunshine", subtitle = "Even when sunny, still coooold")



