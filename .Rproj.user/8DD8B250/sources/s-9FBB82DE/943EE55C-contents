
london_code <- function(london_weather, SUN_Hours){

    L_weather_clean <- london_weather[!is.na(london_weather$mean_temp), ]

    # fix the date
london_weather$date <- as.Date(as.character(london_weather$date),format = "%Y%m%d")

# sunny days, and max temp
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

# mean temp: London vs SA

library(dplyr)

L_weather <- L_weather_clean %>%
    mutate(month = as.Date(as.character(L_weather_clean$date),format = "%Y%m%d")) %>%
    filter(month > as.Date("2019-12-31")) %>%
    mutate(month = lubridate::month(month)) %>%
    group_by(month) %>%
    summarise(mean_temp = mean(mean_temp)) %>%
    add_column(Location = "London")

S_weather <- SUN_Hours %>%
    mutate(month = lubridate::month(as.Date(as.POSIXct(TmStamp)))) %>%
    group_by(month) %>%
    na.omit(.) %>%
    summarise(mean_temp = mean(AirTC_Avg))%>%
    add_column(Location = "Stellenbosch")


weather_results <- rbind(L_weather%>%group_by(mean_temp),S_weather%>%group_by(mean_temp))

ggplot(weather_results, aes(x=month, y=mean_temp,fill = Location)) + geom_bar(stat="identity", position="dodge") +
    scale_x_continuous(breaks = seq(0, 12, 1)) +
    scale_fill_manual(values = c("London" = "blue", "Stellenbosch" = "coral2")) +
    theme_bw() + theme(legend.position = "bottom") + labs(x = "Month",y = "Mean temperature",
                                                          title = "Mean temperature",subtitle = "Comparison between Stellenbosch and London",
                                                          caption = "Note:Sauran external data used for Stellenbosch")

}

