
london_code <- function(london_weather, x,
                                  y, title, subtitle, colour, format){
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
mean_temp_lond = london_weather %>%
    group_by(date) %>%
    filter(date > as.Date("2019-12-31")) %>%
    group_by() %>%
    select(date, mean_temp) %>%
    na.omit(.)

df <- mean_temp_lond %>%
    mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
    group_by(month, year) %>%
    summarize(mean_temp = mean(mean_temp)) %>%
    na.omit(.)

df1 = df %>%
    group_by(month)%>%
    select(mean_temp)


data_bar <- df1$mean_temp
names(data_bar) <- df1$month

barplot(data_bar,
        main = "Average Temperature for 2020",
        xlab = "Month",
        ylab = "Temperature",
        col = "#1b98e0")


}

