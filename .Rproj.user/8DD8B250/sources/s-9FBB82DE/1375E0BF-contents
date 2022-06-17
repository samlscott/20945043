
line_graph_continents <- function(owid_covid_data){

 library(dplyr)
 df_cont = owid_covid_data %>%
     group_by(continent) %>%
     select(total_cases, date) %>%
     na.omit(.)


 library(ggplot2)
 plot_df_cont <- df_cont
 g <- plot_df_cont %>%
     ggplot() +
     geom_line(aes(x = date, y = total_cases, color = continent), alpha = 0.3,
               size = 0.5) +
     theme_bw() + theme(legend.position = "bottom") + labs(x = "Date",
                                                                y = "Total Cases", title = "Covid per Continent", subtitle = "Total number of cases per contintent from beginning 2020",
                                                                caption = "Note:OWID data used")
 print(g)

}
