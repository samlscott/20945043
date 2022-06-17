netflixcode <- function(titles){


library(dplyr)
#Grouping according to movies and series
shows_movies = titles %>%
    group_by(type) %>%
    summarize(tmdb_score, imdb_score) %>%
    na.omit(.) %>%
    ungroup()
#Plotting correlation graph of scores
plot <- ggplot(shows_movies, aes(x=tmdb_score, y=imdb_score, color = type)) +
  geom_point(size=0.9) +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 12, 1)) +
  geom_smooth(method=lm, se =FALSE) +
  theme_bw() + theme(legend.position = "bottom") + labs(x = "TMDB score",
                                                        y = "IMDB score", title = "Ratings", subtitle = "Ratings of movies and series"
                                                        )
print(plot)
}
