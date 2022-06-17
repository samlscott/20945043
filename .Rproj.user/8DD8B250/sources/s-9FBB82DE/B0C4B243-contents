netflix_code <- function(titles)
# show vs movie

library(dplyr)
show_movie = titles %>%
    group_by(release_year) %>%
    select(type, imdb_score) %>%
    na.omit(.)

show_movie_1 <- show_movie %>%
    group_by(type) %>%
    summarize(ave_score = mean(imdb_score)) %>%
    na.omit(.)

# Genres

library(dplyr)
genre = titles %>%
    group_by(genres) %>%
    select(genres, imdb_score, release_year) %>%
    na.omit(.)

genre1 <- genre %>%
    group_by(genres) %>%
    summarize(ave_score = mean(imdb_score)) %>%
    na.omit(.)

genre_new <- genre1[order(genre1$ave_score, decreasing = TRUE), ]

genre_new1 <- top_n(genre_new, 10)
