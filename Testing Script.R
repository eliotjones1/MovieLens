#Test Script
train_set %>% mutate(year = year(as_datetime(timestamp))) %>%  ggplot(aes(year, rating)) + geom_boxplot()



user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

#YESSIR

all_avgs <- train_set %>% mutate(year = year(as_datetime(timestamp))) %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(year) %>% 
  summarize(f_d = (mean(rating - mu - b_i - b_u)))
predicted_avgs <- test_set %>% mutate(year = year(as_datetime(timestamp))) %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(all_avgs, by='year') %>%
  mutate(pred = mu + b_i + b_u + f_d) %>%
  .$pred
RMSE(test_set$rating, predicted_avgs)






