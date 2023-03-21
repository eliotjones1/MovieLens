library(caret)
library(tidyverse)
library(dslabs)


#Build a RMSE Function: 
RMSE <- function(validation_ratings, predicted_ratings){
  sqrt(mean((validation_ratings - predicted_ratings)^2))
}

#Test algorithm: 
mu <- mean(edx$rating)
rmse_1 <- RMSE(validation$rating, mu)
rmse_1
rmse_results <- tibble(method = "Just the average", RMSE = rmse_1)
rmse_results


#Using movieId as a predictor:
mu <- mean(edx$rating) 
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
predicted_ratings <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
model_1_rmse <- RMSE(validation$rating, predicted_ratings)

#Using userId and movieId together
user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
model_2_rmse <- RMSE(validation$rating, predicted_ratings)
rmse_results <- tibble(jta = rmse_1,
                       movie  = model_1_rmse,
                       user  = model_2_rmse)
rmse_results
#Regularized movieId
lambda <- 3
mu <- mean(edx$rating)
movie_reg_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())
predicted_ratings <- validation %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- tibble(jta = rmse_1,
                       movie  = model_1_rmse,
                       user  = model_2_rmse,
                       movie_reg = model_3_rmse)
rmse_results

#Regularized movieId and userId
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, validation$rating))
})

lambda <- lambdas[which.min(rmses)]
lambda <- 3.5
mu <- mean(edx$rating)

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

f_d <- edx %>% mutate(year = year(as_datetime(timestamp))) %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  group_by(year) %>% 
  summarize(f_d = (mean(rating - mu - b_i - b_u)))

predicted_ratings <- 
  validation %>% mutate(year = year(as_datetime(timestamp))) %>% 
  left_join(b_i, by = 'movieId') %>%
  left_join(b_u, by = 'userId') %>%
  left_join(f_d, by='year') %>%
  mutate(pred = mu + b_i + b_u + f_d) %>%
  .$pred
RMSE(validation$rating, predicted_ratings)  




mu <- mean(edx$rating)

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu))

f_d <- edx %>% mutate(year = year(as_datetime(timestamp))) %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  group_by(year) %>% 
  summarize(f_d = mean(rating - mu - b_i - b_u))

predicted_ratings <- 
  validation %>% mutate(year = year(as_datetime(timestamp))) %>% 
  left_join(b_i, by = 'movieId') %>%
  left_join(b_u, by = 'userId') %>%
  left_join(f_d, by='year') %>%
  mutate(pred = mu + b_i + b_u + f_d) %>%
  .$pred
RMSE(validation$rating, predicted_ratings)  


