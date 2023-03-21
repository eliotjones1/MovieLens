#Create the Dataset
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

##############################################################
##############################################################

#Splitting the edx dataset into train and test sets
set.seed(755, sample.kind = "Rounding")
y <- edx$rating
edx_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-edx_index,]
test_set <- edx[edx_index,]
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#Exploring the training data
library(caret)
library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)
n_distinct(train_set$userId)
n_distinct(train_set$movieId)
n_distinct(train_set$genres)

#Calculating average rating for four predictors
movieId_avgs <- train_set %>% group_by(movieId) %>% summarize(mean = mean(rating))
userId_avgs <- train_set %>% group_by(userId) %>% summarize(mean = mean(rating))
genre_avgs <- train_set %>% group_by(genres) %>% summarize(mean = mean(rating), n = n()) %>% filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, mean))
year_rated_avgs <- train_set %>% mutate(year = year(as_datetime(timestamp))) %>% group_by(year) %>% summarize(mean = mean(rating))

#Extracting and cleaning for Release Year
year_released <- str_extract(train_set$title, "\\d{4}") %>% as.numeric()
train_setRD <- train_set %>% mutate(release_year = year_released) %>% select(-timestamp)
train_setRD %>% filter(release_year > 2018) %>% select(movieId, release_year, title) %>% group_by(movieId, title, release_year) %>% summarize(n = n())
train_setRD %>% filter(release_year < 1900) %>% select(movieId, release_year, title) %>% group_by(movieId, title, release_year) %>% summarize(n = n())
#Fixing incorrect years (Too Large)
train_setRD[train_setRD$movieId == "671", "release_year"] <- 1996
train_setRD[train_setRD$movieId == "2308", "release_year"] <- 1973
train_setRD[train_setRD$movieId == "4159", "release_year"] <- 2001
train_setRD[train_setRD$movieId == "5310", "release_year"] <- 1985
train_setRD[train_setRD$movieId == "8864", "release_year"] <- 2004
train_setRD[train_setRD$movieId == "27266", "release_year"] <- 2004

#Fixing Incorrect Years (Too Small)
train_setRD[train_setRD$movieId == "1422", "release_year"] <- 1997
train_setRD[train_setRD$movieId == "4311", "release_year"] <- 1998
train_setRD[train_setRD$movieId == "5472", "release_year"] <- 1972
train_setRD[train_setRD$movieId == "6290", "release_year"] <- 2003
train_setRD[train_setRD$movieId == "6645", "release_year"] <- 1971
train_setRD[train_setRD$movieId == "8198", "release_year"] <- 1960
train_setRD[train_setRD$movieId == "8905", "release_year"] <- 1992
train_setRD[train_setRD$movieId == "53953", "release_year"] <- 2007
head(train_setRD)
release_date <- train_setRD %>% select(release_year) 

year_released_avgs <- train_set %>% mutate(release_year = release_date$release_year) %>% group_by(release_year) %>% summarize(mean = mean(rating))

#Graphing average rating for predictors
userId_avgs %>% ggplot(aes(userId, mean)) + geom_point() + ggtitle("UserId vs Average User Rating")

movieId_avgs %>% ggplot(aes(movieId, mean)) + geom_point() + ggtitle("MovieId vs Average Rating per Movie")

genre_avgs %>% ggplot(aes(genres, mean)) + geom_point() + ggtitle("Genre vs Average Rating per Genre")
genre_avgs$genres[which.max(genre_avgs$mean)]
genre_avgs %>% filter(genres == "Action|Crime|Drama|IMAX")

year_rated_avgs %>% ggplot(aes(year, mean)) + geom_point() + ggtitle("Year Rated vs Average Rating per Year")

year_released_avgs %>% ggplot(aes(release_year, mean)) + geom_point()  + ggtitle("Release Year vs Average Rating per Year")


#Testing the relationship between Month, Week, Day, and Average Rating
year_rated_avgs <- train_set %>% mutate(year = year(as_datetime(timestamp))) %>% group_by(year) %>% summarize(mean = mean(rating))
year_rated_avgs %>% ggplot(aes(year, mean)) + geom_point() + ggtitle("Year Rated vs Average Rating per Year")

month_rated_avgs <- train_set %>% mutate(month = month(as_datetime(timestamp))) %>% group_by(month) %>% summarize(mean= mean(rating))
week_rated_avgs <- train_set %>% mutate(week = week(as_datetime(timestamp))) %>% group_by(week) %>% summarize(mean = mean(rating))
day_rated_avgs <- train_set %>% mutate(day = day(as_datetime(timestamp))) %>% group_by(day) %>% summarize(mean = mean(rating))

month_rated_avgs %>% ggplot(aes(month, mean)) + geom_point() + ggtitle("Month vs Average Rating per Month")
week_rated_avgs %>% ggplot(aes(week, mean)) + geom_point() + ggtitle("Week vs Average Rating per Month")
day_rated_avgs %>% ggplot(aes(day, mean)) + geom_point() + ggtitle("Day vs Average Rating per Month")

#Building the RMSE Function:
RMSE <- function(validation_ratings, predicted_ratings){
  sqrt(mean((validation_ratings - predicted_ratings)^2))
}

#Using the train set to determine which algorithm to use, with y_hat = mu + b_i + b_u
#1 - y_hat = mu
mu <- mean(train_set$rating)
rmse_1 <- RMSE(test_set$rating, mu)
print(rmse_1)

#2 - y_hat = mu + b_i
print(mu)
movieId_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
movieId_preds <- mu + test_set %>% 
  left_join(movieId_avgs, by='movieId') %>%
  pull(b_i)
rmse_2 <- RMSE(test_set$rating, movieId_preds)
print(rmse_2)

#3 - y_hat = mu + b_i + b_u
userId_avgs <- train_set %>%
  left_join(movieId_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
userId_preds <- test_set %>%
  left_join(movieId_avgs, by='movieId') %>%
  left_join(userId_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
rmse_3 <- RMSE(test_set$rating, userId_preds)
print(rmse_3)


#5 - y_hat = mu + b_i + b_u with regularization
lambdas <- seq(0, 10, 0.25)
rmses_2 <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  userId_preds_reg <- test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(test_set$rating, userId_preds_reg))
})
min(rmses_2)
lambdas[which.min(rmses_2)]

#Using regularization on the validation set 
lambda <-  4.75
mu <- mean(edx$rating)

movieId_final <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

userId_final <- edx %>% 
  left_join(movieId_final, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

predicted_ratings_final <- validation %>% 
  left_join(movieId_final, by = "movieId") %>%
  left_join(userId_final, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
RMSE(validation$rating, predicted_ratings_final)  

0.86490 > .8648201




