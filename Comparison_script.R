if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
# Package "tidyverse" has many uses when manipulating data
library(tidyverse)
# Package "caret" has many uses when working with machine learning
library(caret)
# Package "data.table" has many uses when working with tables
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(data.table)
# Package "ggplot2" is used for data visualization
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
library(ggplot2)
# Package "Rcpp" (required when testing the script in other computers)
if(!require(Rcpp)) install.packages("Rcpp", repos = "http://cran.us.r-project.org")
library(Rcpp)
# Package "lubridate" is used with the timestamp in order to handle date-time
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
library(lubridate)
# Package "recosystem" is a Recommender System used on the analysis 
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")
library(recosystem)
# Package "digest" is used to manipulate hash codes
if(!require(digest)) install.packages("digest", repos = "http://cran.us.r-project.org")
library(digest)

################################################################################
################################################################################
# MOVIES
################################################################################
################################################################################

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)

movies_df <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in movies_df set
validation <- temp %>% 
  semi_join(movies_df, by = "movieId") %>%
  semi_join(movies_df, by = "userId")

# Add rows removed from validation set back into movies_df set
removed <- anti_join(temp, validation)
movies_df <- rbind(movies_df, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

################################################################################
# DATASET SAMPLING; TRAINING AND VALIDATION DATA FRAMES
################################################################################
# Creating training and test data frames
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movies_df$rating, times = 1, p = 0.1, list = FALSE)
training_dataframe <- movies_df[-test_index,]
temp <- movies_df[test_index,]
# Make sure userId and movieId in training_dataframe are also in test_dataframe
test_dataframe <- temp %>%
  semi_join(training_dataframe, by = "movieId") %>%
  semi_join(training_dataframe, by = "userId")
# Add rows removed from test_dataframe set back into training_dataframe
# Adding back rows into train set
removed <- anti_join(temp, test_dataframe)
training_dataframe <- rbind(training_dataframe, removed)
rm(test_index, temp, removed)
################################################################################
# movies_df GENERAL OVERVIEW
################################################################################
# As function sumary() returns no useful information about 'movies_df' data frame, 
# it was manually processed.
# movies_df has 10677 movies.
length(unique(movies_df$movieId))
# movies_df has 69878 unique userIds, which means that 69878 users rated the movies.
length(unique(movies_df$userId))
# The highest rating is 5.
max(movies_df$rating)
# The rating "5" was given 1390114 times.
sum(movies_df$rating==5)
# The lowest rating is 0.5
min(movies_df$rating)
# The rating "0.5" was given 85374 times.
sum(movies_df$rating==0.5)
# The average rating of movies_df 3.512465.
mean(movies_df$rating)
# The standard deviation rating of movies_df is 1.060331.
sd(movies_df$rating)

# Ratings histogram
ratings_qty_movies <- movies_df %>% group_by(rating) %>%
  summarize(count = n())
ratings_histogram_movies <- ratings_qty_movies %>% mutate(rating = factor(rating)) %>%
  ggplot(aes(rating, count)) +
  geom_col(fill = "gray")+
  theme_minimal() + 
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Ratings", y = "Qty",
       tag = "Fig. 1 - Histogram showing the quantity of each given rating (movies).") +
  theme(plot.tag.position = "bottom")+ 
  ggtitle("Histogram of ratings")

# Creating a summary table grouped by userId
ratings_per_user_movies <- movies_df %>% group_by(userId) %>%
  summarize(ratings_per_user_qty = n(),
            ratings_given_mean = mean(rating),
            mu_user = mean(rating),
            sd_user = sd(rating),)

# Average of 128.7967 ratings given per user
mean(ratings_per_user_movies$ratings_per_user_qty)

# Histogram showing the number of users per given ratings
ratings_user_hist_movies <- movies_df %>% group_by(userId) %>%
  summarize(count = n()) %>%
  ggplot(aes(count)) +
  geom_histogram(color = "black", fill = "gray", bins = 30) +
  geom_vline(aes(xintercept = mean(ratings_per_user_movies$ratings_per_user_qty)), color = "red", linetype="dashed", size=1)+
  annotate("text", x = 215, y = 5000, label = "avg=128.79",
           color = "red", size = 4, fontface =2) +
  theme_minimal()+
  scale_x_log10() +
  labs(title = "Histogram - ratings per user", x = "Ratings given", 
       y = "Users", tag = "Fig. 2 - Histogram showing the quantity of users and rating given (movies).")+
  theme(plot.tag.position = "bottom")

# Histogram showing the number of ratings per year
ratings_year_histogram_movies <- movies_df %>% mutate(release = round_date(as_datetime(timestamp), unit = "year")) %>% 
  group_by(release) %>%
  summarize(count = n()) %>%
  ggplot(aes(release, count)) +
  geom_col(fill = "gray")+
  theme_minimal()+
  scale_y_log10(labels = scales::comma) +
  ggtitle("Timestamp, time unit : year") +
  labs(title = "Histogram - ratings per year", x = "Year", 
       y = "Qty", tag = "Fig. 3 - Histogram showing the quantity of ratings per year (movies).")+
  theme(plot.tag.position = "bottom")

################################################################################
################################################################################
###TESTS MOVIELENS
################################################################################
################################################################################

################################################################################
###LINEAR MODELS
################################################################################
# lm() function using timestamp as a predictor.
model <- lm(formula=rating ~ timestamp, data = training_dataframe)
prediction_movies_lm_test_ts <- predict(model, newdata = test_dataframe)
rmse_movies_lm_test_ts <- RMSE(prediction_movies_lm_test_ts, test_dataframe$rating)
model <- lm(formula=rating ~ timestamp, data = movies_df)
prediction_validation_ts <- predict(model, newdata = validation)
rmse_movies_lm_validation_ts <- RMSE(prediction_validation_ts, validation$rating)
# lm() function using timestamp and userId as predictors.
model <- lm(formula=rating ~ timestamp+userId, data = training_dataframe)
prediction_movies_lm_test_ts_ui <- predict(model, newdata = test_dataframe)
rmse_movies_lm_test_ts_ui <- RMSE(prediction_movies_lm_test_ts_ui, test_dataframe$rating)
model <- lm(formula=rating ~ timestamp+userId, data = movies_df)
prediction_movies_lm_validation_ts_ui <- predict(model, newdata = validation)
rmse_movies_lm_validation_ts_ui <- RMSE(prediction_movies_lm_validation_ts_ui, validation$rating)
# lm() function using timestamp, userId and movieId as predictors.
model <- lm(formula=rating ~ timestamp+userId+movieId, data = training_dataframe)
prediction_movies_lm_test_ts_ui_mi <- predict(model, newdata = test_dataframe)
rmse_movies_lm_test_ts_ui_mi <- RMSE(prediction_movies_lm_test_ts_ui_mi, test_dataframe$rating)
model <- lm(formula=rating ~ timestamp+userId+movieId, data = movies_df)
prediction_movies_lm_validation_ts_ui_mi <- predict(model, newdata = validation)
rmse_movies_lm_validation_ts_ui_mi <- RMSE(prediction_movies_lm_validation_ts_ui_mi, validation$rating)

# Compiling all results and and showing them in a table
results_movies_lm <- tibble(predictors = "timestamp", RMSE = rmse_movies_lm_test_ts, type = "test")
results_movies_lm <- bind_rows(results_movies_lm, tibble(predictors = "timestamp", RMSE = rmse_movies_lm_validation_ts, type = "validation"))
results_movies_lm <- bind_rows(results_movies_lm, tibble(predictors = "timestamp and userId", RMSE = rmse_movies_lm_test_ts_ui, type = "test"))
results_movies_lm <- bind_rows(results_movies_lm, tibble(predictors = "timestamp and userId", RMSE = rmse_movies_lm_validation_ts_ui, type = "validation"))
results_movies_lm <- bind_rows(results_movies_lm, tibble(predictors = "timestamp, userId and movieId", RMSE = rmse_movies_lm_test_ts_ui_mi, type = "test"))
results_movies_lm <- bind_rows(results_movies_lm, tibble(predictors = "timestamp, userId and movieId", RMSE = rmse_movies_lm_validation_ts_ui_mi, type = "validation"))
results_movies_lm %>% knitr::kable()

rm(model)
################################################################################
###RECOSYSTEM
################################################################################
set.seed(1, sample.kind="Rounding")
# Test and validation data frames to use with "recosystem" library
train_recosys <- with(training_dataframe, data_memory(user_index = userId, item_index = movieId, rating = rating))
test_recosys <- with(test_dataframe, data_memory(user_index = userId, item_index = movieId, rating = rating))
movies_df_recosys <- with(movies_df, data_memory(user_index = userId, item_index = movieId, rating = rating))
validation_recosys <- with(validation, data_memory(user_index = userId, item_index = movieId, rating = rating))

# Creating an object of class RecoSys called "r"
r <- Reco()
# Setting tuning parameters
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 1))
# Training stage
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 1))
# Predicting results
prediction_recosys <- r$predict(test_recosys, out_memory())
# Calculating RMSE
rmse_1_iter_movies_tests <- RMSE(prediction_recosys, test_dataframe$rating)
# Printing RMSE
rmse_1_iter_movies_tests

# Setting tuning parameters
tuning_parameters <- r$tune(movies_df_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 1))
# Training stage
r$train(movies_df_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 1))
# Predicting results
prediction_recosys <- r$predict(validation_recosys, out_memory())
# Calculating RMSE
rmse_1_iter_movies_final <- RMSE(prediction_recosys, validation$rating)
# Printing RMSE
rmse_1_iter_movies_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 2))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 2))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_2_iter_movies_tests <- RMSE(prediction_recosys, test_dataframe$rating)
rmse_2_iter_movies_tests
tuning_parameters <- r$tune(movies_df_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 2))
r$train(movies_df_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 2))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_2_iter_movies_final <- RMSE(prediction_recosys, validation$rating)
rmse_2_iter_movies_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 3))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 3))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_3_iter_movies_tests <- RMSE(prediction_recosys, test_dataframe$rating)
rmse_3_iter_movies_tests
tuning_parameters <- r$tune(movies_df_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 3))
r$train(movies_df_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 3))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_3_iter_movies_final <- RMSE(prediction_recosys, validation$rating)
rmse_3_iter_movies_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 4))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 4))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_4_iter_movies_tests <- RMSE(prediction_recosys, test_dataframe$rating)
rmse_4_iter_movies_tests
tuning_parameters <- r$tune(movies_df_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 4))
r$train(movies_df_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 4))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_4_iter_movies_final <- RMSE(prediction_recosys, validation$rating)
rmse_4_iter_movies_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 5))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 5))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_5_iter_movies_tests <- RMSE(prediction_recosys, test_dataframe$rating)
rmse_5_iter_movies_tests
tuning_parameters <- r$tune(movies_df_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 5))
r$train(movies_df_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 5))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_5_iter_movies_final <- RMSE(prediction_recosys, validation$rating)
rmse_5_iter_movies_final


# Compiling all results and and showing them in a table
results_recosys_movies <- tibble(iterations = 1, RMSE = rmse_1_iter_movies_tests, type = "test")
results_recosys_movies <- bind_rows(results_recosys_movies, tibble(iterations = 1, RMSE = rmse_1_iter_movies_final, type = "validation"))
results_recosys_movies <- bind_rows(results_recosys_movies, tibble(iterations = 2, RMSE = rmse_2_iter_movies_tests, type = "test"))
results_recosys_movies <- bind_rows(results_recosys_movies, tibble(iterations = 2, RMSE = rmse_2_iter_movies_final, type = "validation"))
results_recosys_movies <- bind_rows(results_recosys_movies, tibble(iterations = 3, RMSE = rmse_3_iter_movies_tests, type = "test"))
results_recosys_movies <- bind_rows(results_recosys_movies, tibble(iterations = 3, RMSE = rmse_3_iter_movies_final, type = "validation"))
results_recosys_movies <- bind_rows(results_recosys_movies, tibble(iterations = 4, RMSE = rmse_4_iter_movies_tests, type = "test"))
results_recosys_movies <- bind_rows(results_recosys_movies, tibble(iterations = 4, RMSE = rmse_4_iter_movies_final, type = "validation"))
results_recosys_movies <- bind_rows(results_recosys_movies, tibble(iterations = 5, RMSE = rmse_5_iter_movies_tests, type = "test"))
results_recosys_movies <- bind_rows(results_recosys_movies, tibble(iterations = 5, RMSE = rmse_5_iter_movies_final, type = "validation"))

results_recosys_movies %>% knitr::kable()

# Plotting the results
results_recosys_movies_lineplot <- results_recosys_movies %>% 
  group_by(type) %>%
  ggplot( aes(x=iterations, y=RMSE, group=type, color=type)) +
  theme_minimal() + geom_line() +
  geom_hline(aes(yintercept = 0.865), color = "red", linetype="dashed", size=1)+
  annotate("text", x = 3, y = 0.87, label = "RMSE=8649 (goal)",
           color = "red", size = 4, fontface =2) +
  ggtitle("Timestamp, time unit : year") +
  labs(title = "recosystem - RMSE vs. iterations", x = "iterations", y = "RMSE", 
       tag = "Fig. 12 - RMSE of with different iterations using recosystem (movies).")+
  theme(plot.tag.position = "bottom")

################################################################################
################################################################################
# BEAUTY PRODUCTS
################################################################################
################################################################################

# DATASET DESCRIPTION:
# This is a dataset related to over 2 Million customer reviews and ratings of 
# Beauty related products sold on Amazon website.
# Downloaded from https://www.kaggle.com/skillsmuggler/amazon-ratings

# Downloading the dataset from Dropbox
temp <- read.csv("https://www.dropbox.com/s/edu7fcy7fx3xmpb/ratings_Beauty.csv?dl=1")
# Removing all NAs
temp <- na.omit(temp)
# Removing empty scores/ratings
beauty <- temp %>% filter(Rating != "") %>% droplevels()
# Casting UserId and ProductId as characters (in order to avoid some errors)
beauty$UserId <- as.character(beauty$UserId)
beauty$ProductId <- as.character(beauty$ProductId)
# Converting UserId and ProductId to numbers (they are originally hash codes)
beauty$UserId <- as.numeric(digest::digest2int(beauty$UserId))
beauty$ProductId <- as.numeric(digest::digest2int(beauty$ProductId))
# Ordering the data frame by the UserId
beauty<-as.data.frame(beauty[order(beauty$UserId),])
# As both UserId and ProductId are very large numbers, it was decided to 
# change it to their respective ranks, e.g.: UserId "1082878268" could be converted 
# to "14" or ProductId "568411023" could be converted to "1023" etc.
rownames(beauty) <- c(1:nrow(beauty))
beauty$UserId<-round(rank(beauty$UserId, ties.method = "average"))
beauty$ProductId<-round(rank(beauty$ProductId , ties.method = "average"))

################################################################################
# DATASET SAMPLING; TRAINING AND VALIDATION DATA FRAMES
################################################################################
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = beauty$Rating, times = 1, p = 0.1, list = FALSE)

beauty_df <- beauty[-test_index,]
temp <- beauty[test_index,]

# Make sure UserId and ProductId in validation set are also in beauty_df set
validation <- temp %>% 
  semi_join(beauty_df, by = "ProductId") %>%
  semi_join(beauty_df, by = "UserId")

# Add rows removed from validation set back into beauty_df set
removed <- anti_join(temp, validation)
beauty_df <- rbind(beauty_df, removed)

rm(test_index, temp, beauty, removed)
################################################################################
# beauty_df GENERAL OVERVIEW
################################################################################
# As function sumary() returns no useful information about 'beauty_df' data frame, 
# it was manually processed.
# beauty_df has 249269 products.
length(unique(beauty_df$ProductId))
# beauty_df has 1210084 unique UserIds, which means that 1210084 users rated the movies.
length(unique(beauty_df$UserId))
# The highest rating is 5.
max(beauty_df$Rating)
# The rating "5" was given 1185304 times.
sum(beauty_df$Rating==5)
# The lowest rating is 1
min(beauty_df$Rating)
# The rating "1" was given 176557 times.
sum(beauty_df$Rating==1)
# The average rating of beauty_df 4.146228.
mean(beauty_df$Rating)
# The standard deviation rating of beauty_df is 1.315409.
sd(beauty_df$Rating)

# Ratings histogram
ratings_qty_beauty <- beauty_df %>% group_by(Rating) %>%
  summarize(count = n())
ratings_histogram_beauty <- ratings_qty_beauty %>% mutate(Rating = factor(Rating)) %>%
  ggplot(aes(Rating, count)) +
  geom_col(fill = "gray")+ 
  theme_minimal() + 
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Ratings", y = "Qty",
       tag = "Fig. 4 - Histogram showing the quantity of each given rating (beauty products).") +
  theme(plot.tag.position = "bottom")+ 
  ggtitle("Histogram of ratings")


# Creating a summary table grouped by UserId
ratings_per_user_beauty <- beauty_df %>% group_by(UserId) %>%
  summarize(ratings_per_user_qty = n(),
            ratings_given_mean = mean(Rating),
            mu_user = mean(Rating),
            sd_user = sd(Rating),)

# Average of 1.586911 ratings given per user
mean(ratings_per_user_beauty$ratings_per_user_qty)

# Histogram showing the number of users per given ratings
ratings_user_hist_beauty <- beauty_df %>% group_by(UserId) %>%
  summarize(count = n()) %>%
  ggplot(aes(count)) +
  geom_histogram(color = "black", fill = "gray", bins = 30) +
  geom_vline(aes(xintercept = mean(ratings_per_user_beauty$ratings_per_user_qty)), color = "red", linetype="dashed", size=1)+
  annotate("text", x = 2.2, y = 250000, label = "avg=1.586",
           color = "red", size = 4, fontface =2) +
  theme_minimal()+
  scale_y_log10(labels = scales::comma) +
  xlim(0,10) +
  labs(title = "Histogram - ratings per user", x = "Ratings given", 
       y = "Users", tag = "Fig. 5 - Histogram showing the quantity of users and Rating given (beauty products).")+
  theme(plot.tag.position = "bottom")

# Histogram showing the number of ratings per year
ratings_year_histogram_beauty <- beauty_df %>% mutate(release = round_date(as_datetime(Timestamp), unit = "year")) %>% 
  group_by(release) %>%
  summarize(count = n()) %>%
  ggplot(aes(release, count)) +
  geom_col(fill = "gray")+
  theme_minimal()+
  scale_y_log10(labels = scales::comma) +
  ggtitle("Timestamp, time unit : year") +
  labs(title = "Histogram - ratings per year", x = "Year", 
       y = "Qty", tag = "Fig. 6 - Histogram showing the quantity of ratings per year (beauty products).")+
  theme(plot.tag.position = "bottom")
################################################################################
# TRAINING AND TEST DATA FRAMES
################################################################################
# Creating training and test data frames
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = beauty_df$Rating, times = 1, p = 0.1, list = FALSE)
training_dataframe <- beauty_df[-test_index,]
temp <- beauty_df[test_index,]
# Make sure UserId and ProductId in training_dataframe are also in test_dataframe
test_dataframe <- temp %>%
  semi_join(training_dataframe, by = "ProductId") %>%
  semi_join(training_dataframe, by = "UserId")
# Add rows removed from test_dataframe set back into training_dataframe
# Adding back rows into train set
removed <- anti_join(temp, test_dataframe)
training_dataframe <- rbind(training_dataframe, removed)

################################################################################
###LINEAR MODELS
################################################################################
# lm() function using Timestamp as a predictor.
model <- lm(formula=Rating ~ Timestamp, data = training_dataframe)
prediction_lm_test_ts <- predict(model, newdata = test_dataframe)
rmse_lm_test_ts <- RMSE(prediction_lm_test_ts, test_dataframe$Rating)
model <- lm(formula=Rating ~ Timestamp, data = beauty_df)
prediction_validation_ts <- predict(model, newdata = validation)
rmse_lm_validation_ts <- RMSE(prediction_validation_ts, validation$Rating)
# lm() function using Timestamp and UserId as predictors.
model <- lm(formula=Rating ~ Timestamp+UserId, data = training_dataframe)
prediction_lm_test_ts_ui <- predict(model, newdata = test_dataframe)
rmse_lm_test_ts_ui <- RMSE(prediction_lm_test_ts_ui, test_dataframe$Rating)
model <- lm(formula=Rating ~ Timestamp+UserId, data = beauty_df)
prediction_lm_validation_ts_ui <- predict(model, newdata = validation)
rmse_lm_validation_ts_ui <- RMSE(prediction_lm_validation_ts_ui, validation$Rating)
# lm() function using Timestamp, UserId and ProductId as predictors.
model <- lm(formula=Rating ~ Timestamp+UserId+ProductId, data = training_dataframe)
prediction_lm_test_ts_ui_mi <- predict(model, newdata = test_dataframe)
rmse_lm_test_ts_ui_mi <- RMSE(prediction_lm_test_ts_ui_mi, test_dataframe$Rating)
model <- lm(formula=Rating ~ Timestamp+UserId+ProductId, data = beauty_df)
prediction_lm_validation_ts_ui_mi <- predict(model, newdata = validation)
rmse_lm_validation_ts_ui_mi <- RMSE(prediction_lm_validation_ts_ui_mi, validation$Rating)

# Compiling all results and and showing them in a table
results_beauty_lm <- tibble(predictors = "timestamp", RMSE = rmse_lm_test_ts, type = "test")
results_beauty_lm <- bind_rows(results_beauty_lm, tibble(predictors = "timestamp", RMSE = rmse_lm_validation_ts, type = "validation"))
results_beauty_lm <- bind_rows(results_beauty_lm, tibble(predictors = "timestamp and userId", RMSE = rmse_lm_test_ts_ui, type = "test"))
results_beauty_lm <- bind_rows(results_beauty_lm, tibble(predictors = "timestamp and userId", RMSE = rmse_lm_validation_ts_ui, type = "validation"))
results_beauty_lm <- bind_rows(results_beauty_lm, tibble(predictors = "timestamp, userId and movieId", RMSE = rmse_lm_test_ts_ui_mi, type = "test"))
results_beauty_lm <- bind_rows(results_beauty_lm, tibble(predictors = "timestamp, userId and movieId", RMSE = rmse_lm_validation_ts_ui_mi, type = "validation"))
results_beauty_lm %>% knitr::kable()
rm(model)
################################################################################
###RECOSYSTEM
################################################################################
set.seed(1, sample.kind="Rounding")
# Test and validation data frames to use with "recosystem" library
train_recosys <- with(training_dataframe, data_memory(user_index = UserId, item_index = ProductId, rating = Rating))
test_recosys <- with(test_dataframe, data_memory(user_index = UserId, item_index = ProductId, rating = Rating))
beauty_df_recosys <- with(beauty_df, data_memory(user_index = UserId, item_index = ProductId, rating = Rating))
validation_recosys <- with(validation, data_memory(user_index = UserId, item_index = ProductId, rating = Rating))

# Creating an object of class RecoSys called "r"
r <- Reco()
# Setting tuning parameters
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 1))
# Training stage
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 1))
# Predicting results
prediction_recosys <- r$predict(test_recosys, out_memory())
# Calculating RMSE
rmse_1_iter_tests <- RMSE(prediction_recosys, test_dataframe$Rating)
# Printing RMSE
rmse_1_iter_tests

# Setting tuning parameters
tuning_parameters <- r$tune(beauty_df_recosys, opts = list(dim = c(20, 30),
                                                           nthread = 4, niter = 1))
# Training stage
r$train(beauty_df_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 1))
# Predicting results
prediction_recosys <- r$predict(validation_recosys, out_memory())
# Calculating RMSE
rmse_1_iter_final <- RMSE(prediction_recosys, validation$Rating)
# Printing RMSE
rmse_1_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 2))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 2))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_2_iter_tests <- RMSE(prediction_recosys, test_dataframe$Rating)
rmse_2_iter_tests
tuning_parameters <- r$tune(beauty_df_recosys, opts = list(dim = c(20, 30),
                                                           nthread = 4, niter = 2))
r$train(beauty_df_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 2))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_2_iter_final <- RMSE(prediction_recosys, validation$Rating)
rmse_2_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 3))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 3))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_3_iter_tests <- RMSE(prediction_recosys, test_dataframe$Rating)
rmse_3_iter_tests
tuning_parameters <- r$tune(beauty_df_recosys, opts = list(dim = c(20, 30),
                                                           nthread = 4, niter = 3))
r$train(beauty_df_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 3))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_3_iter_final <- RMSE(prediction_recosys, validation$Rating)
rmse_3_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 4))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 4))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_4_iter_tests <- RMSE(prediction_recosys, test_dataframe$Rating)
rmse_4_iter_tests
tuning_parameters <- r$tune(beauty_df_recosys, opts = list(dim = c(20, 30),
                                                           nthread = 4, niter = 4))
r$train(beauty_df_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 4))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_4_iter_final <- RMSE(prediction_recosys, validation$Rating)
rmse_4_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 5))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 5))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_5_iter_tests <- RMSE(prediction_recosys, test_dataframe$Rating)
rmse_5_iter_tests
tuning_parameters <- r$tune(beauty_df_recosys, opts = list(dim = c(20, 30),
                                                           nthread = 4, niter = 5))
r$train(beauty_df_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 5))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_5_iter_final <- RMSE(prediction_recosys, validation$Rating)
rmse_5_iter_final


# Compiling all results and and showing them in a table
results_recosys_beauty <- tibble(iterations = 1, RMSE = rmse_1_iter_tests, type = "test")
results_recosys_beauty <- bind_rows(results_recosys_beauty, tibble(iterations = 1, RMSE = rmse_1_iter_final, type = "validation"))
results_recosys_beauty <- bind_rows(results_recosys_beauty, tibble(iterations = 2, RMSE = rmse_2_iter_tests, type = "test"))
results_recosys_beauty <- bind_rows(results_recosys_beauty, tibble(iterations = 2, RMSE = rmse_2_iter_final, type = "validation"))
results_recosys_beauty <- bind_rows(results_recosys_beauty, tibble(iterations = 3, RMSE = rmse_3_iter_tests, type = "test"))
results_recosys_beauty <- bind_rows(results_recosys_beauty, tibble(iterations = 3, RMSE = rmse_3_iter_final, type = "validation"))
results_recosys_beauty <- bind_rows(results_recosys_beauty, tibble(iterations = 4, RMSE = rmse_4_iter_tests, type = "test"))
results_recosys_beauty <- bind_rows(results_recosys_beauty, tibble(iterations = 4, RMSE = rmse_4_iter_final, type = "validation"))
results_recosys_beauty <- bind_rows(results_recosys_beauty, tibble(iterations = 5, RMSE = rmse_5_iter_tests, type = "test"))
results_recosys_beauty <- bind_rows(results_recosys_beauty, tibble(iterations = 5, RMSE = rmse_5_iter_final, type = "validation"))

results_recosys_beauty %>% knitr::kable()

results_recosys_beauty_lineplot <- results_recosys_beauty %>% 
  group_by(type) %>%
  ggplot( aes(x=iterations, y=RMSE, group=type, color=type)) +
  theme_minimal() + geom_line() +
  geom_hline(aes(yintercept = 0.865), color = "red", linetype="dashed", size=1)+
  annotate("text", x = 3, y = 0.95, label = "RMSE=8649 (goal)",
           color = "red", size = 4, fontface =2) +
  ggtitle("Timestamp, time unit : year") +
  labs(title = "recosystem - RMSE vs. iterations", x = "iterations", 
       y = "RMSE", tag = "Fig. 13 - RMSE of with different iterations using recosystem (beauty products).")+
  theme(plot.tag.position = "bottom")

################################################################################
################################################################################
# FINE FOODS
################################################################################
################################################################################

# DATASET DESCRIPTION:
# This dataset consists of reviews of fine foods from Amazon. The data span a 
# period of more than 10 years, including all ~500,000 reviews up to October 2012. 
# Reviews include product and user information, ratings, and a plain text review. 
# Downloaded from: https://www.kaggle.com/snap/amazon-fine-food-reviews

# Downloading the dataset from Dropbox
temp <- read.csv("https://www.dropbox.com/s/shi4b3iujyf2710/ratings_Food.csv?dl=1")
# Removing all NAs
temp <- na.omit(temp)
# Removing empty scores/ratings
food <- temp %>% filter(Score != "") %>% droplevels()
# Casting UserId and ProductId as characters (in order to avoid some errors)
food$UserId <- as.character(food$UserId)
food$ProductId <- as.character(food$ProductId)
# Dropping unused columns
food$ProfileName <- NULL
food$HelpfulnessNumerator <- NULL
food$HelpfulnessDenominator <- NULL
food$Summary <- NULL 
food$Text <- NULL
# Renaming column "Score" as "Rating"
food <- food %>% rename(Rating = Score)
# Renaming column "Time" as "Timestamp"
food <- food %>% rename(Timestamp = Time)
# Converting UserId and ProductId to numbers (they are originally hash codes)
food$UserId <- as.numeric(digest::digest2int(food$UserId))
food$ProductId <- as.numeric(digest::digest2int(food$ProductId))
# Ordering the data frame by the UserId
food<-as.data.frame(food[order(food$UserId),])
# As both UserId and ProductId are very large numbers, it was decided to 
# change it to their respective ranks, e.g.: UserId "1082878268" could be converted 
# to "14" or ProductId "568411023" could be converted to "1023" etc.
rownames(food) <- c(1:nrow(food))
food$UserId<-round(rank(food$UserId, ties.method = "average"))
food$ProductId<-round(rank(food$ProductId , ties.method = "average"))

################################################################################
# DATASET SAMPLING; TRAINING AND VALIDATION DATA FRAMES
################################################################################
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = food$Rating, times = 1, p = 0.1, list = FALSE)

food_df <- food[-test_index,]
temp <- food[test_index,]

# Make sure UserId and ProductId in validation set are also in food_df set
validation <- temp %>% 
  semi_join(food_df, by = "ProductId") %>%
  semi_join(food_df, by = "UserId")

# Add rows removed from validation set back into food_df set
removed <- anti_join(temp, validation)
food_df <- rbind(food_df, removed)

rm(test_index, temp, food, removed)

################################################################################
# food_df GENERAL OVERVIEW
################################################################################
# As function sumary() returns no useful information about 'food_df' data frame, 
# it was manually processed.
# food_df has 74258 products.
length(unique(food_df$ProductId))
# food_df has 256047 unique userIds, which means that 69878 256047 rated the products.
length(unique(food_df$UserId))
# The highest rating is 5.
max(food_df$Rating)
# The rating "5" was given 339932 times.
sum(food_df$Rating==5)
# The lowest rating is 1
min(food_df$Rating)
# The rating "1" was given 49259 times.
sum(food_df$Rating==1)
# The average rating of food_df 4.182434.
mean(food_df$Rating)
# The standard deviation rating of food_df is 1.31313.
sd(food_df$Rating)

# Ratings histogram
ratings_qty_food <- food_df %>% group_by(Rating) %>%
  summarize(count = n())
ratings_histogram_food <- ratings_qty_food %>% mutate(Rating = factor(Rating)) %>%
  ggplot(aes(Rating, count)) +
  geom_col(fill = "gray")+ 
  theme_minimal() + 
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Ratings", y = "Qty",
       tag = "Fig. 7 - Histogram showing the quantity of each given rating (fine foods).") +
  theme(plot.tag.position = "bottom")+ 
  ggtitle("Histogram of ratings")

# Creating a summary table grouped by UserId
ratings_per_user_food <- food_df %>% group_by(UserId) %>%
  summarize(ratings_per_user_qty = n(),
            ratings_given_mean = mean(Rating),
            mu_user = mean(Rating),
            sd_user = sd(Rating),)

# Average of 2.07565 ratings given per user
mean(ratings_per_user_food$ratings_per_user_qty)

# Histogram showing the number of users per given ratings
ratings_user_hist_food <- food_df %>% group_by(UserId) %>%
  summarize(count = n()) %>%
  ggplot(aes(count)) +
  geom_histogram(color = "black", fill = "gray", bins = 30) +
  geom_vline(aes(xintercept = mean(ratings_per_user_food$ratings_per_user_qty)), color = "red", linetype="dashed", size=1)+
  annotate("text", x = 3, y = 250000, label = "avg=2.075",
           color = "red", size = 4, fontface =2) +
  theme_minimal()+
  xlim(0,15) +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Histogram - ratings per user", x = "Ratings given", 
       y = "Users", tag = "Fig. 8 - Histogram showing the quantity of users and rating given (fine foods).")+
  theme(plot.tag.position = "bottom")

# Histogram showing the number of ratings per year
ratings_year_histogram_food <- food_df %>% mutate(release = round_date(as_datetime(Timestamp), unit = "year")) %>% 
  group_by(release) %>%
  summarize(count = n()) %>%
  ggplot(aes(release, count)) +
  geom_col(fill = "gray")+
  theme_minimal()+
  scale_y_log10(labels = scales::comma) +
  ggtitle("Timestamp, time unit : year") +
  labs(title = "Histogram - ratings per year", x = "Year",
       y = "Qty", tag = "Fig. 9 - Histogram showing the quantity of ratings per year (fine foods).")+
  theme(plot.tag.position = "bottom")
################################################################################
# TRAINING AND TEST DATA FRAMES
################################################################################
# Creating training and test data frames
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = food_df$Rating, times = 1, p = 0.1, list = FALSE)
training_dataframe <- food_df[-test_index,]
temp <- food_df[test_index,]
# Make sure UserId and ProductId in training_dataframe are also in test_dataframe
test_dataframe <- temp %>%
  semi_join(training_dataframe, by = "ProductId") %>%
  semi_join(training_dataframe, by = "UserId")
# Add rows removed from test_dataframe set back into training_dataframe
# Adding back rows into train set
removed <- anti_join(temp, test_dataframe)
training_dataframe <- rbind(training_dataframe, removed)
rm(test_index, temp, removed)
################################################################################
###LINEAR MODELS
################################################################################
# lm() function using Timestamp as a predictor.
model <- lm(formula=Rating ~ Timestamp, data = training_dataframe)
prediction_lm_test_ts <- predict(model, newdata = test_dataframe)
rmse_lm_test_ts <- RMSE(prediction_lm_test_ts, test_dataframe$Rating)
model <- lm(formula=Rating ~ Timestamp, data = food_df)
prediction_validation_ts <- predict(model, newdata = validation)
rmse_lm_validation_ts <- RMSE(prediction_validation_ts, validation$Rating)
# lm() function using Timestamp and UserId as predictors.
model <- lm(formula=Rating ~ Timestamp+UserId, data = training_dataframe)
prediction_lm_test_ts_ui <- predict(model, newdata = test_dataframe)
rmse_lm_test_ts_ui <- RMSE(prediction_lm_test_ts_ui, test_dataframe$Rating)
model <- lm(formula=Rating ~ Timestamp+UserId, data = food_df)
prediction_lm_validation_ts_ui <- predict(model, newdata = validation)
rmse_lm_validation_ts_ui <- RMSE(prediction_lm_validation_ts_ui, validation$Rating)
# lm() function using Timestamp, UserId and ProductId as predictors.
model <- lm(formula=Rating ~ Timestamp+UserId+ProductId, data = training_dataframe)
prediction_lm_test_ts_ui_mi <- predict(model, newdata = test_dataframe)
rmse_lm_test_ts_ui_mi <- RMSE(prediction_lm_test_ts_ui_mi, test_dataframe$Rating)
model <- lm(formula=Rating ~ Timestamp+UserId+ProductId, data = food_df)
prediction_lm_validation_ts_ui_mi <- predict(model, newdata = validation)
rmse_lm_validation_ts_ui_mi <- RMSE(prediction_lm_validation_ts_ui_mi, validation$Rating)

# Compiling all results and and showing them in a table
results_food_lm <- tibble(predictors = "timestamp", RMSE = rmse_lm_test_ts, type = "test")
results_food_lm <- bind_rows(results_food_lm, tibble(predictors = "timestamp", RMSE = rmse_lm_validation_ts, type = "validation"))
results_food_lm <- bind_rows(results_food_lm, tibble(predictors = "timestamp and userId", RMSE = rmse_lm_test_ts_ui, type = "test"))
results_food_lm <- bind_rows(results_food_lm, tibble(predictors = "timestamp and userId", RMSE = rmse_lm_validation_ts_ui, type = "validation"))
results_food_lm <- bind_rows(results_food_lm, tibble(predictors = "timestamp, userId and movieId", RMSE = rmse_lm_test_ts_ui_mi, type = "test"))
results_food_lm <- bind_rows(results_food_lm, tibble(predictors = "timestamp, userId and movieId", RMSE = rmse_lm_validation_ts_ui_mi, type = "validation"))
results_food_lm %>% knitr::kable()
rm(model)
################################################################################
###RECOSYSTEM
################################################################################
set.seed(1, sample.kind="Rounding")
# Test and validation data frames to use with "recosystem" library
train_recosys <- with(training_dataframe, data_memory(user_index = UserId, item_index = ProductId, rating = Rating))
test_recosys <- with(test_dataframe, data_memory(user_index = UserId, item_index = ProductId, rating = Rating))
food_df_recosys <- with(food_df, data_memory(user_index = UserId, item_index = ProductId, rating = Rating))
validation_recosys <- with(validation, data_memory(user_index = UserId, item_index = ProductId, rating = Rating))

# Creating an object of class RecoSys called "r"
r <- Reco()
# Setting tuning parameters
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 1))
# Training stage
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 1))
# Predicting results
prediction_recosys <- r$predict(test_recosys, out_memory())
# Calculating RMSE
rmse_1_iter_tests <- RMSE(prediction_recosys, test_dataframe$Rating)
# Printing RMSE
rmse_1_iter_tests

# Setting tuning parameters
tuning_parameters <- r$tune(food_df_recosys, opts = list(dim = c(20, 30),
                                                         nthread = 4, niter = 1))
# Training stage
r$train(food_df_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 1))
# Predicting results
prediction_recosys <- r$predict(validation_recosys, out_memory())
# Calculating RMSE
rmse_1_iter_final <- RMSE(prediction_recosys, validation$Rating)
# Printing RMSE
rmse_1_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 2))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 2))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_2_iter_tests <- RMSE(prediction_recosys, test_dataframe$Rating)
rmse_2_iter_tests
tuning_parameters <- r$tune(food_df_recosys, opts = list(dim = c(20, 30),
                                                         nthread = 4, niter = 2))
r$train(food_df_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 2))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_2_iter_final <- RMSE(prediction_recosys, validation$Rating)
rmse_2_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 3))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 3))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_3_iter_tests <- RMSE(prediction_recosys, test_dataframe$Rating)
rmse_3_iter_tests
tuning_parameters <- r$tune(food_df_recosys, opts = list(dim = c(20, 30),
                                                         nthread = 4, niter = 3))
r$train(food_df_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 3))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_3_iter_final <- RMSE(prediction_recosys, validation$Rating)
rmse_3_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 4))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 4))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_4_iter_tests <- RMSE(prediction_recosys, test_dataframe$Rating)
rmse_4_iter_tests
tuning_parameters <- r$tune(food_df_recosys, opts = list(dim = c(20, 30),
                                                         nthread = 4, niter = 4))
r$train(food_df_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 4))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_4_iter_final <- RMSE(prediction_recosys, validation$Rating)
rmse_4_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 5))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 5))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_5_iter_tests <- RMSE(prediction_recosys, test_dataframe$Rating)
rmse_5_iter_tests
tuning_parameters <- r$tune(food_df_recosys, opts = list(dim = c(20, 30),
                                                         nthread = 4, niter = 5))
r$train(food_df_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 5))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_5_iter_final <- RMSE(prediction_recosys, validation$Rating)
rmse_5_iter_final


# Compiling all results and and showing them in a table
results_recosys_food <- tibble(iterations = 1, RMSE = rmse_1_iter_tests, type = "test")
results_recosys_food <- bind_rows(results_recosys_food, tibble(iterations = 1, RMSE = rmse_1_iter_final, type = "validation"))
results_recosys_food <- bind_rows(results_recosys_food, tibble(iterations = 2, RMSE = rmse_2_iter_tests, type = "test"))
results_recosys_food <- bind_rows(results_recosys_food, tibble(iterations = 2, RMSE = rmse_2_iter_final, type = "validation"))
results_recosys_food <- bind_rows(results_recosys_food, tibble(iterations = 3, RMSE = rmse_3_iter_tests, type = "test"))
results_recosys_food <- bind_rows(results_recosys_food, tibble(iterations = 3, RMSE = rmse_3_iter_final, type = "validation"))
results_recosys_food <- bind_rows(results_recosys_food, tibble(iterations = 4, RMSE = rmse_4_iter_tests, type = "test"))
results_recosys_food <- bind_rows(results_recosys_food, tibble(iterations = 4, RMSE = rmse_4_iter_final, type = "validation"))
results_recosys_food <- bind_rows(results_recosys_food, tibble(iterations = 5, RMSE = rmse_5_iter_tests, type = "test"))
results_recosys_food <- bind_rows(results_recosys_food, tibble(iterations = 5, RMSE = rmse_5_iter_final, type = "validation"))

results_recosys_food %>% knitr::kable()

results_recosys_food_lineplot <- results_recosys_food %>% 
  group_by(type) %>%
  ggplot( aes(x=iterations, y=RMSE, group=type, color=type)) +
  theme_minimal() + geom_line() +
  geom_hline(aes(yintercept = 0.865), color = "red", linetype="dashed", size=1)+
  annotate("text", x = 3, y = 0.9, label = "RMSE=8649 (goal)",
           color = "red", size = 4, fontface =2) +
  ggtitle("Timestamp, time unit : year") +
  labs(title = "recosystem - RMSE vs. iterations", x = "iterations", 
       y = "RMSE", tag = "Fig. 14 - RMSE of with different iterations using recosystem (fine foods).")+
  theme(plot.tag.position = "bottom")

################################################################################
################################################################################
# ELECTRONIC PRODUCTS
################################################################################
################################################################################

# DATASET DESCRIPTION:
# Contains a large volume of reviews about electronic products.
# Downloaded from https://www.kaggle.com/pritech/ratings-electronics

# Downloading the dataset from Dropbox
temp <- read.csv("https://www.dropbox.com/s/0eyjvj3tdr37sem/ratings_Electronics.csv?dl=1")
# Removing all NAs
temp <- na.omit(temp)
# Renaming the columns with intelligible names
temp <- temp %>% rename(Rating = X5.0)
temp <- temp %>% rename(ProductId = X0132793040)
temp <- temp %>% rename(UserId = AKM1MP6P0OYPR)
temp <- temp %>% rename(Timestamp = X1365811200)
# Removing empty scores/ratings
electronics <- temp %>% filter(Rating != "") %>% droplevels()
# Casting UserId and ProductId as characters (in order to avoid some errors)
electronics$UserId <- as.character(electronics$UserId)
electronics$ProductId <- as.character(electronics$ProductId)
# Converting UserId and ProductId to numbers (they are originally hash codes)
electronics$UserId <- as.numeric(digest::digest2int(electronics$UserId))
electronics$ProductId <- as.numeric(digest::digest2int(electronics$ProductId))
# Ordering the data frame by the UserId
electronics<-as.data.frame(electronics[order(electronics$UserId),])
# As both UserId and ProductId are very large numbers, it was decided to 
# change it to their respective ranks, e.g.: UserId "1082878268" could be converted 
# to "14" or ProductId "568411023" could be converted to "1023" etc.
rownames(electronics) <- c(1:nrow(electronics))
electronics$UserId<-round(rank(electronics$UserId, ties.method = "average"))
electronics$ProductId<-round(rank(electronics$ProductId , ties.method = "average"))

################################################################################
# DATASET SAMPLING; TRAINING AND VALIDATION DATA FRAMES
################################################################################
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = electronics$Rating, times = 1, p = 0.1, list = FALSE)

electronics_df <- electronics[-test_index,]
temp <- electronics[test_index,]

# Make sure UserId and ProductId in validation set are also in electronics_df set
validation <- temp %>% 
  semi_join(electronics_df, by = "ProductId") %>%
  semi_join(electronics_df, by = "UserId")

# Add rows removed from validation set back into electronics_df set
removed <- anti_join(temp, validation)
electronics_df <- rbind(electronics_df, removed)

electronics_df$ProductId<-round(rank(electronics_df$ProductId , ties.method = "average"))

rm(test_index, temp, electronics, removed)

################################################################################
# electronics_df GENERAL OVERVIEW
################################################################################
# As function sumary() returns no useful information about 'electronics_df' data frame, 
# it was manually processed.
# electronics_df has 475970 products.
length(unique(electronics_df$ProductId))
# electronics_df has 4199593 unique UserIds, which means that 4199593 users rated the movies.
length(unique(electronics_df$UserId))
# The highest rating is 5.
max(electronics_df$Rating)
# The rating "5" was given 4078876 times.
sum(electronics_df$Rating==5)
# The lowest rating is 1
min(electronics_df$Rating)
# The rating "1" was given 860965 times.
sum(electronics_df$Rating==1)
# The average rating of electronics_df 4.005487.
mean(electronics_df$Rating)
# The standard deviation rating of electronics_df is 1.38653.
sd(electronics_df$Rating)


# Ratings histogram
ratings_qty_electronics <- electronics_df %>% group_by(Rating) %>%
  summarize(count = n())
ratings_histogram_electronics <- ratings_qty_electronics %>% mutate(Rating = factor(Rating)) %>%
  ggplot(aes(Rating, count)) +
  geom_col(fill = "gray")+ 
  theme_minimal() + 
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Ratings", y = "Qty",
       tag = "Fig. 10 - Histogram showing the quantity of each given rating (electronic products).") +
  theme(plot.tag.position = "bottom")+ 
  ggtitle("Histogram of ratings")

# Creating a summary table grouped by UserId
ratings_per_user_electronics <- electronics_df %>% group_by(UserId) %>%
  summarize(ratings_per_user_qty = n(),
            ratings_given_mean = mean(Rating),
            mu_user = mean(Rating),
            sd_user = sd(Rating),)

# Average of 1.752009 ratings given per user
mean(ratings_per_user_electronics$ratings_per_user_qty)

# ERROR PLOTTING THE VECTOR
# Histogram showing the number of users per given ratings
# ratings_user_hist_electronics <- electronics_df %>% group_by(UserId) %>%
#  summarize(count = n()) %>%
# ggplot(aes(count)) +
#  geom_histogram(color = "black", fill = "gray", bins = 5) +
#  geom_vline(aes(xintercept = mean(ratings_per_user_food$ratings_per_user_qty)), color = "red", linetype="dashed", size=1)+
#  annotate("text", x = 2, y = 250000, label = "avg=1.752",
#           color = "red", size = 4, fontface =2) +
#  theme_minimal()+
#  xlim(0,15) +
#  scale_y_log10(labels = scales::comma) +
#  labs(title = "Histogram - ratings per user", x = "Ratings given", 
#       y = "Users", tag = "Fig. 11 - Histogram showing the quantity of users and rating given (electronic products).")+
#  theme(plot.tag.position = "bottom")

# Histogram showing the number of ratings per year
ratings_year_histogram_electronics <- electronics_df %>% mutate(release = round_date(as_datetime(Timestamp), unit = "year")) %>% 
  group_by(release) %>%
  summarize(count = n()) %>%
  ggplot(aes(release, count)) +
  geom_col(fill = "gray")+
  theme_minimal()+
  scale_y_log10(labels = scales::comma) +
  ggtitle("Timestamp, time unit : year") +
  labs(title = "Histogram - ratings per year", x = "Year", 
       y = "Qty", tag = "Fig. 11 - Histogram showing the quantity of ratings per year (electronic products).")+
  theme(plot.tag.position = "bottom")
################################################################################
###LINEAR MODELS
################################################################################
# lm() function using Timestamp as a predictor.
model <- lm(formula=Rating ~ Timestamp, data = training_dataframe)
prediction_lm_test_ts <- predict(model, newdata = test_dataframe)
rmse_lm_test_ts <- RMSE(prediction_lm_test_ts, test_dataframe$Rating)
model <- lm(formula=Rating ~ Timestamp, data = electronics_df)
prediction_validation_ts <- predict(model, newdata = validation)
rmse_lm_validation_ts <- RMSE(prediction_validation_ts, validation$Rating)
# lm() function using Timestamp and UserId as predictors.
model <- lm(formula=Rating ~ Timestamp+UserId, data = training_dataframe)
prediction_lm_test_ts_ui <- predict(model, newdata = test_dataframe)
rmse_lm_test_ts_ui <- RMSE(prediction_lm_test_ts_ui, test_dataframe$Rating)
model <- lm(formula=Rating ~ Timestamp+UserId, data = electronics_df)
prediction_lm_validation_ts_ui <- predict(model, newdata = validation)
rmse_lm_validation_ts_ui <- RMSE(prediction_lm_validation_ts_ui, validation$Rating)
# lm() function using Timestamp, UserId and ProductId as predictors.
model <- lm(formula=Rating ~ Timestamp+UserId+ProductId, data = training_dataframe)
prediction_lm_test_ts_ui_mi <- predict(model, newdata = test_dataframe)
rmse_lm_test_ts_ui_mi <- RMSE(prediction_lm_test_ts_ui_mi, test_dataframe$Rating)
model <- lm(formula=Rating ~ Timestamp+UserId+ProductId, data = electronics_df)
prediction_lm_validation_ts_ui_mi <- predict(model, newdata = validation)
rmse_lm_validation_ts_ui_mi <- RMSE(prediction_lm_validation_ts_ui_mi, validation$Rating)

# Compiling all results and and showing them in a table
results_electronics_lm <- tibble(predictors = "timestamp", RMSE = rmse_lm_test_ts, type = "test")
results_electronics_lm <- bind_rows(results_electronics_lm, tibble(predictors = "timestamp", RMSE = rmse_lm_validation_ts, type = "validation"))
results_electronics_lm <- bind_rows(results_electronics_lm, tibble(predictors = "timestamp and userId", RMSE = rmse_lm_test_ts_ui, type = "test"))
results_electronics_lm <- bind_rows(results_electronics_lm, tibble(predictors = "timestamp and userId", RMSE = rmse_lm_validation_ts_ui, type = "validation"))
results_electronics_lm <- bind_rows(results_electronics_lm, tibble(predictors = "timestamp, userId and movieId", RMSE = rmse_lm_test_ts_ui_mi, type = "test"))
results_electronics_lm <- bind_rows(results_electronics_lm, tibble(predictors = "timestamp, userId and movieId", RMSE = rmse_lm_validation_ts_ui_mi, type = "validation"))
results_electronics_lm %>% knitr::kable()
rm(model)
################################################################################
# TRAINING AND TEST DATA FRAMES
################################################################################
# Creating training and test data frames
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = electronics_df$Rating, times = 1, p = 0.1, list = FALSE)
training_dataframe <- electronics_df[-test_index,]
temp <- electronics_df[test_index,]
# Make sure UserId and ProductId in training_dataframe are also in test_dataframe
test_dataframe <- temp %>%
  semi_join(training_dataframe, by = "ProductId") %>%
  semi_join(training_dataframe, by = "UserId")
# Add rows removed from test_dataframe set back into training_dataframe
# Adding back rows into train set
removed <- anti_join(temp, test_dataframe)
training_dataframe <- rbind(training_dataframe, removed)
rm(test_index, temp, removed)
################################################################################
###RECOSYSTEM
################################################################################
set.seed(1, sample.kind="Rounding")
# Test and validation data frames to use with "recosystem" library
train_recosys <- with(training_dataframe, data_memory(user_index = UserId, item_index = ProductId, rating = Rating))
test_recosys <- with(test_dataframe, data_memory(user_index = UserId, item_index = ProductId, rating = Rating))
electronics_df_recosys <- with(electronics_df, data_memory(user_index = UserId, item_index = ProductId, rating = Rating))
validation_recosys <- with(validation, data_memory(user_index = UserId, item_index = ProductId, rating = Rating))

# Creating an object of class RecoSys called "r"
r <- Reco()
# Setting tuning parameters
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 1))
# Training stage
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 1))
# Predicting results
prediction_recosys <- r$predict(test_recosys, out_memory())
# Calculating RMSE
rmse_1_iter_tests <- RMSE(prediction_recosys, test_dataframe$Rating)
# Printing RMSE
rmse_1_iter_tests

# Setting tuning parameters
tuning_parameters <- r$tune(electronics_df_recosys, opts = list(dim = c(20, 30),
                                                                nthread = 4, niter = 1))
# Training stage
r$train(electronics_df_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 1))
# Predicting results
prediction_recosys <- r$predict(validation_recosys, out_memory())
# Calculating RMSE
rmse_1_iter_final <- RMSE(prediction_recosys, validation$Rating)
# Printing RMSE
rmse_1_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 2))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 2))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_2_iter_tests <- RMSE(prediction_recosys, test_dataframe$Rating)
rmse_2_iter_tests
tuning_parameters <- r$tune(electronics_df_recosys, opts = list(dim = c(20, 30),
                                                                nthread = 4, niter = 2))
r$train(electronics_df_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 2))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_2_iter_final <- RMSE(prediction_recosys, validation$Rating)
rmse_2_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 3))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 3))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_3_iter_tests <- RMSE(prediction_recosys, test_dataframe$Rating)
rmse_3_iter_tests
tuning_parameters <- r$tune(electronics_df_recosys, opts = list(dim = c(20, 30),
                                                                nthread = 4, niter = 3))
r$train(electronics_df_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 3))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_3_iter_final <- RMSE(prediction_recosys, validation$Rating)
rmse_3_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 4))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 4))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_4_iter_tests <- RMSE(prediction_recosys, test_dataframe$Rating)
rmse_4_iter_tests
tuning_parameters <- r$tune(electronics_df_recosys, opts = list(dim = c(20, 30),
                                                                nthread = 4, niter = 4))
r$train(electronics_df_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 4))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_4_iter_final <- RMSE(prediction_recosys, validation$Rating)
rmse_4_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 5))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 5))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_5_iter_tests <- RMSE(prediction_recosys, test_dataframe$Rating)
rmse_5_iter_tests
tuning_parameters <- r$tune(electronics_df_recosys, opts = list(dim = c(20, 30),
                                                                nthread = 4, niter = 5))
r$train(electronics_df_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 5))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_5_iter_final <- RMSE(prediction_recosys, validation$Rating)
rmse_5_iter_final

# Compiling all results and and showing them in a table
results_recosys_electronics <- tibble(iterations = 1, RMSE = rmse_1_iter_tests, type = "test")
results_recosys_electronics <- bind_rows(results_recosys_electronics, tibble(iterations = 1, RMSE = rmse_1_iter_final, type = "validation"))
results_recosys_electronics <- bind_rows(results_recosys_electronics, tibble(iterations = 2, RMSE = rmse_2_iter_tests, type = "test"))
results_recosys_electronics <- bind_rows(results_recosys_electronics, tibble(iterations = 2, RMSE = rmse_2_iter_final, type = "validation"))
results_recosys_electronics <- bind_rows(results_recosys_electronics, tibble(iterations = 3, RMSE = rmse_3_iter_tests, type = "test"))
results_recosys_electronics <- bind_rows(results_recosys_electronics, tibble(iterations = 3, RMSE = rmse_3_iter_final, type = "validation"))
results_recosys_electronics <- bind_rows(results_recosys_electronics, tibble(iterations = 4, RMSE = rmse_4_iter_tests, type = "test"))
results_recosys_electronics <- bind_rows(results_recosys_electronics, tibble(iterations = 4, RMSE = rmse_4_iter_final, type = "validation"))
results_recosys_electronics <- bind_rows(results_recosys_electronics, tibble(iterations = 5, RMSE = rmse_5_iter_tests, type = "test"))
results_recosys_electronics <- bind_rows(results_recosys_electronics, tibble(iterations = 5, RMSE = rmse_5_iter_final, type = "validation"))

results_recosys_electronics %>% knitr::kable()

results_recosys_electronics_lineplot <- results_recosys_electronics %>% 
  group_by(type) %>%
  ggplot( aes(x=iterations, y=RMSE, group=type, color=type)) +
  theme_minimal() + geom_line() +
  geom_hline(aes(yintercept = 0.865), color = "red", linetype="dashed", size=1)+
  annotate("text", x = 3, y = 0.9, label = "RMSE=8649 (goal)",
           color = "red", size = 4, fontface =2) +
  ggtitle("Timestamp, time unit : year") +
  labs(title = "recosystem - RMSE vs. iterations", x = "iterations", 
       y = "RMSE", tag = "Fig. 15 - RMSE of with different iterations using recosystem (electronic products).")+
  theme(plot.tag.position = "bottom")
