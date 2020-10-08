## Libraries
library(tidyverse)
library(caret)
library(data.table)
library(matrixStats)
data("edx_and_validation")
options(digits = 5)

## Reducing data to a smller more managable size
dat <- slice_sample(edx, n = nrow(edx)/10)

# Splitting the reduced data into a training and a test set
test_index <- createDataPartition(dat$rating, times = 1, p = 0.1, list = F)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")
rm(test_index, dat)

# Creating RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Predicting rating by average rating as a baseline
mu_hat <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu_hat)

# Creating a results table for comparison:
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)

# Calculating movie-effects
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu_hat))

# RMSE of model with movie-effects added
predicted_ratings <- mu_hat + test_set %>% 
  left_join(movie_avgs, by = "movieId") %>% 
  .$b_i
m_effect_rmse <- RMSE(test_set$rating, predicted_ratings)

# Adding movie-effects RMSE to table
rmse_results <- rbind(rmse_results, c("Added movie-effects", m_effect_rmse))

# Calculating user-effects
user_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>% 
  group_by(userId) %>% 
  summarise(b_u = mean(rating - mu_hat - b_i))

# RMSE of model with both movie- and user-effects added
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by = "movieId") %>% 
  left_join(user_avgs, by = "userId") %>% 
  mutate(pred = mu_hat + b_i + b_u) %>% 
  .$pred
u_and_m_effect_rmse <- RMSE(test_set$rating, predicted_ratings)

# Adding user- + movie-effects RMSE to table:
rmse_results <- rbind(rmse_results, c("Added user-effects", u_and_m_effect_rmse))
