## Libraries
library(tidyverse)
library(caret)
library(data.table)
library(matrixStats)
library(lubridate)
data("edx_and_validation")
options(digits = 5)

## Cleaning data
edx <- edx %>% mutate(release = str_extract(edx$title, "\\(\\d{4}\\)"),
               title = str_replace(edx$title, "\\(\\d{4}\\)", ""),
               release = as.numeric(str_replace_all(edx$release, "[^\\d]", "")))

## Reducing data to a smaller more managable size
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

## In this first section I will simply follow the approach used in the
# course itself. That is up to and including the regularized user- +
# movie-effects model.

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

# Adding user- + movie-effects model RMSE to table:
rmse_results <- rbind(rmse_results, c("Added user-effects", u_and_m_effect_rmse))

# Regularization with penalized least squares
# Making a sequence for the penalty parameter lambda:
lambdas <- seq(0, 10, 0.25)
# Finding the lambda, that minimizes RMSE:
rmses <- sapply(lambdas, function(l){
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating-mu_hat)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by = "movieId") %>% 
    group_by(userId) %>% 
    summarize(b_u = sum(rating - b_i - mu_hat)/(n()+l))
  predicted_ratings <- test_set %>% 
    left_join(b_i, by = "movieId") %>% 
    left_join(b_u, by = "userId") %>% 
    mutate(pred = mu_hat + b_i + b_u) %>% 
    .$pred
  return(RMSE(test_set$rating, predicted_ratings))
})
qplot(lambdas, rmses)
best_lambda <- lambdas[which.min(rmses)]
# Adding the best regularized RMSE to the table
rmse_results <- rbind(rmse_results, c("Added regularization", min(rmses)))

## In this next section I will experiment with the use of matrix
# factorization guided by recommenderlab-package.

# A first step will be to create a matrix. The problem is, that even
# the smaller train- and test sets are simply too large to convert to
# a matrix format. Let's look at the distribution of ratings pr. movie
# and ratings pr. users to look for a good cutoff for a matrix of the
# most commonly rated movies as rated by the most prolific users.
edx %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 50, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

edx %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 50, color = "black") + 
  scale_x_log10() +
  ggtitle("Users")

# Due to the sheer size of the data, let us limit ourselves to the top
# 1% most commonly rated movies and the top 1% most active users
top_raters <- edx %>%
  dplyr::count(userId) %>%
  slice_max(n, n = nrow(.)/100)
top_movies <- edx %>% 
  dplyr::count(movieId) %>% 
  slice_max(n, n = nrow(.)/100)

top_edx <- edx %>% filter(movieId %in% top_movies$movieId, 
                          userId %in% top_raters$userId)
# Let us inspect what that leaves us with
top_edx %>% summarise(n_obs = nrow(.),
                      pct_obs = n_obs/nrow(edx)*100,
                      n_users = n_distinct(userId),
                      n_movies = n_distinct(movieId))
# We are left with roughly 25% of the original dataset. These ratings 
# are made by roughly 7000 users spread across roughly 1000 movies. 
# Let us transform it into a matrix.
top_matrix <- top_edx %>% 
  select(userId, movieId, rating) %>% 
  spread(movieId, rating) %>% 
  as.matrix()
rownames(top_matrix) <- top_matrix[,1]
top_matrix <- top_matrix[,-1]
top_matrix <- sweep(top_matrix, 2, colMeans(top_matrix, na.rm = T))
top_matrix <- sweep(top_matrix, 1, rowMeans(top_matrix, na.rm = T))
# Finding the distance between movies
d <- dist(t(top_matrix))

# Hierarchical clustering:
h <- hclust(d)
h_groups <- cutree(h, k = 10)
# Lets see what movies are in these groups:
movie_titles <- edx %>%
  select(movieId, title) %>% 
  distinct()
movie_titles$title <- str_remove(movie_titles$title, pattern =  ": Episode") %>% 
  str_trunc(20)

h_groups_w_titles <- movie_titles %>% 
  left_join(data.frame(groups = h_groups, 
                       movieId = as.numeric(names(h_groups)))) %>% 
  filter(!is.na(groups)) %>% 
  arrange(groups)
h_groups_w_titles

# k_means clustering:
matrix_0 <- top_matrix
matrix_0[is.na(matrix_0)] <- 0
k <- kmeans(t(matrix_0), centers = 10, nstart = 25)
k_groups <- k$cluster
# Let's see what movies are in these groups:
k_groups_w_titles <- movie_titles %>% 
  left_join(data.frame(groups = k_groups, 
                       movieId = as.numeric(names(k_groups)))) %>% 
  filter(!is.na(groups)) %>% 
  arrange(groups)
k_groups_w_titles
