##Library
library(tidyverse)
data("edx_and_validation")

## Quiz: MovieLens Dataset

# Q1. 
# How many rows and columns are there in the edx dataset?
dim(edx)
# Answers:
#Rows: 9000055
#Columns: 6

# Q2. 
# .1 How man zeros were given as ratings in the edx dataset?
sum(edx$rating == 0)
# Answer: 0
# .2 How many threes were given as ratings in the edx dataset?
sum(edx$rating == 3)
# Answer: 2121240

# Q3.
# How many different movies are in the edx dataset?
length(unique(edx$movieId))
# Answer: 10677

# Q4. How many different users are in the edx dataset?
length(unique(edx$userId))
# Answer: 69878

# Q5. How many ratings are in each of the following genres in the edx
# dataset?
# .1 Drama:
sum(str_detect(edx$genres, "Drama"))
# Answer: 3910127
# .2 Comedy:
sum(str_detect(edx$genres, "Comedy"))
# Answer: 3540930
# .3 Thriller:
sum(str_detect(edx$genres, "Thriller"))
# Answer: 2325899
# .4 Romance:
sum(str_detect(edx$genres, "Romance"))
# Answer: 1712100

# Q6. Which movie has the greatest number of ratings?
edx %>% group_by(movieId) %>% summarise(n = n()) %>% arrange(desc(n))
edx$title[edx$movieId == 296]
# Answer: c.: Pulp Fiction

# Q7. What are the five most given ratings in order form most to least?
edx %>% group_by(rating) %>% summarise(n = n()) %>% arrange(desc(n))
# Answer: a.: 4, 3, 5, 3.5, 2

# Q8 True or False: In general, half star ratings are less common than
# whole star ratings (e.g. there are fewer ratings of 3.5 than there
# are ratings of 3 or 4, etc.)
edx %>% group_by(rating) %>% summarise(n = n()) %>% arrange(desc(n))
# Answer: a.: True