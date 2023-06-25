# 1
library(tidyverse)

# 2
# Read in our data
retreats <- read_csv("Retreats_Workshops_Data.csv")

# 3
# Explore the data
head(retreats)

# 4
# Summarize basic information about our data
summary(retreats)

# 5
# Visualize our data using ggplot2
ggplot(retreats, aes(x = star_rating, y = price)) + 
  geom_point() + 
  labs(title = "Retreats and Workshops Prices vs. Star Rating")

# 6
# Create table to understand the average price points
retreat_price_summary <- retreats %>% 
  group_by(star_rating) %>% 
  summarize(average_price = mean(price)) %>% 
  ungroup()

# 7
# Plot the average price points
ggplot(retreat_price_summary, aes(x = star_rating, y = average_price)) + 
  geom_col() + 
  labs(title = "Average Retreat & Workshop Prices by Star Rating")

# 8
# Create a variable to hold the number of reviews
number_of_reviews <- retreats %>% 
  group_by(star_rating) %>% 
  summarize(number_of_reviews = n()) %>% 
  ungroup()

# 9
# Plot number of reviews 
ggplot(number_of_reviews, aes(x = star_rating, y = number_of_reviews)) + 
  geom_col() + 
  labs(title = "Number of Reviews per Star Rating")

# 10
# Create a table for the longest retreats 
longest_retreats <- retreats %>% 
  group_by(star_rating) %>% 
  summarize(longest_retreat = max(duration)) %>% 
  ungroup()

# 11
# Plot the longest retreats
ggplot(longest_retreats, aes(x = star_rating, y = longest_retreat)) + 
  geom_col() + 
  labs(title = "Longest Retreats by Star Rating")

# 12
# Create a table for the shortest retreats
shortest_retreats <- retreats %>% 
  group_by(star_rating) %>% 
  summarize(shortest_retreat = min(duration)) %>% 
  ungroup()

# 13
# Plot the shortest retreats
ggplot(shortest_retreats, aes(x = star_rating, y = shortest_retreat)) + 
  geom_col() + 
  labs(title = "Shortest Retreats by Star Rating")

# 14
# Create a table of the most expensive retreats 
most_expensive_retreats <- retreats %>% 
  group_by(star_rating) %>% 
  summarize(most_expensive_retreats = max(price)) %>% 
  ungroup()

# 15
# Plot the most expensive retreats
ggplot(most_expensive_retreats, aes(x = star_rating, y = most_expensive_retreats)) + 
  geom_col() + 
  labs(title = "Most Expensive Retreats by Star Rating")

# 16
# Create a table of the least expensive retreats
least_expensive_retreats <- retreats %>% 
  group_by(star_rating) %>% 
  summarize(least_expensive_retreats = min(price)) %>% 
  ungroup()

# 17
# Plot the least expensive retreats
ggplot(least_expensive_retreats, aes(x = star_rating, y = least_expensive_retreats)) + 
  geom_col() + 
  labs(title = "Least Expensive Retreats by Star Rating")

# 18
# Create a table to find the average number of days for each retreat in our dataset
average_days <- retreats %>% 
  group_by(star_rating) %>% 
  summarize(average_days = mean(duration)) %>% 
  ungroup()

# 19
# Plot the average number of days for each retreat in our dataset
ggplot(average_days, aes(x = star_rating, y = average_days)) + 
  geom_col() + 
  labs(title = "Average Number of Days by Star Rating")

# 20
# Create a table to find the most common type of retreat
popular_retreat_types <- retreats %>% 
  group_by(type) %>% 
  summarize(count = n()) %>% 
  arrange(-count) %>% 
  ungroup()

# 21
# Plot the most common type of retreat
ggplot(popular_retreat_types, aes(x = type, y = count)) + 
  geom_col() + 
  labs(title = "Most Common Type of Retreat")

# 22
# Create a table to find the average retreat ratings
average_ratings <- retreats %>% 
  group_by(star_rating) %>% 
  summarize(average_rating = mean(rating)) %>% 
  ungroup()

# 23
# Plot the average retreat ratings
ggplot(average_ratings, aes(x = star_rating, y = average_rating)) + 
  geom_col() + 
  labs(title = "Average Rating by Star Rating")

# 24
# Create a table to find the most expensive retreat type
expensive_retreat_types <- retreats %>% 
  group_by(type) %>% 
  summarize(most_expensive_retreat = max(price)) %>% 
  ungroup()

# 25
# Plot the most expensive retreat type 
ggplot(expensive_retreat_types, aes(x = type, y = most_expensive_retreat)) + 
  geom_col() + 
  labs(title = "Most Expensive Retreat Type")

# 26
# Create a table to find what types of retreats have the highest ratings
types_highest_ratings <- retreats %>% 
  group_by(type) %>% 
  summarize(highest_rating = max(rating)) %>% 
  ungroup()

# 27
# Plot what types of retreats have the highest ratings 
ggplot(types_highest_ratings, aes(x = type, y = highest_rating)) + 
  geom_col() + 
  labs(title = "Highest Rated Retreat Type")

# 28
# Create a table to find the longest retreats by type
longest_retreats_by_type <- retreats %>% 
  group_by(type) %>% 
  summarize(longest_retreat = max(duration)) %>% 
  ungroup()

# 29
# Plot the longest retreats by type
ggplot(longest_retreats_by_type, aes(x = type, y = longest_retreat)) + 
  geom_col() + 
  labs(title = "Longest Retreat by Type")

# 30
# Create a table to find the shortest retreats by type
shortest_retreats_by_type <- retreats %>% 
  group_by(type) %>% 
  summarize(shortest_retreat = min(duration)) %>% 
  ungroup()

# 31
# Plot the shortest retreats by type
ggplot(shortest_retreats_by_type, aes(x = type, y = shortest_retreat)) + 
  geom_col() + 
  labs(title = "Shortest Retreats by Type")

# 32
# Create a table to find the price distributions by type
price_distributions_by_type <- retreats %>% 
  group_by(type) %>% 
  summarize(price_distribution = mean(price)) %>% 
  ungroup()

# 33
# Plot the price distributions by type 
ggplot(price_distributions_by_type, aes(x = type, y = price_distribution)) + 
  geom_col() + 
  labs(title = "Price Distributions by Type")

# 34
# Create a table to find the ratings distributions by type 
ratings_distributions_by_type <- retreats %>% 
  group_by(type) %>% 
  summarize(ratings_distribution = mean(rating)) %>% 
  ungroup()

# 35
# Plot the ratings distributions by type 
ggplot(ratings_distributions_by_type, aes(x = type, y = ratings_distribution)) + 
  geom_col() + 
  labs(title = "Ratings Distributions by Type")

# 36
# Create a table to find the average ratings by type
average_ratings_by_type <- retreats %>% 
  group_by(type) %>% 
  summarize(average_rating = mean(rating)) %>% 
  ungroup()

# 37
# Plot the average ratings by type 
ggplot(average_ratings_by_type, aes(x = type, y = average_rating)) + 
  geom_col() + 
  labs(title = "Average Ratings by Type")

# 38
# Create a table to find the costliest retreats by type
costliest_retreats_by_type <- retreats %>% 
  group_by(type) %>% 
  summarize(costliest_retreat = max(price)) %>% 
  ungroup()

# 39
# Plot the costliest retreats by type
ggplot(costliest_retreats_by_type, aes(x = type, y = costliest_retreat)) + 
  geom_col() + 
  labs(title = "Costliest Retreats by Type")

# 40
# Create a table to find the cheapest retreats by type
cheapest_retreats_by_type <- retreats %>% 
  group_by(type) %>% 
  summarize(cheapest_retreat = min(price)) %>% 
  ungroup()

# 41
# Plot the cheapest retreats by type
ggplot(cheapest_retreats_by_type, aes(x = type, y = cheapest_retreat)) + 
  geom_col() + 
  labs(title = "Cheapest Retreats by Type")

# 42
# Create a table to find the average star ratings by type
star_rating_by_type <- retreats %>% 
  group_by(type) %>% 
  summarize(average_star_rating = mean(star_rating)) %>% 
  ungroup()

# 43
# Plot the average star ratings by type
ggplot(star_rating_by_type, aes(x = type, y = average_star_rating)) + 
  geom_col() + 
  labs(title = "Average Star Ratings by Type")

# 44
# Create a table to find the average cost of a retreat by location
cost_by_location <- retreats %>% 
  group_by(location) %>% 
  summarize(average_cost = mean(price)) %>% 
  ungroup()

# 45
# Plot the average cost of a retreat by location
ggplot(cost_by_location, aes(x = location, y = average_cost)) + 
  geom_col() + 
  labs(title = "Average Cost by Location")

# 46
# Create a table to find the most expensive retreat by location
most_expensive_retreat_by_location <- retreats %>% 
  group_by(location) %>% 
  summarize(most_expensive_retreat = max(price)) %>% 
  ungroup()

# 47
# Plot the most expensive retreat by location
ggplot(most_expensive_retreat_by_location, aes(x = location, y = most_expensive_retreat)) + 
  geom_col() + 
  labs(title = "Most Expensive Retreat by Location")

# 48
# Create a table to find the longest retreat by location
longest_retreat_by_location <- retreats %>% 
  group_by(location) %>% 
  summarize(longest_retreat = max(duration)) %>% 
  ungroup()

# 49
# Plot the longest retreat by location
ggplot(longest_retreat_by_location, aes(x = location, y = longest_retreat)) + 
  geom_col() + 
  labs(title = "Longest Retreat by Location")

# 50
# Create a table to find the shortest retreat by location
shortest_retreat_by_location <- retreats %>% 
  group_by(location) %>% 
  summarize(shortest_retreat = min(duration)) %>% 
  ungroup()

# 51
# Plot the shortest retreat by location
ggplot(shortest_retreat_by_location, aes(x = location, y = shortest_retreat)) + 
  geom_col() + 
  labs(title = "Shortest Retreat by Location")

# 52
# Create a table to find the average ratings by location
ratings_by_location <- retreats %>% 
  group_by(location) %>% 
  summarize(average_rating = mean(rating)) %>% 
  ungroup()

# 53
# Plot the average ratings by location 
ggplot(ratings_by_location, aes(x = location, y = average_rating)) + 
  geom_col() + 
  labs(title = "Average Ratings by Location")

# 54
# Create a table to find the type of retreats with the highest ratings by location
highest_rated_retreats_by_location <- retreats %>% 
  group_by(location, type) %>% 
  summarize(highest_rating = max(rating)) %>% 
  ungroup()

# 55
# Plot the type of retreats with the highest ratings by location
ggplot(highest_rated_retreats_by_location, aes(x = type, y = highest_rating)) + 
  geom_col() + 
  facet_wrap(~location, scales = "free", ncol = 2) + 
  labs(title = "Types