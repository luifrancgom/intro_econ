library(tidyverse)
library(gganimate)

# Jon Spring
# https://stackoverflow.com/questions/53162821/animated-sorted-bar-chart-with-bars-overtaking-each-other/53163549
# https://stackoverflow.com/questions/52623722/how-does-gganimate-order-an-ordered-bar-time-series/52652394#52652394
## Original idea

# Steven Burr
# https://github.com/stevejburr/Bar-Chart-Race/

# Tutorial
# https://michaeltoth.me/how-to-create-a-bar-chart-race-in-r-mapping-united-states-city-population-1790-2010.html

# YouTube
# How To Create An ANIMATED Bar Chart Race
# https://youtu.be/FOEoKbRUsT8?si=98QABLrOdWBpU9A3

# Minimal data set
df <- tibble(
  year = rep(2000:2002, each = 3),
  category = rep(c("A", "B", "C"), times = 3),
  value = c(
    10, 5, 3,   # 2000: A is max
    6, 12, 4,   # 2001: B is max
    7, 6, 14    # 2002: C is max
  )
)

# Problem using geom_col
## fct_reorder doesn't work
### Can not change the factor
### each time
df |>
  filter(year == 2000) |> 
  ggplot() +
  geom_col(aes(x = value, 
               y = fct_reorder(category, value)))

# Calculate
df_rank <- df |> 
  group_by(year) |> 
  mutate(rank = rank(value)) |> 
  ungroup()

# Step by step visualization
## Step 1
df_rank |> 
  filter(year == 2000) |>
  ggplot() +
  geom_tile(aes(x = rank,
                y = value))

## Step 2
df_rank |> 
  filter(year == 2000) |>
  ggplot() +
  geom_tile(aes(x = rank,
                y = value,
                height = value))

## Step 3
df_rank |> 
  filter(year == 2000) |>
  ggplot() +
  geom_tile(aes(x = rank,
                # Expand height in both
                # directions
                ## It will cover half upwards
                ## and half downwards
                y = value/2,
                height = value))

## Step 4
df_rank |> 
  filter(year == 2000) |>
  ggplot() +
  geom_tile(aes(x = rank,
                y = value/2,
                height = value)) +
  coord_flip()

# Step 5
df_rank |> 
  filter(year == 2000) |> 
  ggplot(aes(x = rank, y = value)) + 
  geom_tile(aes(y = value/2,
                height = value),
            width = 0.9) +
  # Allows drawing outside panel
  # When labels/annotations extend outside axes
  coord_flip(clip="off")



tibble(x = 2,
       y = 2) |>   
  ggplot() +
  geom_tile(aes(x = x, 
                y = y/2)) +
  geom_point(aes(x = x,
                 y = y),
             color = "red")
  