# 13.1 Introduction ####
## 13.1.1 Prerequisites ####
library(tidyverse)
library(nycflights13)
# 13.2 nycflights ####
airlines
airports
planes
weather
# 13.3 Keys ####
planes |> 
  count(tailnum) |> 
  filter(n > 1)
weather |> 
  count(year, month, day, hour, origin) |> 
  filter(n > 1)

flights |> 
  count(year, month, day, flight) |> 
  filter(n > 1)
flights |> 
  count(year, month, day, tailnum) |> 
  filter(n > 1)

## 13.3.1 Exercises ####
Lahman::Batting

# Mutating joins ####
flights2 <- flights |> 
  select(year:day, hour, origin, dest, tailnum, carrier)

flights2 |> 
  select(-origin, -dest) |> 
  left_join(airlines, by = "carrier")

## 13.4.1 Understanding joins ####
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)
x
y
## 13.4.2 Inner join ####
x |> 
  inner_join(y, by = "key")
## 13.4.3 Outer joins ####

## 13.4.4 Duplicate keys ####
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)
left_join(x, y, by = "key")
## 13.4.5 Defining the key columns ####
flights2 |> 
  left_join(weather)

flights2 |> 
  left_join(planes, by = "tailnum")

flights2 |> 
  left_join(airports, c("dest" = "faa"))

flights2 |> 
  left_join(airports, c("origin" = "faa"))

# 13.5 Filtering joins ####
top_dest <- flights |> 
  count(dest, sort = TRUE) |> 
  head(10)

flights |> 
  filter(dest %in% top_dest$dest)

flights |> 
  semi_join(top_dest)

flights |> 
  anti_join(planes, by = "tailnum") |> 
  count(tailnum, sort = TRUE)

## 13.5.1 Exercises ####

# 13.6 Join problems ####
airports |> count(alt, lon) |>  filter(n > 1)
# 13.7 Set operations ####
df1 <- tribble(
  ~x, ~y,
  1, 1,
  2, 1
)
df2 <- tribble(
  ~x, ~y,
  1, 1,
  1, 2
)
intersect(df1, df2)
union(df1, df2)
setdiff(df1, df2)
