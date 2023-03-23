# 13.1 Introduction ####
# Mutating joins, which add new variables to one data frame from matching observations in another.

# Filtering joins, which filter observations from one data frame based on whether or not they match an observation in the other table.

# Set operations, which treat observations as if they were set elements.

## 13.1.1 Prerequisites ####
library(tidyverse)
library(nycflights13)

# 13.2 nycflights13 ####
airlines # carrier, name
airports # faa, name, lat, lon, alt, tz, dst, tzone
planes # tailnum, year, type, manufacturer, model engines, seats, speed, engine
weather # origin, year, month, day, hour, temp, dewp, humid, wind_dir, wind_speed, wind_gust, precip, pressure, visib, time_hour

# 13.3 Keys ####
planes %>%
  count(tailnum) %>%
  filter(n > 1)

weather %>%
  count(year, month, day, hour, origin) %>%
  filter(n > 1)

flights %>%
  count(year, month, day, flight) %>%
  filter(n > 1)

flights %>%
  count(year, month, day, tailnum) %>%
  filter(n > 1)

library(Lahman)
Batting %>%
  count(playerID) %>%
  filter(n > 1)

# 13.4 Mutating joins ####
flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)

flights2 %>%
  select(-origin, -dest) %>%
  left_join(airlines, by = "carrier")

flights2 %>%
  select(-origin, - dest) %>%
  mutate(name = airlines$name[match(carrier, airlines$carrier)])

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

# 13.4.2 Inner join ####
# The simplest type of join is the inner join. An inner join matches pairs of observations whenever their keys are equal.
x %>%
  inner_join(y, by = "key")
# The most important property of an inner join is that unmatched rows are not included in the result. This means that generally inner joins are usually not appropriate for use in analysis because it’s too easy to lose observations.

## 13.4.3 Outer joins ####
# An inner join keeps observations that appear in both tables. An outer join keeps observations that appear in at least one of the tables. There are three types of outer joins:
#   
  # A left join keeps all observations in x.
  # A right join keeps all observations in y.
  # A full join keeps all observations in x and y.
# These joins work by adding an additional “virtual” observation to each table. This observation has a key that always matches (if no other key matches), and a value filled with NA.

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

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)

y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)

left_join(x, y, by = "key")

## 13.4.5 Definining the key columns ####
my_df <- flights2 %>%
  left_join(weather)

my_df <- flights2 %>%
  left_join(planes, by = "tailnum")

my_df <- flights2 %>%
  left_join(airports, c("dest" = "faa"))

flights2 %>%
  left_join(airports, c("origin" = "faa"))

## 13.4.6 Exercises ####
airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
    borders("state") +
    geom_point() +
    coord_quickmap()

flights3 <- flights %>%
  group_by(dest) %>%
  summarise(avg_delay = mean(dep_delay)) %>%
  left_join(airports, c("dest" = "faa"))

# join the two datasets using the origin airport
flights4 <- flights %>%
  left_join(airports, by = c("origin" = "faa")) %>%
  rename(origin_lat = lat, origin_lon = lon)

# join the two datasets using the destination airport
flights4 <- flights4 %>%
  left_join(airports, by = c("dest" = "faa")) %>%
  rename(dest_lat = lat, dest_lon = lon)

View(flights4)

planes1 <- planes %>%
  select(tailnum, year) %>%
  mutate(age = 2023 - year)

planes1 <- planes1 %>%
  select(tailnum, age)

flights5 <- flights %>%
  group_by(tailnum) %>%
  summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE), avg_arr_delay = mean(arr_delay, na.rm = TRUE))

flights5 <- flight5 %>%
  full_join(planes1)

flight5 %>%
  ggplot(aes(x=age, y=avg_dep_delay)) +
  geom_point(na.rm = TRUE)

flight5 %>%
  ggplot(aes(x=age, y=avg_arr_delay)) +
  geom_point(na.rm = TRUE)

create_report(flights)
library(summarytools)
descr(flights)

# 13.5 Filtering joins ####
# Imagine you've found the top ten most popular destinations
top_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  head(10)
# To find each flight that went to one of those destinations.
flights %>%
  filter(dest %in% top_dest$dest)

# Use semi_join, only keeps the rows in x that have a match in y
flights %>%
  semi_join(top_dest)

# Use anti_join, keeps the rows that don't have a match
flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(tailnum, sort = TRUE)
