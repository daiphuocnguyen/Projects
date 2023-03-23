library(nycflights13)
library(tidyverse)

flights

# 5.1.3 dplyr basics ####
# Pick observations by their values (filter()).
# Reorder the rows (arrange()).
# Pick variables by their names (select()).
# Create new variables with functions of existing variables (mutate())
# Collapse many values down to a single summary (summarise()).

# 5.2 Filter rows with filter() ####
filter(flights, month == 1, day == 1)
jan1 <- filter(flights, month == 1, day == 1)
(dec25 <- filter(flights, month == 12, day == 25))
## 5.2.1 Comparisons ####
## 5.2.2 Logical operators ####
filter(flights, month == 11 | month == 12)
nov_dec <- filter(flights, month %in% c(11, 12))
filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)
## 5.2.3 Missing values ####
df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)
## 5.2.4 Exercises ####
# had an arrival delay of two or more hours
filter(flights, arr_delay >= 120)
# flew to Houston(IAH or HOU)
filter(flights, dest %in% c("IAH", "HOU"))
# were operated by United, American, or Delta
filter(flights, carrier %in% c("UA", "AA", "DL"))
# departed in summer (July, August, and September)
filter(flights, month %in% c(7, 8, 9))
# arrived more than two hours late, but didn't leave late
filter(flights, arr_delay > 120, dep_delay <= 0)
# were delayed by at least an hour, but made up over 30 minutes in flight
filter(flights, dep_delay >= 60, arr_delay < -30)
# departed between midnight and 6 am
filter(flights, dep_time <= 600)
# how many flights have a missing dep_time
filter(flights, is.na(dep_time))
# 5.3 Arrange rows with arrange() ####
# arrange() works similarly to filter() except that instead of selecting rows, it changes their order.
arrange(flights, year, month, day)
arrange(flights, desc(dep_delay))
df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))
arrange(df, desc(is.na(x)))
## 5.3.1 Exercises ####
# sort all missing values to the start.
arrange(flights, desc(is.na(dep_delay)))
# sort flights to find the most delayed flights. Find the flights that left earliest.
arrange(flights, desc(dep_delay))
# 5.4 Select columns with select() ####
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))
# There are a number of helper functions you can use within select():

# starts_with("abc"): matches names that begin with “abc”.
# ends_with("xyz"): matches names that end with “xyz”.
# contains("ijk"): matches names that contain “ijk”.
#matches("(.)\\1"): selects variables that match a regular expression. This one matches any variables that contain repeated characters. You’ll learn more about regular expressions in strings.
# num_range("x", 1:3): matches x1, x2 and x3.
df <- rename(flights, tail_num = tailnum)
select(flights, time_hour, air_time, everything())

## 5.4.1 Exercises ####

df <- select(flights, dep_time, dep_delay, arr_time, arr_delay)
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
df <- select(flights, any_of(vars))
df <- select(flights, contains("TIME"))

# 5.5 Add new variables with mutate() ####
# mutate() always adds new columns at the end of your dataset.
flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time)
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60)
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours)
# Only want to keep the new variables, use transmute()
transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours)
## 5.5.1 Useful creation functions ####
transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100)
(x <- 1:10)
lag(x)
lead(x)
cumsum(x)
cummean(x)
y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
min_rank(desc(y))
row_number(y)
dense_rank(y)
percent_rank(y)
cume_dist(y)
## 5.5.2 Exercises ####
#1.
df <- flights %>%
  select(dep_time, sched_dep_time) %>%
  mutate(dep_time = (dep_time %/% 100)*60 + (dep_time %% 100),
         sched_dep_time = (sched_dep_time %/% 100)*60 + (sched_dep_time %% 100))

df <- flights %>%
  mutate(dep_delay_rank = min_rank(dep_delay)) %>%
  arrange(desc(dep_delay_rank))

# 5.6 Grouped summaries with summarise() ####

summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

## 5.6.1 Combining multiple operations with the pipe ####

by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dest != "HNL")
ggplot(data = delay, mapping = aes(x=dist, y=delay))+
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

# Using pipe

delays <- flights %>%
  group_by(dest) %>%
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  filter(count > 20, dest != "HNL")

## 5.6.2 Missing values ####
flights %>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay))

flights %>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay, na.rm = TRUE))

not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay)) %>%
  print(n = Inf)

## 5.6.3 Counts ####
delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay)
  )
ggplot(data = delays, mapping = aes(x = delay)) +
  geom_freqpoly(binwidth = 10)

delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(data = delays, mapping = aes(x = n, y = delay))+
  geom_point(alpha=1/10)

delays %>%
  filter(n > 25) %>%
  ggplot(mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)

# Convert to a tibble so it prints nicely
batting <- as_tibble(Lahman::Batting)
batters <- batting %>%
  group_by(playerID) %>%
  summarize(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

batters %>%
  filter(ab > 100) %>%
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() +
  geom_smooth(se = FALSE)

batters %>%
  arrange(desc(ba))

## 5.6.4 Useful summary functions ####
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  )

not_cancelled %>%
  group_by(dest) %>%
  summarise(distance_sd = sd(distance)) %>%
  arrange(desc(distance_sd))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )

not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    first = quantile(dep_time, 0.25),
    last = quantile(dep_time, 0.75)
  )

not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    first_dep = first(dep_time),
    last_dep = last(dep_time)
  )

df <- not_cancelled %>%
  group_by(year, month, day) %>%
  mutate(r = min_rank(desc(dep_time))) %>%
  filter(r %in% range(r))

# Which destinations have the most carriers?
df <- not_cancelled %>%
  group_by(dest) %>%
  summarise(carriers = n_distinct(carrier)) %>%
  arrange(desc(carriers))

not_cancelled %>%
  count(dest)

not_cancelled %>%
  count(tailnum, wt = distance) # "count" (sum) the total number of miles a plane flew

# How many flights left before 5am?
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(n_early = sum(dep_time < 500))

# What proportion of flights are delayed by more than an hour?
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(hour_prop = mean(arr_delay > 60))

## 5.6.5 Grouping by multiple variables ####
daily <- group_by(flights, year, month, day)
(per_day <- summarise(daily, flights = n()))

(per_month <- summarise(per_day, flights = sum(flights)))

(per_year <- summarise(per_month, flights = sum(flights)))

## 5.6.6 Ungrouping ####

daily %>%
  ungroup() %>%            # no longer grouped by date
  summarise(flights = n()) # all flights

## 5.6.7 Exercises ####
#1.

not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    first = quantile(dep_time, 0.25),
    last = quantile(dep_time, 0.75)
  )

# 5.7 Grouped mutates (and filters) ####
# Find the worst members of each group
flights_sml %>%
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

# Find all groups bigger than a threshold:
popular_dests <- flights %>%
  group_by(dest) %>%
  filter(n() > 365)
# Standardize to compute per group metrics:
popular_dests %>%
  filter(arr_delay > 0) %>%
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>%
  select(year:day, dest, arr_delay, prop_delay)

