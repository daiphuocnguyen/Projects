library(nycflights13)
library(tidyverse)

flights
glimpse(flights)

# 4.1 Introduction ####
## 4.1.3 dplyr basics ####
flights |>
  filter(dest == "IAH") |>
  group_by(year, month, day) |>
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )
# 4.2 Rows ####
## 4.2.1 filter() ####
# filter() allows you to keep rows based on the values of the columns.
flights |>
  filter(dep_delay > 120)

# Flights that departed on January 1
flights |>
  filter(month == 1 & day == 1)

# Flights that departed in January or February
flights |>
  filter(month == 1 | month == 2)

# A shorter way to select flights that departed in January or February
flights |>
  filter(month %in% c(1, 2))

jan1 <- flights |>
  filter(month == 1 & day == 1)

## 4.2.2 Common mistakes ####
## 4.2.3 arrange() ####
# arrange() changes the order of the rows based on the values of the columns.
flights |>
  arrange(year, month, day, dep_time)

flights |>
  arrange(desc(dep_delay))

## 4.2.4 distinct() ####
# distinct() finds all the unique rows in a dataset, so in a technical sense, it primarily operates on the rows.
# Remove duplicate rows, if any
flights |>
  distinct()

# Find all unique origin and destination pairs
flights |>
  distinct(origin, dest)

flights |>
  distinct(origin, dest, .keep_all = TRUE) |>
  View()

flights |>
  count(origin, dest, sort = TRUE)

## 4.2.5. Exercises ####
flights |>
  filter(arr_delay >= 2 & dest %in% c("IAH", "HOU") & carrier %in% c("UA", "AA", "DL") & month %in% c(7, 8, 9) & arr_delay > 2 & dep_delay <= 0)

flights |>
  arrange(desc(dep_delay)) |>
  View()

flights |>
  arrange(desc(distance / air_time * 60)) |>
  View()
flights |>
  distinct(month, day) |>
  count()

# 4.3 Columns ####
# mutate() creates new columsn that are derived from the existing columns
# select() changes which columns are present
# rename() changes the names of the columns
# relocate() changes the positions of the columns

## 4.3.1 mutate() ####
flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60
  ) |>
  View()

flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .before = 1
  ) |>
  View()

flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .after = day
  ) |>
  View()

flights |>
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours,
    .keep = "used"
  ) |>
  View()

## 4.3.2 select() ####
flights |>
  select(year, month, day)

flights |>
  select(year:day)

flights |>
  select(!year:day)

flights |>
  select(where(is.character))

flights |>
  select(tail_num = tailnum)

## rename() ####
flights |>
  rename(tail_num = tailnum)

## 4.3.4 relocate() ####
flights |>
  relocate(time_hour, air_time)

flights |>
  relocate(year:dep_time, .after = time_hour) |>
  View()
flights |>
  relocate(starts_with("arr"), .before = dep_time) |>
  View()

## 4.3.5 Exercises ####
flights |>
  select(dep_time, sched_dep_time, dep_delay) |>
  View()

flights |>
  select(any_of(c("year", "month", "day", "dep_delay", "arr_delay")))

flights |> select(contains("TIME"))

flights |>
  rename(air_time_min = air_time) |>
  relocate(air_time_min)

flights |>
  arrange(arr_delay) |>
  select(tailnum)

# 4.4 The pipe ####
flights |>
  filter(dest == "IAH") |>
  mutate(speed = distance / air_time * 60) |>
  select(year:day, dep_time, carrier, flight, speed) |>
  arrange(desc(speed))

mtcars |> 
  group_by(cyl) |> 
  summarize(n = n())

# 4.5 Groups ####
## group_by() ####
flights |> 
  group_by(month)
## summarize()
flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay)
  )

flights |> 
  group_by(month) |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE)
  )

flights |> 
  group_by(month) |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  )

## 4.5.3 The slice_ functions ####
# find the flights that are most delayed upon arrival at each destination
flights |> 
  group_by(dest) |> 
  slice_max(arr_delay, n = 1) |> 
  relocate(dest)

flights |> 
  group_by(dest) |> 
  slice_max(arr_delay, n = 1, with_ties = FALSE) |> 
  relocate(dest)

## 4.5.4 Grouping by multiple variables ####
daily <- flights |> 
  group_by(year, month, day)
daily

daily_flights <- daily |> 
  summarize(n = n())

daily_flights <- daily |> 
  summarize(
    n = n(),
    .groups = "drop_last"
  )
daily_flights

## 4.5.5 Ungrouping ####
daily |> 
  ungroup()
daily |> 
  ungroup() |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    flights = n()
  )
## 4.5.6 .by ####
# .by argument to group within a single operation
flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .by = month
  ) |> 
  View()

flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .by = c(origin, dest)
  ) |> 
  View()

## 4.5.7 Exercises ####
flights |> 
  group_by(carrier, dest) |> 
  summarize(n())

# 4.6 Case study: aggregates and sample size ####
library(Lahman)
batters <- Lahman::Batting |> 
  group_by(playerID) |> 
  summarize(
    performance = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    n = sum(AB, na.rm = TRUE)
  )

batters |> 
  filter(n > 100) |> 
  ggplot(aes(x = n, y = performance)) +
  geom_point(alpha = 1 / 10) +
  geom_smooth(se = FALSE)

batters |> 
  arrange(desc(performance))

