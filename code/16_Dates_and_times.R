## 16.1.1 Prerequisites ####
library(tidyverse)
library(lubridate)
library(nycflights13)

# 16.2 Creating date/times ####
today()
now()
## 16.2.1 From strings ####
ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")
ymd(20170131)
ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")
ymd(20170131, tz = "UTC")

## 16.2.2 From individual components ####
flights |> 
  select(year, month, day, hour, minute)

flights |> 
  select(year, month, day, hour, minute) |> 
mutate(departure = make_datetime(year, month, day, hour, minute))

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights |> 
  filter(!is.na(dep_time), !is.na(arr_time)) |> 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) |> 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt

flights_dt |> 
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

flights_dt |> 
  filter(dep_time < ymd(20130102)) |> 
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 600) # 600 s = 10 minutes

## 16.2.3 From other types ####
as_datetime(today())
as_date(now())
as_datetime(60 * 60 * 10)
as_date(365 * 10 + 2)

## 16.2.4 Exercises ####
ymd(c("2010-10-10", "bananas"))
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014
mdy(d1)
ymd(d2)
dmy(d3)
mdy(d4)
mdy(d5)

# 6.3 Date-time components ####
datetime <- ymd_hms("2016-07-08 12:34:56")
year(datetime)
month(datetime)
mday(datetime)
yday(datetime)
wday(datetime)
month(datetime, label = TRUE)
wday(datetime, label = TRUE, abbr = FALSE)

flights_dt |> 
  mutate(wday = wday(dep_time, label = TRUE)) |> 
  ggplot(aes(x = wday)) +
  geom_bar()

flights_dt |> 
  mutate(minute = minute(dep_time)) |> 
  group_by(minute) |> 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()) |> 
  ggplot(aes(minute, avg_delay)) +
  geom_line()

sched_dep <- flights_dt %>% 
  mutate(minute = minute(sched_dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n())

ggplot(sched_dep, aes(minute, avg_delay)) +
  geom_line()

ggplot(sched_dep, aes(minute, n)) +
  geom_line()

## 16.3.2 Rounding
flights_dt |> 
  count(week = floor_date(dep_time, "week")) |> 
  ggplot(aes(week, n)) +
  geom_line()

## 16.3.3 Setting components ####
(datetime <- ymd_hms("2016-07-08 12:34:56"))
year(datetime) <- 2020
month(datetime) <- 01
datetime
hour(datetime) <- hour(datetime) + 1
datetime
update(datetime, year = 2020, month = 2, mday = 2, hour = 2)
ymd("2015-02-01") |> 
  update(mday = 30)
ydm("2015-02-01") |> 
  update(hour = 400)
flights_dt |> 
  mutate(dep_hour = update(dep_time, yday = 1)) |> 
  ggplot(aes(dep_hour)) +
  geom_freqpoly(binwidth = 300)

## 16.3.4 Exercises ####
flights_dt %>%
  mutate(sched_dep_hour = hour(sched_dep_time)) %>%
  group_by(sched_dep_hour) %>%
  summarise(dep_delay = mean(dep_delay)) %>%
  ggplot(aes(y = dep_delay, x = sched_dep_hour)) +
  geom_point() +
  geom_smooth()

flights_dt |> 
  mutate(dow = wday(sched_dep_time)) |> 
  group_by(dow) |> 
  summarise(
    dep_delay = mean(dep_delay),
    arr_delay = mean(arr_delay, na.rm = TRUE)
  ) |> 
  print(n = Inf)

flights_dt |> 
  mutate(wday = wday(dep_time, label = TRUE)) |> 
  group_by(wday) |> 
  summarize(ave_dep_delay = mean(dep_delay, na.rm = TRUE)) |> 
  ggplot(aes(x = wday, y = ave_dep_delay)) +
  geom_bar(stat = "identity")

flights_dt |> 
  mutate(wday = wday(dep_time, label = TRUE)) |> 
  group_by(wday) |> 
  summarise(ave_arr_delay = mean(arr_delay, na.rm = TRUE)) |> 
  ggplot(aes(x = wday, y = ave_arr_delay)) +
  geom_bar(stat = "identity")

ggplot(diamonds, aes(x = carat)) +
  geom_density()
ggplot(diamonds, aes(x = carat %% 1 * 100)) +
  geom_histogram(binwidth = 1)
ggplot(flights_dt, aes(x = minute(sched_dep_time))) +
  geom_histogram(binwidth = 1)

flights_dt |> 
  mutate(minute = minute(dep_time),
         early = dep_delay < 0) |> 
  group_by(minute) |> 
  summarise(
    early = mean(early, na.rm = TRUE),
    n = n()) |> 
  ggplot(aes(minute, early)) +
  geom_line()

# 16.4 Time spans ####
## 16.4.1 Durations ####
# How old is Hadley?
h_age <- today() - ymd(19791014)
h_age
as.duration(h_age)
dseconds(15)
dminutes(10)
dhours(c(12, 24))
ddays(0:5)
dweeks(3)
dyears(1)
2 * dyears(1)
dyears(1) + dweeks(12) + dhours(15)
tomorrow <- today() + ddays(1)
tomorrow
last_year <- today() - dyears(1)
last_year
one_pm <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York")
one_pm
one_pm + ddays(1)
## 16.4.2 Periods ####
one_pm 
one_pm + days(1)
seconds(15)
minutes(10)
hours(c(12, 24))
days(7)
months(1:6)
weeks(3)
years(1)
10 * (months(6) + days(1))
days(50) + hours(25) + minutes(2)
# A leap year
ymd("2016-12-31") + dyears(1)
ymd("2016-01-01") + years(1)

# Daylight Savings Time
one_pm + ddays(1)
one_pm + days(1)
one_pm

flights_dt |> 
  filter(arr_time < dep_time) |> View()

flights_dt <- flights |> 
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight * 1),
    sched_arr_time = sched_arr_time + days(overnight * 1)
  )

flights_dt |> 
  filter(overnight, arr_time < dep_time)

## 16.4.3 Intervals ####
years(1) / days(1)
next_year <- today() + years(1)
(today() %--% next_year) / ddays(1)

(today() %--% next_year) %/% days(1)

## Exercise 16.4.3 ####
ymd("2015-01-01") + months(0:11)
floor_date(today(), unit = "year") + months(0:11)

age <- function(bday) {
  (bday %--% today()) %/% years(1)
}
age(ymd("1992-11-06"))

(today() %--% (today() + years(1))) %/% months(1)

(today() %--% (today() + years(1))) / days(30)
as.Date("2016-02-29") + years(1)

Sys.timezone()
length(OlsonNames())
head(OlsonNames())

(x1 <- ymd_hms("2015-06-01 12:00:00", tz = "America/New_York"))
#> [1] "2015-06-01 12:00:00 EDT"
(x2 <- ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen"))
#> [1] "2015-06-01 18:00:00 CEST"
(x3 <- ymd_hms("2015-06-02 04:00:00", tz = "Pacific/Auckland"))
#> [1] "2015-06-02 04:00:00 NZST"
x1 - x2
x1 - x3
x4 <- c(x1, x2, x3)
x4
x4a <- with_tz(x4, tzone = "Australia/Lord_Howe")
x4a
x4a - x4
x4b <- force_tz(x4, tzone = "Australia/Lord_Howe")
x4b
#> [1] "2015-06-01 12:00:00 +1030" "2015-06-01 12:00:00 +1030"
#> [3] "2015-06-01 12:00:00 +1030"
x4b - x4
#> Time differences in hours
#> [1] -14.5 -14.5 -14.5
