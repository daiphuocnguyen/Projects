# 12.1 Introduction ####
## 12.1.1 Prerequisites ####
library(tidyverse)

# 12.2 Tidy Data ####
table1
table2
table3
table4a
table4b

# Compute rate per 10,000
table1 |> 
  mutate(rate = cases / population * 10000)

# Compute cases per year
table1 |> 
  count(year, wt = cases)

# Visualize changes over time
library(ggplot2)
ggplot(table1, aes(year, cases)) +
  geom_line(aes(group = country), colour = "grey50") +
  geom_point(aes(colour = country))

## 12.2.1 Exercises ####

# 12.3 Pivoting ####
## 12.3.1 Longer ####
tidy4a <- table4a |> 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")

tidy4b <- table4b |> 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population")

left_join(tidy4a, tidy4b)

## 12.3.2 Wider ####
table2 |> 
  pivot_wider(names_from = type, values_from = count)

## 12.3.3 Exercises ####
stocks <- tibble(
  year = c(2015, 2015, 2016, 2016),
  half = c(1, 2, 1, 2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks |> 
  pivot_wider(names_from = year, values_from = return) |> 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return")

people <- tribble(
  ~name,             ~names,  ~values,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)
people |> 
  pivot_wider(names_from = names, values_from = values)

preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes", NA, 10,
  "no", 20, 12
)
preg |> 
  pivot_longer(c(male, female), names_to = "sex", values_to = "count")

# 12.4 Separating and uniting ####
## 12.4.1 Separate ####
table3 |> 
  separate(rate, into = c("cases", "population"))

table3 |> 
  separate(rate, into = c("cases", "population"), sep = "/")

table3 |> 
  separate(rate, into = c("cases", "population"), convert = TRUE)

table3 |> 
  separate(year, into = c("century", "year"), sep = 2)

## 12.4.2 Unite ####
table5 |> 
  unite(new, century, year)

table5 |> 
  unite(new, century, year, sep = "")

## 12.4.3 Exercises ####
tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) |> 
  separate(x, c("one", "two", "three"))

tibble(x = c("a,b,c", "d,e", "h,i,j")) |> 
  separate(x, c("one", "two", "three"))

# 12.5 Missing values ####
stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
stocks |> 
  pivot_wider(names_from = year, values_from = return)

stocks |> 
  pivot_wider(names_from = year, values_from = return) |> 
  pivot_longer(
    cols = c(`2015`, `2016`),
    names_to = "year",
    values_to = "return",
    values_drop_na = TRUE
  )
stocks |> 
  complete(year, qtr)

treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)

treatment |> 
  fill(person)

# 12.6 Case Study ####
who
who1 <- who |> 
  pivot_longer(cols = new_sp_m014:newrel_f65,
               names_to = "key",
               values_to = "cases",
               values_drop_na = TRUE
               )

who1 |> 
  count(key)

who2 <- who1 |> 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))

who3 <- who2 |> 
  separate(key, c("new", "type", "sexage"), sep = "_")

who3 |> 
  count(new)

who4 <- who3 |> 
  select(-new, -iso2, -iso3)

who5 <- who4 |> 
  separate(sexage, c("sex", "age"), sep = 1)

who %>%
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases", 
    values_drop_na = TRUE
  ) %>% 
  mutate(
    key = stringr::str_replace(key, "newrel", "new_rel")
  ) %>%
  separate(key, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)
