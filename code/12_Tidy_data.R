# 12.1 Introduction ####
## 12.1.1 Prerequisites ####
library(tidyverse)

# 12.2 Tidy data ####
library(gapminder)
library(Lahman)
table1
table2
table3
table4a
table4b

# Compute rate per 10,000
table1 %>%
  mutate(rate = cases / population * 10000)

# Compute cases per year
table1 %>%
  count(year, wt = cases)

# Visualise changes over time
library(ggplot2)
ggplot(table1, aes(year, cases)) +
  geom_line(aes(group = country), colour = "grey50") +
  geom_point(aes(colour = country))

## 12.2.1 Exercises ####
my_df <- table2 %>%
  filter(type == "cases") %>%
  count(country, wt = count)
my_df

# 12.3 Pivoting ####
## 12.3.1 Longer ####
table4a %>%
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")

table4b %>%
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "Population")
## 12.3.2 Wider ####
table2 %>%
  pivot_wider(names_from = type, values_from = count)

## 12.3.3 Exercises ####
stocks <- tibble(
  year = c(2015, 2015, 2016, 2016),
  half = c(1, 2, 1, 2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>%
  pivot_wider(names_from = year, values_from = return) %>%
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return")

people <- tribble(
  ~name,               ~names, ~values,
  #------------------|----------|------
  "Phillip Woods",     "age",     45,
  "Phillip Woods",     "height", 186,
  "Phillip Woods",     "age",     50,
  "Jessica Cordedo",   "age",     37,
  "Jessica Cordedo",   "height", 156
)
people %>%
  pivot_wider(names_from = names, values_from = values)

preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,     10,
  "no",      20,     12
)
preg

# 12.4 Separating and uniting ####

## 12.4.1 Separate ####
table3 %>%
  separate(rate, into = c("cases", "population"))

table3 %>%
  separate(rate, into = c("cases", "population"), sep = "/")

table3 %>%
  separate(rate, into = c("cases", "population"), convert = TRUE)

table3 %>%
  separate(year, into = c("century", "year"), sep = 2)

table5 %>%
  unite(new, century, year)

table5 %>%
  separate(rate, into = c("cases", "population"), convert = TRUE) %>%
  unite(year, century, year, sep = "") %>%
  mutate(proportion = cases / population)

## 12.4.3 Exercises ####
tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three") )

# 12.5 Missing values ####
stocks <- tibble(
  year = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr = c(1, 2, 3, 4, 2, 3, 4),
  return = c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.66)
)
stocks
stocks %>%
  pivot_wider(names_from = year, values_from = return)

stocks %>%
  pivot_wider(names_from = year, values_from = return) %>%
  pivot_longer(
    cols = c(`2015`, `2016`),
    names_to = "year",
    values_to = "return",
    values_drop_na = TRUE
  )
stocks %>%
  complete(year, qtr)

treatment <- tribble(
  ~person, ~treatment, ~response,
  "Derrick Whitmore", 1, 7,
  NA, 2, 10,
  NA, 3, 9,
  "Katherine Burke", 1, 4
)

treatment %>%
  fill(person)

# 12.6 Case Study ####
who1 <- who %>%
  pivot_longer(
    cols = new_sp_m014:newrel_f65,
    names_to = "key",
    values_to = "cases",
    values_drop_na = TRUE
  )
who1
who1 %>%
  count(key)

who2 <- who1 %>%
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
who2

who3 <- who2 %>%
  separate(key, c("new", "type", "sexage"), sep = "_")

who3
who3 %>%
  count(new)
who4 <- who3 %>%
  select(-new, -iso2, -iso3)
who4

who5 <- who4 %>%
  separate(sexage, c("sex", "age"), sep = 1)
who5

# The entire code to tidy who data set
tb_data <- who %>%
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

tb_sum <- tb_data %>%
  group_by(country, year, sex) %>%
  summarise(total_cases = sum(cases))

head(tb_sum)

ggplot(tb_sum, aes(x = year, y = total_cases, fill = sex)) +
  geom_bar(stat = "identity") +
  facet_wrap(~country, nrow = 4) +
  labs(title = "Total number of TB Cases by Country, Year, and Sex",
       x = "Year",
       y = "Total Number of Cases",
       fill = "Sex") +
  theme_bw()

