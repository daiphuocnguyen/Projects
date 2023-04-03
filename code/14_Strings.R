library(tidyverse)

string1 <- "This is a string"
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'

double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'"

x <- c("\"", "\\")
x
#> [1] "\"" "\\"
writeLines(x)
#> "
#> \

x <- "\u00b5"
x
#> [1] "µ"

c("one", "two", "three")
#> [1] "one"   "two"   "three"

## 14.2.1 String length ####
str_length(c("a", "R for data science", NA))
#> [1]  1 18 NA

## 14.2.2 Combining strings ####
str_c("x", "y")
str_c("x", "y", "z")

str_c("x", "y", sep = ", ")

x <- c("abc", NA)
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|")

str_c("prefix-", c("a", "b", "c"), "-suffix")

name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE

str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
)

str_c(c("x", "y", "z"), collapse = ", ")

## 14.2.3 Subsetting strings ####
x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)
