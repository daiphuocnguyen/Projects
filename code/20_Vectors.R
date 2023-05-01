library(tidyverse)
typeof(letters)
typeof(1:10)
x <- list("a", "b", 1:10)
length(x)
1:10 %% 3 == 0
c(TRUE, TRUE, FALSE, NA)
typeof(1)
typeof(1L)
1.5L
x <- sqrt(2) ^ 2
x - 2
c(-1, 0, 1) / 0

x <- "This is a reasonably long string."
pryr::object_size(x)

y <- rep(x, 1000)
pryr::object_size(y)
NA
NA_integer_
NA_real_
NA_character_

as.logical()
as.integer()
as.double()
as.character()
x <- sample(20, 100, replace = TRUE)
y <- x > 10
sum(y)
mean(y)
typeof(c(TRUE, 1L))
typeof(c(1L, 1.5))
typeof(c(1.5, "a"))

sample(10) + 100
runif(10) > 0.5
1:10 + 1:2
tibble(x = 1:4, y = 1:2)
tibble(x = 1:4, y = rep(1:2, 2))
tibble(x = 1:4, y = rep(1:2, each = 2))
c(x = 1, y = 2, z = 4)
set_names(1:3, c("a", "b", "c"))

## 20.4.5 Subsetting ####
x <- c("one", "two", "three", "four", "five")
x[c(3,2,5)]

x[c(1, 1, 5, 5, 5, 2)]

x[c(-1, -3, -5)]

x[0]

x <- c(10, 3, NA, 5, 8, 1, NA)

# All non-missing values of x
x[!is.na(x)]

# All even (or missing!) values of x
x[x %% 2 == 0]

x <- c(abc = 1, def = 2, xyz = 5)
x[c("xyz", "def")]

# 20.5 Recursive vectors (lists) ####
x <- list(1, 2, 3)
x
str(x)
x_named <- list(a = 1, b = 2, c = 3)
str(x_named)
y <- list("a", 1L, 1.5, TRUE)
str(y)
z <- list(list(1, 2), list(3, 4))
str(z)

## 20.5.1 Visualising lists ####
x1 <- list(c(1, 2), c(3, 4))
x2 <- list(list(1, 2), list(3, 4))
x3 <- list(1, list(2, list(3)))

## 20.5.2 Subsetting ####
a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))
str(a[1:2])
str(a[4])
str(a[[1]])
str(a[[4]])

a$a
a[["a"]]
a
a[1:2]
a[4]
a[[4]]
a[[4]][1]
a[[4]][[1]]

## 20.5.4 Exercises ####

# 20.6 Attributes ####
x <- 1:10
attr(x, "greeting")
attr(x, "greeting") <- "Hi!"
attr(x, "farewell") <- "Bye!"
attributes(x)
as.Date
methods("as.Date")
getS3method("as.Date", "default")
getS3method("as.Date", "numeric")

# 20.7 Augmented vectors ####
## 20.7.1 Factors ####
x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
typeof(x)
attributes(x)
## 20.7.2 Dates and date-times ####
x <- as.Date("1971-01-01")
unclass(x)
typeof(x)
attributes(x)
x <- lubridate::ymd_hm("1970-01-01 01:00")
unclass(x)
typeof(x)
attributes(x)
attr(x, "tzone") <- "US/Pacific"
x
attr(x, "tzone") <- "US/Eastern"
x
y <- as.POSIXlt(x)
attributes(y)

## 20.7.3 Tibbles ####
tb <- tibble::tibble(x = 1:5, y = 5:1)
typeof(tb)
attributes(tb)

df <- data.frame(x = 1:5, y = 5:1)
typeof(df)
attributes(df)
