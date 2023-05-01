library(tidyverse)

# 21.2 For loops ####
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

median(df$a)

median(df$b)

median(df$c)

median(df$d)

output <- vector("double", ncol(df)) # 1. output
for (i in seq_along(df)) {           # 2. sequence
  output[[i]] <- median(df[[i]])     # 3. body  
}
output

y <- vector("double", 0)
seq_along(y)
1:length(y)

## 21.2.1 Exercises ####
# To compute the mean of every column in mtcars.
output <- vector("double", ncol(mtcars))
names(output) <- names(mtcars)
for (i in names(mtcars)) {
  output[i] <- mean(mtcars[[i]])
}
output
library(nycflights13)
# Determine the type of each column in nycflights13::flights.
output <- vector("list", ncol(nycflights13::flights))
names(output) <-names(nycflights13::flights)

for (i in names(nycflights13::flights)) {
  output[[i]] <- class(nycflights13::flights[[i]])
}
output

# To compute the number of unique values in each column of the iris dataset.
data("iris")
iris_uniq <- vector("double", ncol(iris))
names(iris_uniq) <- names(iris)
for (i in names(iris)) {
  iris_uniq[i] <- n_distinct(iris[[i]])
}
iris_uniq

# To generate 10 random normals for each of μ = -10, 0, 10, and 100.
# number to draw
n <- 10
# values of the mean
mu <- c(-10, 0, 10, 100)
normals <- vector("list", length(mu))
for (i in seq_along(normals)) {
  normals[[i]] <- rnorm(n, mu[i])
}
normals

matrix(rnorm(n * length(mu), mean = mu), ncol = n)

str_c(letters, collapse = "")

x <- sample(100)
sd. <- 0
for (i in seq_along(x)) {
  sd. <- sd. + (x[i] - mean(x))^2
}
sd. <- sqrt(sd. / (length(x) - 1))
sd.
#> [1] 29
sd(x)

x <- runif(100)
out <- vector("numeric", length(x))
out[1] <- x[1]
for (i in 2:length(x)) {
  out[i] <- out[i - 1] + x[i]
}
out
all.equal(cumsum(x), out)
cumsum(x)

output <- vector("integer", 0)
for (i in seq_along(x)) {
  output <- c(output, lengths(x[[i]]))
}
output

add_to_vector <- function(n) {
  output <- vector("integer", 0)
  for (i in seq_len(n)) {
    output <- c(output, i)
  }
  output
}

add_to_vector_2 <- function(n) {
  output <- vector("integer", n)
  for (i in seq_len(n)) {
    output[[i]] <- i
  }
  output
}

library(microbenchmark)

timings <- microbenchmark(add_to_vector(10000), add_to_vector_2(10000), times = 10)
timings

# 21.3 For loop variations ####
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

for (i in seq_along(df)) {
  df[[i]] <- rescale01(df[[i]])
}

## 21.3.2 Looping patterns ####
results <- vector("list", length(x))
names(results) <- names(x)
for (i in seq_along(x)) {
  name <- names(x)[[i]]
  value <- x[[i]]
}

# 21.3.3 Unknown output length ####
means <- c(0, 1, 2)

output <- double()
for (i in seq_along(means)) {
  n <- sample(100, 1)
  output <- c(output, rnorm(n, means[[i]]))
}
str(output)

# better
out <- vector("list", length(means))
for (i in seq_along(means)) {
  n <- sample(100, 1)
  out[[i]] <- rnorm(n, means[[i]])
}
str(out)
str(unlist(out))

## 21.3.4 Unknown sequence length ####
# To find how many tries it takes to get three heads in a row
flip <- function() sample(c("T", "H"), 1)
flips <- 0
nheads <- 0

while (nheads < 3) {
  if (flip() == "H") {
    nheads <- nheads + 1
  } else {
    nheads <- 0
  }
  flips <- flips + 1
}
flips

## 21.3.5 Exercises ####
files <- dir("data/", pattern = "\\.csv$", full.names = TRUE)
files
df_list <- vector("list", length(files))

for (i in seq_along(files)) {
  df_list[[i]] <- read_csv(files[[i]])
}

print(df_list)

x <- c(11, 12, 13)
print(names(x))

for (nm in names(x)) {
  print(nm)
  print(x[[nm]])
}

x <- c(a = 11, 12, c = 13)
print(names(x))

for (nm in names(x)) {
  print(nm)
  print(x[[nm]])
}

x <- c(a = 11, a = 12, c = 13)
print(names(x))

for (nm in names(x)) {
  print(nm)
  print(x[[nm]])
}

# Write a function that prints the mean of each numeric column in a data frame, along with its name.
show_mean <- function(df, digits = 2) {
  # Get max length of all variable names in the dataset
  maxstr <- max(str_length(names(df)))
  for (nm in names(df)) {
    if (is.numeric(df[[nm]])) {
      cat(
        str_c(str_pad(str_c(nm, ":"), maxstr + 1L, side = "right"),
              format(mean(df[[nm]]), digits = digits, nsmall = digits),
              sep = " "
              ),
        "\n"
      )
    }
  }
}
show_mean(iris)

trans <- list(
  disp = function(x) x * 0.0163871,
  am = function(x) {
    factor(x, labels = c("auto", "manual"))
  }
)
for (var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}

trans[["disp"]]
trans[["disp"]](mtcars[["disp"]])

# 21.4 For loops vs. functionals ####

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

output <- vector("double", length(df))
for (i in seq_along(df)) {
  output[[i]] <- mean(df[[i]])
}
output

col_mean <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[[i]] <- mean(df[[i]])
  }
  output
}

col_median <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[[i]] <- median(df[[i]])
  }
  output
}

col_sd <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[[i]] <- sd(df[[i]])
  }
  output
}

f1 <- function(x) abs(x - mean(x)) ^ 1
f2 <- function(x) abs(x - mean(x)) ^ 2
f3 <- function(x) abs(x - mean(x)) ^ 3

f <- function(x, i) abs(x - mean(x)) ^ i

col_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  }
  out
}

col_summary(df, median)
col_summary(df, mean)

## 21.4.1 Exercises ####
x <- matrix(rnorm(15), nrow = 5)
apply(x, 1, mean)
apply(x, 2, mean)

col_summary2 <- function(df, fun) {
  # create an empty vector which will store whether each
  # column is numeric
  numeric_cols <- vector("logical", length(df))
  # test whether each column is numeric
  for (i in seq_along(df)) {
    numeric_cols[[i]] <- is.numeric(df[[i]])
  }
  # find the indexes of the numeric columns
  idxs <- which(numeric_cols)
  # find the number of numeric columns
  n <- sum(numeric_cols)
  # create a vector to hold the results
  out <- vector("double", n)
  # apply the function only to numeric vectors
  for (i in seq_along(idxs)) {
    out[[i]] <- fun(df[[idxs[[i]]]])
  }
  # name the vector
  names(out) <- names(df)[idxs]
  out
}

df <- tibble(
  X1 = c(1, 2, 3),
  X2 = c("A", "B", "C"),
  X3 = c(0, -1, 5),
  X4 = c(TRUE, FALSE, TRUE)
)
col_summary2(df, mean)
#>   X1   X3 
#> 2.00 1.33

col_summary2(df, median)

# 21.5 The map functions ####
map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)

df |> map_dbl(mean)
df |> map_dbl(median)
df |> map_dbl(sd)

map_dbl(df, mean, trim = 0.5)
z <- list(x = 1:3, y = 4:5)
map_int(z, length)

## 21.5.1 Shortcuts ####
models <- mtcars %>%
  split(.$cyl) %>%
  map(function(df) lm(mpg ~ wt, data = df))

models <- mtcars %>%
  split(.$cyl) %>%
  map(~lm(mpg ~ wt, data = .))
models %>%
  map(summary) %>%
  map_dbl(~.$r.squared)

models %>%
  map(summary) %>%
  map_dbl("r.squared")

x <- list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
x |>  map_dbl(2)

## 21.5.2 Base R ####
x1 <- list(
  c(0.27, 0.37, 0.57, 0.91, 0.20),
  c(0.90, 0.94, 0.66, 0.63, 0.06), 
  c(0.21, 0.18, 0.69, 0.38, 0.77)
)
x2 <- list(
  c(0.50, 0.72, 0.99, 0.38, 0.78), 
  c(0.93, 0.21, 0.65, 0.13, 0.27), 
  c(0.39, 0.01, 0.38, 0.87, 0.34)
)

threshold <- function(x, cutoff = 0.8) x[x > cutoff]
x2 %>% sapply(threshold) %>% str()
vapply(df, is.numeric, logical(1))
map_lgl(df, is.numeric)

## 21.5.3 Exercises ####
map_dbl(mtcars, mean)
map_chr(mtcars, typeof)
map_int(iris, n_distinct)
map_dbl(iris, n_distinct)
map_int(iris, function(x) length(unique(x)))
map_int(iris, ~length(unique(.x)))
map(c(-10,0,10,100), ~rnorm(n = 10, mean = .))
is.factor(diamonds$color)
map_lgl(diamonds, is.factor)
map(1:5, runif)
map(c(TRUE, FALSE, TRUE), ~ ! .)
map(c("Hello", "World"), str_to_upper)
map(1:5, ~ rnorm(.))
map(c(-0.5, 0, 1), ~ rnorm(1, mean = .))
map(1:5, runif)
list(
  runif(1),
  runif(2),
  runif(3),
  runif(4),
  runif(5)
)
map(-2:2, rnorm, n = 5)
map_dbl(-2:2, rnorm, n = 5)
map(-2:2, rnorm, n = 5) |> 
  flatten_dbl()
x <- split(mtcars, mtcars$cyl)
map(x, ~ lm(mpg ~ wt, data = .))
run_reg <- function(df) {
  lm(mpg ~wt, data = df)
}

map(x, run_reg)
safe_log <- safely(log)
str(safe_log(10))

x <- list(1, 10, "a")
y <- x |> map(safely(log))
str(y)
y <- y |>  transpose()
str(y)
is_ok <- y$error %>% map_lgl(is_null)
x[!is_ok]
y$result[is_ok] %>% flatten_dbl()

x <- list(1, 10, "a")
x %>% map_dbl(possibly(log, NA_real_))

x <- list(1, -1)
x %>% map(quietly(log)) %>% str()

# 21.7 Mapping over multiple arguments ####
mu <- list(5, 10, -3)
mu |> 
  map(rnorm, n = 5) |> 
  str()

sigma <- list(1, 5, 10)
seq_along(mu) |> 
  map(~rnorm(5, mu[[.]], sigma[[.]])) |> 
  str()

map2 <- function(x, y, f, ...) {
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], y[[i]], ...)
  }
  out
}

n <- list(1, 3, 5)
args1 <- list(n, mu, sigma)
args1 |> 
  pmap(rnorm) |> 
  str()

params <- tribble(
  ~mean, ~sd, ~n,
    5,     1,  1,
   10,     5,  3,
   -3,    10,  5
)

params %>% 
  pmap(rnorm)

## 21.7.1 Invoking different functions ####
f <- c("runif", "rnorm", "rpois")
param <- list(
  list(min = -1, max = 1),
  list(sd = 5),
  list(lambda = 10)
)

invoke_map(f, param, n = 5) |> str()

sim <- tribble(
  ~f, ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)

sim |> 
  mutate(sim = invoke_map(f, params, n = 10))

# 21.8 Walk ####
x <- list(1, "a", 3)
x |> 
  walk(print)

library(ggplot2)
plots <- mtcars %>% 
  split(.$cyl) %>% 
  map(~ggplot(., aes(mpg, wt)) + geom_point())
paths <- stringr::str_c(names(plots), ".pdf")

pwalk(list(paths, plots), ggsave, path = tempdir())

# 21.9 Other patterns of for loops ####
## 21.9.1 Predicate functions ####
iris %>% 
  keep(is.factor) %>% 
  str()

iris %>% 
  discard(is.factor) %>% 
  str()

x <- list(1:5, letters, list(10))
x
x %>% 
  some(is_character)
x %>% 
  every(is_vector)

x <- sample(10)
x
x %>% 
  detect(~ . > 5)

x %>% 
  detect_index(~ . > 5)
x %>% 
  head_while(~ . > 5)
x %>% 
  tail_while(~ . > 5)

# 21.9.2 Reduce and accumulate ####
dfs <- list(
  age = tibble(name = "John", age = 30),
  sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
  trt = tibble(name = "Mary", treatment = "A")
)
dfs
dfs %>% reduce(full_join)

vs <- list(
  c(1, 3, 5, 6, 10),
  c(1, 2, 3, 7, 8, 10),
  c(1, 2, 3, 4, 8, 9, 10)
)

vs %>% reduce(intersect)

x <- sample(10)
x
x %>% accumulate(`+`)

## Exercise 21.9.1 ####
# Use ... to pass arguments to the function
every2 <- function(.x, .p, ...) {
  for (i in .x) {
    if (!.p(i, ...)) {
      # If any is FALSE we know not all of them were TRUE
      return(FALSE)
    }
  }
  # if nothing was FALSE, then it is TRUE
  TRUE
}

every2(1:3, function(x) {
  x > 1
})

col_sum2 <- function(df, f, ...) {
  map(keep(df, is.numeric), f, ...)
}

col_sum2(iris, mean)

col_sum3 <- function(df, f) {
  is_num <- sapply(df, is.numeric)
  df_num <- df[, is_num]
  sapply(df_num, f)
}

df <- tibble(
  x = 1:3,
  y = 3:1,
  z = c("a", "b", "c")
)

col_sum3(df, mean)
col_sum3(df[1:2], mean)
col_sum3(df[1], mean)
col_sum3(df[0], mean)
