library(tidyverse)
#14.2 String Basics ####
x <- "\u00b5"
x
## 14.2.1 String length ####
str_length(c("a", "R for data science", NA))
## 14.2.2 Combining strings ####
str_c("x", "y")
str_c("x", "y", sep = ", ")
str_c("Good job ", x)
x <- c("\"", "\\")
writeLines(x)
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
str_sub(x, -3, -1)
str_sub("a", 1, 5)
str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))

## 14.2.4 Locales ####
# Turkish has two i's: with and without a dot, and it
# has a different rule for capitalising them:
str_to_upper(c("i", "ı"))
#> [1] "I" "I"
str_to_upper(c("i", "ı"), locale = "tr")
#> [1] "İ" "I"

x <- c("apple", "eggplant", "banana")

str_sort(x, locale = "en")  # English
#> [1] "apple"    "banana"   "eggplant"

str_sort(x, locale = "haw") # Hawaiian
#> [1] "apple"    "eggplant" "banana"

## 14.2.5 Exercises ####
# 14.3 Matching patterns with regular expressions ####
## 14.3.1 Basic matches ####
x <- c("apple", "banana", "pear")
str_view(x, "an")
str_view(x, ".a.")

# To create the regular expression, we need \\
dot <- "\\."

# But the expression itself only contains one:
writeLines(dot)

# And this tells R to look for an explicit .
str_view(c("abc", "a.c", "bef"), "a\\.c")

x <- "a\\b"
writeLines(x)
str_view(x, "\\\\")
str_view(c("ab...c", ".a.c.f", "bef"), "\\..\\..\\..")

## 14.3.2 Anchors ####
x <- c("apple", "banana", "pear")
str_view(x, "^a")
str_view(x, "a$")

x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")
str_view(x, "^apple$")
## 14.3.3 Character classes and alternatives ####
# \d: matches any digit.
# \s: mathces any whitespace (e.g. space, tab, newline)
# [abc]: matches a, b, or c.
# [^abc]: matches anything except a, b, or c.

# Look for a literal character that normally has special meaning in a regex
str_view(c("abc", "a.c", "a*c", "a c"), "a[.]c")
str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c")
str_view(c("abc", "a.c", "a*c", "a c"), "a[ ]")
str_view(c("grey", "gray"), "gr(e|a)y")
## 14.3.4 Repetition ####
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")
str_view(x, "CC+")
str_view(x, 'C[LX]+')
str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,3}")
str_view(x, 'C{2,3}?')
str_view(x, 'C[LX]+?')

## 14.3.5 Grouping and backreferences ####
fruit
str_view(fruit, "(.)\\1", match = TRUE)
str_view(c("butter", "summer"), "(.)\1\1")
str_view(c("wowo"), "(..)\\2\\1")

# 14.4 Tools ####
## 14.4.1 Detect matches ####
x <- c("apple", "banana", "pear")
str_detect(x, "e")
# How many common words start with t?
sum(str_detect(words, "^t"))
# What proportion of common words end with a vowel?
mean(str_detect(words, "[aeiou]$"))

# Find all words containing at least one vowel, and negate
no_vowels_1 <- !str_detect(words, "[aeiou]")
# Find all words consisting only of consonants (non-vowels)
no_vowels_2 <- str_detect(words, "^[^aeiuou]+$")
identical(no_vowels_1, no_vowels_2)

words[str_detect(words, "x$")]
str_subset(words, "x$")
df <- tibble(
  word = words,
  i = seq_along(word)
)
df |> 
  filter(str_detect(word, "x$"))

x <- c("apple", "banana", "pear")
str_count(x, "a")

# On average, how many vowels per word?
mean(str_count(words, "[aeiou]"))

df |> 
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )

str_count("abababa", "aba")
str_view_all("abababa", "aba")

## 14.4.1.1 Exercises ####
df <- tibble(
  word = words,
  i = seq_along(words)
)

# Find all words that start or end with x.
str_subset(words, "[x]$")

# Find all words that start with a vowel and end with a consonant.
str_subset(words, "(^[aeiou]).*([^aeiou]$)+")

# Extract matches
length(sentences)
head(sentences)
colours <- c("red", "orange", "yellow", "green", "blue", "purple")
colour_match <- str_c(colours, collapse = "|")
colour_match
has_colour <- str_subset(sentences, colour_match)
matches <- str_extract(has_colour, colour_match)
matches
more <- sentences[str_count(sentences, colour_match) > 1]
str_view_all(more, colour_match)
str_extract_all(more, colour_match)

str_extract_all(more, colour_match, simplify = TRUE)
x <- c("a", "a b", "a b c")
str_extract_all(x, "[a-z]", simplify = TRUE)
head(sentences)

## 14.4.3 Grouped matches ####
noun <- "(a|the) ([^ ]+)" # "a" or "the", then a space, then a word.

has_noun <- sentences |> 
  str_subset(noun) |> 
  head(10)
has_noun |> 
  str_extract(noun)

has_noun |> 
  str_match(noun)

tibble(sentence = sentences) |> 
  tidyr::extract(
    sentence, c("article", "noun"), "(a|the) ([^ ]+)",
    remove = FALSE
  )

# 14.4.4 Replacing matches ####
x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "_")
str_replace_all(x, "[aeiou]", "_")

x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three"))
sentences |> 
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") |> 
  head(5)

## 14.4.5 Splitting ####
sentences |> 
  head(5) |> 
  str_split(" ")

"a|b|c|d" |> 
  str_split("\\|")

sentences |> 
  head(5) |> 
  str_split(" ", simplify = TRUE)
fields <- c("Name: Hadley", "Country: NZ", "Age: 35")
fields |> str_split(": ", n = 2, simplify = TRUE)

x <- "This is a sentence. This is another sentence."
str_view_all(x, boundary("word"))

str_split(c("apples, pears, and bananas"), boundary("word"))

## 14.4.6 Find matches ####

# 14.5 Other types of pattern ####
# The regular call:
str_view(fruit, "nana")
# Is shorthand for
str_view(fruit, regex("nana"))

bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")
bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")
str_view(bananas, regex("banana", ignore_case = TRUE))

x <- "Line 1\nLine 2\nLine 3"
str_extract_all(x, "^Line")[[1]]
str_extract_all(x, regex("^Line", multiline = TRUE))[[1]]

phone <- regex("
  \\(?     # optional opening parens
  (\\d{3}) # area code
  [) -]?   # optional closing parens, space, or dash
  (\\d{3}) # another three numbers
  [ -]?    # optional space or dash
  (\\d{3}) # three more numbers
  ", comments = TRUE)

str_match("514-791-8141", phone)
#>      [,1]          [,2]  [,3]  [,4] 
#> [1,] "514-791-814" "514" "791" "814"

# install.packages("microbenchmark")
library(microbenchmark)
microbenchmark::microbenchmark(
  fixed = str_detect(sentences, fixed("the")),
  regex = str_detect(sentences, "the"),
  times = 20
)

a1 <- "\u00e1"
a2 <- "a\u0301"
c(a1, a2)
#> [1] "á" "á"
a1 == a2
#> [1] FALSE

str_detect(a1, fixed(a2))
#> [1] FALSE
str_detect(a1, coll(a2))
#> [1] TRUE

# That means you also need to be aware of the difference
# when doing case insensitive matches:
i <- c("I", "İ", "i", "ı")
i
#> [1] "I" "İ" "i" "ı"

str_subset(i, coll("i", ignore_case = TRUE))
#> [1] "I" "i"
str_subset(i, coll("i", ignore_case = TRUE, locale = "tr"))
#> [1] "İ" "i"
stringi::stri_locale_info()

x <- "This is a sentence."
str_view_all(x, boundary("word"))
apropos("replace")
head(dir(pattern = "\\.Rmd$"))
