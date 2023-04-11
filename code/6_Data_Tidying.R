library(tidyverse)
# 6.2 Tidy data ####
table1
table2
table3
# Compute rate per 10,000
table1 |> 
  mutate(rate = cases / population * 10000)

