library(tidyr)

# create a long data frame
df_long <- data.frame(
  id = c(1, 2, 3, 1, 2, 3),
  category = c("A", "A", "A", "B", "B", "B"),
  value = c(10, 20, 30, 40, 50, 60)
)

# use pivot_wider() to convert to a wide data frame
df_wide <- pivot_wider(df_long, id_cols = id, names_from = category, values_from = value)

# print the resulting wide data frame
df_wide

library(tidyr)

# Create example data frame
