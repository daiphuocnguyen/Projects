library(tidyverse)
# install.packages("palmerpenguins")
library(palmerpenguins)
penguins
glimpse(penguins)
# 2.2 First steps ####
## 2.2.3 Creating a ggplot ####
ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

## 2.2.4 Adding aesthetics and layers ####
ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(mapping = aes(color = species)) +
  geom_smooth(method = "lm")

# install.packages("ggthemes")
library(ggthemes)
ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(mapping = aes(color = species, shape = species), na.rm = TRUE) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()

## 2.2.5 Exercises ####
nrow(penguins)
ncol(penguins)
?penguins
ggplot(data = penguins,
       mapping = aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Bill depth and bill length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Bill length (mm)", y = "Bill depth (mm)",
    color = "Species", shape = "Species") +
  scale_color_colorblind()

ggplot(data = penguins,
       mapping = aes(x = species, y = bill_length_mm)) +
  geom_col()

ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(mapping = aes(color = bill_depth_mm), na.rm = TRUE) +
  geom_smooth()

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point() +
  geom_smooth(se = FALSE)

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
  geom_smooth(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  )

# 2.3 ggplot2 calls ####
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)) + 
    geom_point()

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

penguins |>
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

# 2.4 Visualizing distributions ####
## 2.4.1 A categorial variable ####
ggplot(penguins, aes(x = species)) +
  geom_bar()

ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()

## 2.4.2 A numerical variable ####
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)

ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()

## 2.4.3 Exercises ####
ggplot(penguins, aes(y = fct_infreq(species))) +
  geom_bar()

ggplot(penguins, aes(x = species)) +
  geom_bar(color = "red")

ggplot(penguins, aes(x = species)) +
  geom_bar(fill = "red")

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(bins = 7)

ggplot(diamonds, aes(x = carat)) +
  geom_histogram()

# 2.5 Visualizing relationships ####
## 2.5.1 A numerical and a categorical variable ####
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()

ggplot(penguins, aes(x = body_mass_g, color = species)) +
  geom_density(linewidth = 0.75)

ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 0.5)

## 2.5.2 Two categorical variables ####
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar()

ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")

## 2.5.3 Two numerical variables ####
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

## 2.5.4 Three or more variables ####
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = island))

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)

## 2.5.5 Exercises ####
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = hwy))

ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm, y = bill_depth_mm, 
     color = species
  )
) +
  geom_point() +
  labs(color = "Species")

ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")
ggplot(penguins, aes(x = species, fill = island)) +
  geom_bar(position = "fill")

# 2.6 Saving your plots ####
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
ggsave(filename = "penguin-plot.png")

ggplot(mpg, aes(x = class)) +
  geom_bar()
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point()
ggsave("mpg-plot.png")
