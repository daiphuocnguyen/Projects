library(tidyverse)
# The mpg data frame
mpg
# Creating a ggplot
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))
# A graphing template
# ggplot(data = <DATA>) +
#  <GEOM_FUNCTION>(mapping = aes(<MAPPING>))
ggplot(data = mpg) # blank graph
nrow(mpg) # number of rows in mpg
ncol(mpg) # number of columns in mpg
?mpg
ggplot(data = mpg) +
  geom_point(mapping = aes(x = cyl, y = hwy))
ggplot(data = mpg) +
  geom_point(mapping = aes(x = drv, y = class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = class))
# Warning: Using size for a discrete variable is not advised.

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = "red"))
             
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, colour = displ < 5))

# Facets
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ cyl)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

# Exercises
ggplot(data = mpg) +
  geom_point(mapping = aes(x = drv, y = cyl))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 2)

# Geometric objects
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, color = drv))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv), show.legend = FALSE)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth(formula = 'y ~ x', method = 'loess')

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_line()

ggplot(data = mpg, aes(x = displ)) + geom_boxplot()

ggplot(data = mpg, aes(x = hwy)) + geom_boxplot()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))

ggplot(data = mpg, aes(x=displ, y = hwy))+
  geom_point()+
  geom_smooth()

ggplot(data = mpg, aes(x=displ, y = hwy))+
  geom_point()+
  geom_smooth(aes(group = drv))

ggplot(data = mpg, aes(x=displ, y = hwy))+
  geom_point()+
  geom_smooth(aes(group = drv, color = drv), formula = 'y ~ x', method = 'loess')

ggplot(data = mpg, aes(x=displ, y = hwy))+
  geom_point(aes(color = drv))+
  geom_smooth()

ggplot(data = mpg, aes(x=displ, y = hwy))+
  geom_point(aes(color=drv))+
  geom_smooth(aes(group=drv))

ggplot(data = mpg, aes(x=displ, y=hwy))+
  geom_point(aes(color=drv))

# 3.7 Statistical Transformations
ggplot(data=diamonds)+
  geom_bar(mapping=aes(x=cut))

ggplot(data = diamonds)+
  stat_count(mapping = aes(x = cut))

demo <- tribble(
  ~cut, ~freq,
  "Fair", 1610,
  "Good", 4906,
  "Very Good", 12082,
  "Premium", 13791,
  "Ideal", 21551
)
ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat="identity")

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = after_stat(prop), group = 1))

ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
)

# Exercises
demo <- tribble(
  ~cut, ~freq,
  "Fair", 1610,
  "Good", 4906,
  "Very Good", 12082,
  "Premium", 13791,
  "Ideal", 21551
)
ggplot(data = demo) +
  geom_col(mapping = aes(x = cut, y = freq))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = after_stat(prop), group = 1))


# 3.8 Position adjustments ####
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, colour = cut))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = cut))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity))

ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) +
  geom_bar(alpha = 1/5, position = "identity")

ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity) ) +
  geom_bar(fill = NA, position = "identity")

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

## 3.8.1 Exercises ####
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point()+
  geom_jitter()

# 3.9 Coordinate systems ####
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot() +
  coord_flip()

nz <- map_data("nz")
ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

bar <- ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = cut),
    show.legend = FALSE,
    width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

## 3.9.1 Exercises ####
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity") +
  coord_polar()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() +
  geom_abline() +
  coord_fixed()
# The layer grammar of graphics ####
# ggplot(data = <DATA>) +
#   <GEOM_FUNCTION>(
#     mapping = aes(<MAPPINGS>),
#     stat = <STAT>,
#     position = <POSITION>
#   ) +
#   <COORDINATE_FUNCTION> +
#   <FACET_FUNCTION>
  
# 4 Workflow: basics ####
## 4.1 Coding basics ####
1 / 200 * 30
(59 + 73 + 2) / 3
sin(pi / 2)
x <- 3 * 4
## 4.2 What's in a name? ####
# i_use_snake_case
# otherPeopleUseCamelCase
# some.people.use.periods
# And_aFew.People_RENOUNCEconvention
x
this_is_a_really_long_name <- 2.5
r_rocks <- 2 ^ 3
r_rock
r_rocks
## 4.3 Calling functions ####
seq(1, 10)
x <- "hello world"
x <- "Hello"
y <- seq(1, 10, length.out = 5)
y
(y <- seq(1, 10, length.out = 5))
## Exercises ####
library(tidyverse)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

filter(mpg, cyl == 8)
filter(diamonds, carat > 3)
