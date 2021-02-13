library(dplyr)
data(starwars)

# Problem 1 ####
names(starwars)

table1 = starwars %>%
  group_by(species) %>%
  filter(n() >=3)
print(table1)

# Problem 2 ####

# 2a ####
type = starwars$species
planets = starwars$homeworld

combo = paste(type, planets)

mytable = table(combo)
a.top3 = sort(mytable, decreasing = TRUE)[1:3]
a.top3

# 2b ####
b.top3 = starwars %>%
  group_by(species, homeworld) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n))
b.top3[1:3,]
