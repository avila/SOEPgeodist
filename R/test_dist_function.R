n1 <- 5
n2 <- 10

set.seed(15)
dfst <- data.frame(
  id = letters[1:n1],
  lat = rnorm(n1),
  lng = rnorm(n1)
)

dfsc <- data.frame(
  id = LETTERS[1:n2],
  lat = rnorm(n2),
  lng = rnorm(n2)
)

plot(x=c(x=0), y=c(y=0), ylim = c(-2, 2), xlim = c(-2, 2))

text(lat~lng, labels=id, data=dfst, cex=0.9, font=2, offset=c(5,1), col = "blue")
text(lat~lng, labels=id, data=dfsc, cex=0.9, font=2, offset=c(5,1), col = "green")


sf_stud <- sf::st_as_sf(dfst, coords = c("lng", "lat"))
sf_schl <- sf::st_as_sf(dfsc, coords = c("lng", "lat"))
(near_idx <- sf::st_nearest_feature(sf_stud, sf_schl))

# create a subset with only the nearest schools
sf_subset <- sf_schl[near_idx,]

# calculate distance in meters
(dist <- st_distance(
  sf_stud,
  sf_subset,
  by_element = TRUE)
)

sf_stud %>%
  mutate(
    dist = dist,
    nearest_school = sf_subset$id
  )

