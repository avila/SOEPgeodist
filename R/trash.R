
schooldata_unique <- schooldata %>%
  filter(schultyp == school_types[1]) %>%
  select(art, art_reduziert, loc_hash, geometry) %>% unique()

de_cities_named <- de_cities %>%
  tidyr::unite("names", city, jahr, pers_ID)


xxx <- st_distance(de_cities_named, schooldata_unique, by_element = FALSE)

rownames(xxx) <- de_cities_named$names
colnames(xxx) <- schooldata_unique$loc_hash

dim(xxx)

result <- t(sapply(seq(nrow(xxx)), function(i) {
  j <- which.min(xxx[i,])
  c(paste(rownames(xxx)[i], colnames(xxx)[j], sep='_'), xxx[i,j])
}))
result


set.seed(2021)
n1 <- 3
(df1 <- data.frame(
  id1 = paste0("x", 1:n1),
  lat = sample(1:10, n1),
  lon = sample(1:10, n1))
)

sf1 <- df1 %>%
  st_as_sf(coords = c('lon','lat'), crs = 4326)

n2 <- 5
(df2 <- data.frame(
  id2 = paste0("y", 1:n2),
  lat = sample(1:10, n2),
  lon = sample(1:10, n2))
)

sf2 <- df2 %>%
  st_as_sf(coords = c('lon','lat'), crs = 4326)

dists <- st_distance(sf1, sf2, by_element = FALSE)
dists_by_elem <-st_distance(sf1, sf2, by_element = TRUE)

dists

dim(dists)
rownames(dists) <- df1$id1
colnames(dists) <- df2$id2
dists
df1; df2


match <- t(sapply(seq(nrow(dists)), function(i) {
  j <- which.min(dists[i,])
  c(paste(rownames(dists)[i], colnames(dists)[j], sep='_'), dists[i,j])
}))
match
df1; df2

geodist::geodist(x = df1, y = df2, measure = "haversine")
"geosphere" %>% install.packages()
