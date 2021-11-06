germany_shp <- st_read("misc/vign/NUTS_RG_03M_2021_4326_LEVL_1.shp") %>%
  filter(CNTR_CODE=="DE")

### some checks
st_crs(germany_shp)
st_geometry_type(germany_shp)
st_bbox(germany_shp)


schools_minmax <- schools_raw %>%
  group_by(bundesland) %>%
  summarise(
    ymin = min(jahr),
    ymax = max(jahr)
  )

schools_unique_years <- schools_raw %>%
  select(bundesland, jahr) %>%
  unique() %>%
  tidylog::mutate(
    NUTS_NAME = case_when(
      bundesland == "bawue"            ~ "Baden-Württemberg",
      bundesland == "bayern"           ~ "Bayern",
      bundesland == "berlin"           ~ "Berlin",
      bundesland == "brandenburg"      ~ "Brandenburg",
      bundesland == "bremen"           ~ "Bremen",
      bundesland == "hamburg"          ~ "Hamburg",
      bundesland == "hessen"           ~ "Hessen",
      bundesland == "meckpomm"         ~ "Mecklenburg-Vorpommern",
      bundesland == "niedersachsen"    ~ "Niedersachsen",
      bundesland == "nrw"              ~ "Nordrhein-Westfalen",
      bundesland == "rp"               ~ "Rheinland-Pfalz",
      bundesland == "saarland"         ~ "Saarland",
      bundesland == "sachsen"          ~ "Sachsen",
      bundesland == "sachsenanhalt"    ~ "Sachsen-Anhalt",
      bundesland == "sh"               ~ "Schleswig-Holstein",
      bundesland == "thueringen"       ~ "Thüringen"
    )
  )

germany_shp$NUTS_NAME %>% sort
schools_raw$bundesland %>% unique() %>% sort()

de_cities_only_valid_years <- de_cities %>%
  st_join(germany_shp, join = st_within) %>%
  tidylog::inner_join(schools_unique_years)

if (interactive()) {
  View(de_cities_only_valid_years)
  View(de_cities)
}


de_cities_only_valid_years


de_cities_minmax <- de_cities_only_valid_years %>% sf::st_drop_geometry() %>%
  select(bundesland, jahr) %>%
  group_by(bundesland) %>%
  summarise(
    ymin = min(jahr),
    ymax = max(jahr)
  )

all_equal(de_cities_minmax, schools_minmax)


