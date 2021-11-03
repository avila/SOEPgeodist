germany_shp <- st_read("misc/vign/NUTS_RG_03M_2021_4326_LEVL_1.shp") %>%
  filter(CNTR_CODE=="DE")

### some checks
st_crs(germany_shp)
st_geometry_type(germany_shp)
st_bbox(germany_shp)


xx <- de_cities %>% st_join(germany_shp, join = st_within)

schools_minmax <- schools %>%
  group_by(bundesland) %>%
  summarise(
    ymin = min(jahr),
    ymax = max(jahr)
  )


schools_bulaclean <- xx %>%
    mutate(
      bula_clean = case_when(
        NUTS_NAME == "bawue"            ~ "Baden-Württemberg",
        NUTS_NAME == "bayern"           ~ "Bayern",
        NUTS_NAME == "berlin"           ~ "Berlin",
        NUTS_NAME == "brandenburg"      ~ "Brandenburg",
        NUTS_NAME == "bremen"           ~ "Bremen",
        NUTS_NAME == "hamburg"          ~ "Hamburg",
        NUTS_NAME == "hessen"           ~ "Hessen",
        NUTS_NAME == "meckpomm"         ~ "Mecklenburg-Vorpommern",
        NUTS_NAME == "niedersachsen"    ~ "Niedersachsen",
        NUTS_NAME == "nrw"              ~ "Nordrhein-Westfalen",
        NUTS_NAME == "rp"               ~ "Rheinland-Pfalz",
        NUTS_NAME == "saarland"         ~ "Saarland",
        NUTS_NAME == "sachsen"          ~ "Sachsen",
        NUTS_NAME == "sachsenanhalt"    ~ "Sachsen-Anhalt",
        NUTS_NAME == "sh"               ~ "Schleswig-Holstein",
        NUTS_NAME == "thueringen"       ~ "Thüringen"
      )
    )
schools_bulaclean

germany_shp$NUTS_NAME %>% sort
schools$bundesland %>% unique() %>% sort()
