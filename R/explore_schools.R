## library -----

library(haven)
library(dplyr)
library(sf)
library(ggplot2)
library(tidyr)
library(glue)
library(purrr)

## read data ----

schools <- haven::read_dta("data/schooldata_example.dta") %>%
  mutate(
    ## mutate into factor to be able to plot later.
    across(where(haven::is.labelled),
           ~haven::as_factor(.x)
    ),

    # shorten `art` for concise labels in legend
    art_trunc = stringr::str_trunc(as.character(art), 13, "center")
  )

head(schools)

## very basic summary ----

schools %>% select(
  bundesland, art, art_reduziert
) %>% map(~table(.x, schools$jahr))

## calculate number of schools per type (art_reduziert) and bundesland.
## and for easier visualization, reshape into wide.

schools %>% select(jahr, bundesland, art_reduziert) %>%
  group_by(bundesland, jahr, art_reduziert) %>%
  summarise(n = n()) %>%
  tidyr::pivot_wider(names_from = jahr, values_from = n) %>%
  View()

## plot timeseries ----

schools %>%
  group_by(jahr, bundesland, art_reduziert) %>%
  summarise(n=n()) %>%
  ggplot(aes(x = jahr, y = n, color = art_reduziert)) +
  geom_line() +
  geom_point(size=.5) +
  facet_wrap(~bundesland) +
  labs(title = "Schools by type / bundesland / year")
ggsave(filename = glue::glue("output/maps/time_series_art_reduziert.png"),
       width = 8, height = 6, dpi = 150)


## save as above, but for all types (art instead of art_reduziert)
schools %>%
  group_by(jahr, bundesland, art_trunc) %>%
  summarise(n=n()) %>%
  ggplot(aes(x = jahr, y = n, color = art_trunc)) +
  geom_line() +
  geom_point(size=.5) +
  facet_wrap(~bundesland) +
  labs(title = "Schools by type / bundesland / year")
ggsave(filename = glue::glue("output/maps/time_series_art_trunc.png"),
       width = 8, height = 6, dpi = 150)


## plot maps ------------

### read shapefile  -----
germany_shp <- st_read("misc/vign/NUTS_RG_03M_2021_4326_LEVL_1.shp") %>%
  filter(CNTR_CODE=="DE")

st_crs(germany_shp)
st_geometry_type(germany_shp)
st_bbox(germany_shp)

year_min <- range(schools$jahr)[1]
year_max <- range(schools$jahr)[2]
year_max <- year_min + 1

for (year in seq(year_min, year_max)) {
  for (type in c("art_trunc", "art_reduziert")) {

    cat(glue::glue("year: {year}, type: {type}"))

    type_i <- dplyr::sym(type)

    p <- schools %>%
      filter(jahr==year) %>%
      ggplot() +
      geom_point(aes(x=lng, y=lat, col=!!type_i), alpha=.7, size = .5) +
      geom_sf(data = germany_shp, color = "white", fill = NA, size=.5) +
      labs(title = "Schools locations", subtitle = glue::glue("Year: {year}"))

    print(p)

    ggsave(glue::glue("output/maps/m_schools_{year}_{type_i}.png"), height = 9,
           width = 8, dpi = 150)

    cat(" Done!\n")
  }
}
