### This script explores the school data. By the end of the script it plots the
### school data points one a map for every year.


## library -----

library(haven)
library(dplyr)
library(sf)
library(ggplot2)
library(tidyr)
library(glue)
library(purrr)


## read data ----
schools <- readxl::read_excel(
  "data/schulen_komplett.xlsx",
  col_types = c(art = "text",
                art_reduziert = "text",
                bundesland = "text",
                jahr = "numeric",
                lat = "numeric",
                lng = "numeric",
                loc_hash = "text",
                name = "text",
                ort = "text",
                plz = "text",
                priv_schule_typ = "text",
                strasse = "text",
                traeger = "text"
  )
) %>%
  mutate(
    art_trunc = stringr::str_trunc(as.character(art), 13, "center"),
    schultyp = paste(art_reduziert, traeger, sep = "_")
    # -> for shorter legend labels
  )


# schools <- haven::read_dta("data/schooldata_example.dta") %>%
#   mutate(
#     ## mutate into factor to be able to plot later.
#     across(where(haven::is.labelled),
#            ~haven::as_factor(.x)
#     ),
#
#     # shorten `art` for concise labels in legend
#     art_trunc = stringr::str_trunc(as.character(art), 13, "center")
#   )


## very basic summary ----

schools %>% select(
  bundesland, art, art_reduziert
) %>% map(~table(.x, schools$jahr))

## calculate number of schools per type (art_reduziert) and bundesland.
## and for easier visualization, reshape into wide.

sch_table_red <- schools %>%
  arrange(jahr, bundesland) %>%
  # -> sort by year (increasing), then bundesland.
  select(jahr, bundesland, art_reduziert) %>%
  group_by(jahr, bundesland, art_reduziert) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = jahr, values_from = n)

sch_table_komp <- schools %>%
  arrange(jahr, bundesland) %>%
  # -> sort by year (increasing), then bundesland.
  select(jahr, bundesland, art) %>%
  group_by(jahr, bundesland, art) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = jahr, values_from = n)

sch_table_typ <- schools %>%
  arrange(jahr, bundesland) %>%
  # -> sort by year (increasing), then bundesland.
  select(jahr, bundesland, schultyp) %>%
  group_by(jahr, bundesland, schultyp) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = jahr, values_from = n)


openxlsx::write.xlsx(
  x = list(
    "art" = sch_table_komp,
    "art_reduziert" = sch_table_red,
    "schultyp" = sch_table_typ
  ),
  file = "output/tables/school_data_tables.xlsx",
  overwrite = TRUE
)


if (interactive()) {
  View(sch_table_red)
}


## plot timeseries ----

schools %>%
  group_by(jahr, bundesland, schultyp) %>%
  summarise(n=n()) %>%
  ggplot(aes(x = jahr, y = n, color = schultyp)) +
  geom_line() +
  geom_point(size=.5) +
  facet_wrap(~bundesland) +
  labs(title = "Schools by type / bundesland / year")
ggsave(filename = glue::glue("output/figs/art_reduziert/time_series_art_reduziert.png"),
       width = 8, height = 6, dpi = 150)


## Schools range year

schools %>%
  group_by(bundesland) %>%
  summarise(
    ymin = min(jahr),
    ymax = max(jahr)
  )


## plot maps ------------

### read shapefile  -----

germany_shp <- st_read("misc/vign/NUTS_RG_03M_2021_4326_LEVL_1.shp") %>%
  filter(CNTR_CODE=="DE")

### some checks
st_crs(germany_shp)
st_geometry_type(germany_shp)
st_bbox(germany_shp)



## plot for all years ----

export_plot <- TRUE
year_min <- range(schools$jahr)[1]
year_max <- range(schools$jahr)[2]
#year_max <- year_min + 1

for (year in seq(year_min, year_max)) {
  type <- "schultyp"

    cat(glue::glue("year: {year}, type: {type}"))

    type_i <- dplyr::sym(type)

    p <- schools %>%
      filter(jahr==year) %>%
      ggplot() +
      geom_point(aes(x=lng, y=lat, col=!!type_i), alpha=.7, size = .5) +
      geom_sf(data = germany_shp, color = "white", fill = NA, size=.5) +
      labs(title = "Schools locations", subtitle = glue::glue("Year: {year}")) +
      theme(axis.title=element_blank())

    print(p)

    if (export_plot) {
      cat(" exporting plot...")
      ggsave(glue::glue("output/figs/{type_i}/map_schools_{year}.png"), height = 9,
             width = 8, dpi = 150)
    }
    cat(" Done!\n")
}
