# load packages -----------------------------------------------------------

library(sf)
library(haven)
library(readxl)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)

# set paths ---------------------------------------------------------------

path_data <- '~/soep-data'
path_schools <- '~/work/2021-06-16'
path_states <- '~/transfer/import/2020-02-27'


if (Sys.getenv("USERNAME")=="avila") {
  path_data <- 'data'
  path_schools <- 'data'
  path_states <- '##'
}

# read data ---------------------------------------------------------------


## read schools data -----------------------------------------------------------------

schools_raw <- readxl::read_excel(
  file.path(path_schools, 'schulen_komplett.xlsx'),
  # manual definition of types to avoid warnings, as
  # priv_shule_typ was falsely being read as logical
  # and not text due to missings.
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
                traeger = "text")
) %>% filter(jahr >= 2000)

schooldata <- st_as_sf(schools_raw, coords = c('lng','lat'), crs = 4326) %>%
  mutate(
    schultyp = paste(art_reduziert, traeger, sep = "_")
  )


school_types <- unique(schooldata$schultyp)
st_crs(schooldata)

## read students data ----------------------------------------------------------------

set.seed(123)
de_cities <- read.csv("data/de.csv") %>%
  select(city, admin_name, capital, lat, lng) %>%
  filter((capital %in% c("admin", "primary")) | runif(n())>.80) %>%
  tidyr::expand_grid(jahr = 2000:2019) %>%
  mutate(pers_ID = 1) %>%
  st_as_sf(coords = c('lng','lat'), crs = 4326)


de_cities
st_crs(de_cities)


## example basic data set

(school_types <- schooldata$schultyp %>% unique())
sch_type <- school_types[1]


calc_dist_to_school_type <- function(school_type, df_ind, df_schools, year) {
  cat(glue::glue("year: {year}; school type: {school_type}"), "\n")

  # get unique schools
  df_schools_unique <- df_schools %>%
    dplyr::filter(
      schultyp == school_type,
      jahr == year
    ) %>%
    dplyr::select(art, art_reduziert, schultyp, loc_hash, geometry) %>% unique()

  # create a new row with a "hash" or unique identifier for the
  # individual data frame as well. Data should be identifiable by `jahr` and `pers_ID`
  df_ind_named <- df_ind %>%
    filter(jahr == year) %>%
    tidyr::unite("ind_hash", city, jahr, pers_ID)


  ## calculate distance
  dist_matrix <- sf::st_distance(df_ind_named, df_schools_unique, by_element = FALSE)


  rownames(dist_matrix) <- df_ind_named$ind_hash
  colnames(dist_matrix) <- df_schools_unique$loc_hash

  # Ok, this basically finds the minimum distance by row and the
  # get the rowname (individual identifier) and colname (school hash)
  # an returns as a data frame.
  res_df <- t(sapply(seq(nrow(dist_matrix)), function(i) {
    j <- which.min(dist_matrix[i,])
    ind <- rownames(dist_matrix)[i]
    sch <- colnames(dist_matrix)[j]
    dist <- dist_matrix[i,j]

    return(c(ind=ind, sch=sch, dist=dist))
  }))  %>% as.data.frame() %>%
    mutate(
      dist = as.numeric(dist),
      jahr = year
    )

  # this is just to get different colnames for each school type so it
  # does not overwrite itselft.
  c_oldnames <- c("sch", "dist")
  cnames <- c(paste(school_type, "hash", sep = "-"),
              paste(school_type, "dist", sep = "-"))
  names(c_oldnames) <- cnames

  ## join results frame with individual frame (and rename)
  df_results <- df_ind_named %>%
    dplyr::left_join(res_df, by = c("ind_hash"="ind")) %>%
    dplyr::rename(!!!c_oldnames) %>% as.data.frame()

  ## Done!
  return(df_results)
}

# Consolidate final data frame ------------------------------------------------------

# nested map ----
years <- seq(2000, 2019, 1)

final_list_of_frames <-
  map(.x = set_names(years),
      .f = function(x)
        map(.x = school_types,
            .f = ~calc_dist_to_school_type(
              school_type = .x,
              df_ind = de_cities,
              df_schools = schooldata,
              year = x
            )
        ) %>%
        # join for a single year all schools types into one data.frame
        purrr::reduce(
          left_join,
          by = c(
            "ind_hash", "admin_name", "capital", "geometry", "jahr"
          )
        )
  )

final_df <- final_list_of_frames %>%
  purrr::reduce(bind_rows)


# pivot longer
final_long <- final_df %>%
  tidyr::pivot_longer(
    cols = matches("-hash|-dist"),
    names_to = c("schultyp", ".value"),
    names_pattern = "(.*)-(.*)"
  ) #%>% mutate(dist = as.numeric(dist))

unique_school_data <- schooldata %>%
  #dplyr::filter(schultyp == school_type) %>%
  dplyr::select(art, art_reduziert, loc_hash, geometry, priv_schule_typ,
                bundesland) %>% unique()

final_joined <- final_long %>%
  left_join(
    y = unique_school_data,
    by = c("hash" = "loc_hash")
  )

final_joined_latlon <- final_joined %>%
  st_as_sf() %>%
  mutate(
    lon_ind = sf::st_coordinates(.)[,1],
    lat_ind = sf::st_coordinates(.)[,2]
  ) %>% st_drop_geometry() %>%
  st_as_sf() %>%
  mutate(
    lon_school = sf::st_coordinates(.)[,1],
    lat_school = sf::st_coordinates(.)[,2]
  ) %>% st_drop_geometry()



# export excel ----------------------------------------------------------------------

openxlsx::write.xlsx(
  x = final_df %>% select(-geometry) %>%
    mutate(across(contains("-dist"), .fns = ~round(.x, digits = 2))),
  file = "output/data/final_distcalc_germancities_wide.xlsx",
  overwrite = TRUE
)
openxlsx::write.xlsx(
  x = final_joined_latlon %>%
    mutate(dist = round(dist, 2)),
  file = "output/data/final_distcalc_germancities_long.xlsx",
  overwrite = TRUE
)


# Check ups --------------------------------------------------------------------------


## plot ------------------------------------------------------------------------------

### read shapefile  -----

germany_shp <- st_read("misc/vign/NUTS_RG_03M_2021_4326_LEVL_1.shp") %>%
  filter(CNTR_CODE=="DE") #%>% filter(NAME_LATN = "Hessen")

### some checks
st_crs(germany_shp)
st_geometry_type(germany_shp)
st_bbox(germany_shp)


## plot for all years ----

export_plot <- TRUE
year_min <- range(schooldata$jahr)[1]
year_max <- range(schooldata$jahr)[2]
#year_max <- year_min + 2

for (
  year in years #seq(year_min, year_max)
) {
  type <- "schultyp"
  cat(glue::glue("year: {year}, type: {type}"))
  type_i <- dplyr::sym(type)
  year <- 2002

  p <- final_joined_latlon %>%
    filter(jahr==year) %>%
    ggplot() +
    geom_point(
      aes(x=lon_school, y=lat_school,
          col = !!type_i),
      alpha=.7, size = .5) +
    geom_point(
      aes(x=lon_ind, y=lat_ind,
          #col=!!type_i
      ),
      alpha=.7,
      size = .5,
      col = "red"
      ) +

    geom_sf(
      data = germany_shp, color = "white", fill = NA, size=.5

    ) +
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


# summary stats ---------------------------------------------------------------------

schooldata %>%
  as.data.frame() %>%
  group_by(bundesland, jahr, schultyp) %>%
  summarise(n = n()) %>%
  tidyr::pivot_wider(names_from = jahr, values_from = n)


schooldata %>%
  as.data.frame() %>%
  group_by(bundesland, jahr, schultyp, .drop = TRUE) %>%
  summarise(n = n()) %>%

  ggplot(
    aes(x = jahr, y = n, col = schultyp)
  ) +
  geom_line() + geom_point() +
  scale_y_log10() +
  facet_wrap(~bundesland) +
  ggtitle("number of schools by federal state")



p <- final_joined_latlon %>% as.data.frame() %>%
  group_by(bundesland, jahr, schultyp) %>%
  summarise(
    dist_mean = mean(dist, na.rm = FALSE),
  ) %>%

  ggplot() +
  geom_line(aes(x = jahr, y = dist_mean, col = schultyp)) +
  #geom_line(aes(x = jahr, y = n, col = "n")) +
  facet_wrap(~bundesland) +
  ggtitle("avg distance of schools to fixed city locations")
p
ggsave(
  filename = "output/figs/art_reduziert/fig_average_dist_by_bula_year.png",
  height = 9, width = 8, dpi = 150
)


final_joined_latlon %>% as.data.frame() %>%
  group_by(bundesland, jahr, schultyp) %>%
  summarise(
    dist_mean = mean(dist, na.rm = FALSE),
  )


final_joined_latlon %>% as.data.frame() %>%
  mutate(dist_KM = dist/1e3,2) %>%
  arrange(jahr) %>%
  group_by(jahr, bundesland) %>%
  summarise(
    mean_dist_KM = mean(dist_KM, na.rm = FALSE) %>% round(2),
  ) %>%
  pivot_wider(names_from = "jahr", values_from = "mean_dist_KM") %>%
  arrange(bundesland) %>% View()
ggs







schooldata %>%
  filter(bundesland=="hessen") %>%
  filter(jahr>=2003 & jahr <= 2006) %>%
  ggplot() +
  geom_sf(aes(col = schultyp, alpha =.3)) +
  facet_wrap(~jahr)



final_joined_latlon %>%
  #filter(admin_name=="Berlin") %>%
  filter(!is.na(admin_name)) %>%
  group_by(admin_name, jahr, schultyp) %>%
  summarise(mean_dist = mean(dist)) %>%
  ggplot(aes(x = jahr, y=mean_dist, col = schultyp)) +
  geom_line() +
  facet_wrap(~admin_name)


