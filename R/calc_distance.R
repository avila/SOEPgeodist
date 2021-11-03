# load packages -----------------------------------------------------------

library(sf)
library(haven)
library(readxl)
library(dplyr)

rm(list = ls())

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

de_cities <- read.csv("data/de.csv") %>%
  select(city, admin_name, capital, lat, lng) %>%
  filter((capital %in% c("admin", "primary")) | runif(n())>.66) %>%
  tidyr::expand_grid(jahr = 2000:2015) %>%
  mutate(pers_ID = 1) %>%
  st_as_sf(coords = c('lng','lat'), crs = 4326)


de_cities
st_crs(de_cities)


## define function
calculate_distance <- function(students_df, schools_df, year, school_type) {

  # school data
  schools_df_comb <- schools_df %>%
    filter(jahr==year,
           school_type==schultyp)

  # students
  students_df_comb <- students_df %>%
    filter(jahr==year) %>% select(-jahr)

  # st_nearest_points assumes that they are planar
  nearest_school_idx <- st_nearest_feature(students_df_comb,
                                           schools_df_comb)

  # create a subset with only the nearest schools
  schools_df_subset <- schools_df_comb[nearest_school_idx,]

  # calculate distance in meters
  dist <- st_distance(students_df_comb,
                      schools_df_subset,
                      by_element = TRUE)


  resulting_df <- students_df_comb %>%
    bind_cols(st_drop_geometry(schools_df_subset), .name_repair = "minimal") %>%
    mutate(dist = dist)


  return(data.table::as.data.table(resulting_df))
}


if (interactive()) {
  if (FALSE) debugonce(calculate_distance)
  x <- calculate_distance(students_df = de_cities,
                          schools_df = schooldata,
                          year = 2010,
                          school_type = "gs_oeff")

  mean(x$dist)
  View(x)
  rm(x)
}

# define variables
year_start <- 2000
year_end   <- 2019


list_of_raw_data_frames <- list()
list_of_yearly_data_frames <- list()
for (year in seq(year_start, year_end)) {
  for (school_type in school_types) {
    cat(year, ": ", school_type, "\n")

    data <- calculate_distance(
      students_df = de_cities,
      schools_df = schooldata,
      year = year, school_type = school_type
    )

    # delete all variables but student ID, year, distance and private school type
    data <- data %>% dplyr::select(pers_ID, jahr,
                                   dist, bundesland, priv_schule_typ)

    # rename the distance variable and the variable concerning the private school type
    names(data)[2] <- "jahr"
    names(data)[3] <- paste("distance", school_type, sep = "_")
    names(data)[4] <- paste("bundesland", school_type, sep = "_")
    names(data)[5] <- paste("privtyp", school_type, sep = "_")

    name_of_data <- paste("data", year, school_type, sep = "_")
    list_of_raw_data_frames[[name_of_data]] <- data
  }

  # second loop that merges all the data sets

  # 1. step: Take the list that contains info about public elementary and store them in yearly_data

  year_data_name <- paste("data", year, "gs_oeff", sep = "_")
  yearly_data <- list_of_raw_data_frames[[year_data_name]]

  # 2. step: merge this yearly data with a data set that is called merge_data
  for (school_type in school_types[c(2:6)]) {

    cat(".")
    merge_data_name <- paste("data", year, school_type, sep = "_")
    merge_data <- list_of_raw_data_frames[[merge_data_name]]

    yearly_data <- merge(x = as.data.frame(yearly_data),
                         y = as.data.frame(merge_data),
                         by = c("pers_ID", "jahr"), all.x. = TRUE
    )
  }

  name_of_data <- paste("data", year, sep = "_")
  list_of_yearly_data_frames[[name_of_data]] <- yearly_data
}

# 3. step: append on all these yearly data sets to an export data set
export_data <- NULL
for (year in seq(year_start, year_end)) {
  cat(".")
  append_data_name <- paste("data", year, sep = "_")
  export_data <- rbind(export_data, list_of_yearly_data_frames[[append_data_name]])
}

# Save data set export_data

(fname <- paste0("geomatched_schols_", format(Sys.time(), "%Y_%m_%d"), ".csv"))


write.csv(export_data, file = file.path("data/output/", fname))
