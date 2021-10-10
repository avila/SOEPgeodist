# load packages -----------------------------------------------------------

library(sf)
library(haven)
library(RANN)
library(dplyr)
library(data.table)
library(readxl)
library(readstata13)
library(geosphere)
library(foreign)
library(gtools)
library(dplyr)


rm(list = ls())


# set paths ---------------------------------------------------------------

path_data <- '~/soep-data'
path_schools <- '~/work/2021-06-16'
path_states <- '~/transfer/import/2020-02-27'


# read data ---------------------------------------------------------------

Schulen <- readxl::read_excel(file.path(path_schools, 'schulen_komplett.xlsx'), 
                              # manual definition of typesto avoid warnings, as
                              # priv_shule_typ was falsely bein read as logical
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
                                            traeger = "text"))

Schools <- st_as_sf(Schulen, coords = c('lng','lat'))

st_layers(file.path(path_data, 'soep36'))
studentsdata <- read_sf(file.path(path_data, 'soep36'),
                        layer = 'soep_hh_korr_mit_fakes_utm32-v36')


# wrangle with coordinates ------------------------------------------------

st_crs(studentsdata)
st_crs(Schools) <- 4326 #EPSG:4326 WGS 84 -- WGS84 - World Geodetic System 1984, used in GPS 
schooldata <- st_transform(Schools, st_crs(studentsdata))
st_crs(schooldata)

## modify school data ----

schooldata$ostwert <- st_coordinates(schooldata)[,1]
schooldata$nordwert <- st_coordinates(schooldata)[,2]

schooldata <- schooldata %>% filter(jahr > 1999)

schooldata$traeger <- as.character(schooldata$traeger)
schooldata$art_reduziert <- as.character(schooldata$art_reduziert)
schooldata$priv_schule_typ <- as.character(schooldata$priv_schule_typ)
schooldata$bundesland <- as.character(schooldata$bundesland)


schooldata$traeger[schooldata$traeger == "0"] <- "oeff"
schooldata$traeger[schooldata$traeger == "1"] <- "priv"

schooldata$art_reduziert[schooldata$art_reduziert == "1"] <- "gs"
schooldata$art_reduziert[schooldata$art_reduziert == "2"] <- "mit_gym_os"
schooldata$art_reduziert[schooldata$art_reduziert == "3"] <- "ohne_gym_os"

schooldata$priv_schule_typ[schooldata$priv_schule_typ == "1"] <- "Frei"
schooldata$priv_schule_typ[schooldata$priv_schule_typ == "2"] <- "Kirchlich"
schooldata$priv_schule_typ[schooldata$priv_schule_typ == "3"] <- "Montessori"
schooldata$priv_schule_typ[schooldata$priv_schule_typ == "4"] <- "Waldorf"

schooldata$bundesland[schooldata$bundesland == "1"] <- "bawue"
schooldata$bundesland[schooldata$bundesland == "2"] <- "bayern"
schooldata$bundesland[schooldata$bundesland == "3"] <- "berlin"
schooldata$bundesland[schooldata$bundesland == "4"] <- "brandenburg"
schooldata$bundesland[schooldata$bundesland == "5"] <- "bremen"
schooldata$bundesland[schooldata$bundesland == "6"] <- "hamburg"
schooldata$bundesland[schooldata$bundesland == "7"] <- "hessen"
schooldata$bundesland[schooldata$bundesland == "8"] <- "meckpomm"
schooldata$bundesland[schooldata$bundesland == "9"] <- "niedersachsen"
schooldata$bundesland[schooldata$bundesland == "10"] <- "nrw"
schooldata$bundesland[schooldata$bundesland == "11"] <- "rp"
schooldata$bundesland[schooldata$bundesland == "12"] <- "saarland"
schooldata$bundesland[schooldata$bundesland == "13"] <- "sachsen"
schooldata$bundesland[schooldata$bundesland == "14"] <- "sachsenanhalt"
schooldata$bundesland[schooldata$bundesland == "15"] <- "sh"
schooldata$bundesland[schooldata$bundesland == "16"] <- "thueringen"

schooldata$bundesland %>% unique()
## modify student data -----

studentsdata$ostwert <- st_coordinates(studentsdata)[,1]
studentsdata$nordwert <- st_coordinates(studentsdata)[,2]

names(studentsdata)[names(studentsdata)=="ID"] <- "pers_ID"
names(studentsdata)[names(studentsdata)=="erhebj"] <- "jahr"
#names(studentsdata)[names(studentsdata)=="geometry"] <- "stud_geometry"

#studentsdata <- studentsdata %>% filter(jahr < 2017)

schooldata$schultyp <- paste(schooldata$art_reduziert,
                             schooldata$traeger, sep = "_")
school_types <- unique(schooldata$schultyp)


# save intermediate data --------------------------------------------------

(fname_Rdata <- paste0("data/intermediate/inter_", format(Sys.time(), "%Y_%m_%d"), ".Rdata"))
save(Schools, Schulen, studentsdata, schooldata, 
     file = fname_Rdata)


# Calculations  -----------------------------------------------------------
if (FALSE) {
  # make sure packages are also laoded
  load(fname_Rdata)
}

if (FALSE) {
  # test args
  year <- 2010
  school_type <- "gs_oeff"  
}

## define function
calculate_distance <- function(year, school_type)  {
  
  # school data 
  schooldata_comb <- schooldata %>% 
    filter(year == jahr, 
           school_type== schultyp) 
  
  # students
  studentsdata_comb <- studentsdata %>% 
    filter(year == jahr)
  
  
  nearest_school <- st_nearest_feature(studentsdata_comb,
                                       schooldata_comb)
  
  dist <- st_distance(studentsdata_comb,
                      schooldata_comb[nearest_school,],
                      by_element = TRUE)
  
  studentsdata_comb <- cbind(studentsdata_comb,
                             st_drop_geometry(schooldata_comb)[nearest_school,])
  
  studentsdata_comb$dist <- dist
  # convert to data table
  studentsdata_comb <- as.data.table(studentsdata_comb)
  
  return(studentsdata_comb)
}

if (interactive()) {
  x <- calculate_distance(2010, "gs_oeff") %>% head(300)
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
    
    data <- calculate_distance(year = year, school_type = school_type)
    
    # delete all variables but student ID, year, distance and private school type
    data <- data %>% dplyr::select(pers_ID, jahr, 
                                   dist, bundesland, priv_schule_typ)
    
    # rename the distance variable and the varibale concerning the private school type
    names(data)[2] <- "jahr"
    names(data)[3] <- paste("distance", school_type, sep = "_")
    names(data)[4] <- paste("bundesland", school_type, sep = "_")
    names(data)[5] <- paste("privtyp", school_type, sep = "_")
    
    name_of_data <- paste("data", year, school_type, sep = "_")
    list_of_raw_data_frames[[name_of_data]] <- data 
  }
  
  #assign(name_of_data, data)
  
  
  # second loop that merges all the datasets
  
  # 1. step: Take the list that contains info about public elementary and store them in yearly_data
  
  year_data_name <- paste("data", year, "gs_oeff", sep = "_")
  yearly_data <- list_of_raw_data_frames[[year_data_name]]
  
  
  # 2. step: merge this yearly data with a dataset that is called merge_data
  for (school_type in school_types[c(2:6)]) {
    merge_data_name <- paste("data", year, school_type, sep = "_")
    merge_data <- list_of_raw_data_frames[[merge_data_name]]
    
    yearly_data <-     merge(yearly_data, merge_data,
                             by = c("pers_ID","jahr"), all.x. = TRUE)
  }
  
  name_of_data <- paste("data", year, sep = "_")
  list_of_yearly_data_frames[[name_of_data]] <- yearly_data 
}

# 3. step: append on all these yearly datasets to an export dataset
export_data <- NULL
for (year in seq(year_start, year_end)) {
  append_data_name <- paste("data", year, sep = "_") 
  export_data <- rbind(export_data, list_of_yearly_data_frames[[append_data_name]])
}

# Save data set export_data

(fname <- paste0("geomatched_schols_", format(Sys.time(), "%Y_%m_%d"), ".csv"))


write.csv(export_data, file = file.path("data/output/", fname))
