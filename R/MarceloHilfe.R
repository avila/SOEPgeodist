
library(foreign)
library(readxl)
library(dplyr)
library(readstata13)
library(stringr)

#setwd("H:/School Matching and Distances")

#read data
schooldata <- read.dta13("./data/schooldata_example.dta")
studentsdata <- read.dta13("./data/persons_example.dta")

# define variables
year_start <- 1992
year_end   <- 2016

schooldata$schultyp <- paste(schooldata$art_reduziert,
                             schooldata$traeger, sep = "_")
school_types <- unique(schooldata$schultyp)

# define function
calculate_distance <- function(year, school_type) {

  # year <- 2004
  # school_type <- "gs_oeff"

  # school data
  schooldata_comb <- schooldata %>%
    filter(year == jahr,
           school_type == schultyp) %>%
    mutate(position = row_number())


  schooldata_latlng <- schooldata_comb %>%
    select(lat, lng)


  # students
  studentsdata_comb <- studentsdata %>%
    filter(year == jahr)

  studentdata_latlng <- studentsdata_comb %>%
    select(lat_pers, lng_pers)

  matched_data <- nn2(schooldata_latlng, query = studentdata_latlng, 1) %>%
    # convert to data frame
    as.data.frame() %>%
    # rename variables for readability
    rename(position=nn.idx, distance2school=nn.dists)

  # Spit out the results of the commands
  studentsdata_comb <- cbind(studentsdata_comb, matched_data)

  # Now we merge the information about the schools
  studentsdata_comb <- merge(studentsdata_comb, schooldata_comb,
                             by = "position", all.x. = TRUE)
  return(studentsdata_comb)
}

list_of_data_frames <- list()
for (year in seq(year_start, year_end)) {
  for (school_type in school_types) {
    cat(year, ": ", school_type, "\n")
    data <- calculate_distance(year = year, school_type = school_type)

    name_of_data <- paste("data", year, school_type, sep = "_")
    list_of_data_frames[[name_of_data]] <- data

    #assign(name_of_data, data)
  }
}


View(list_of_data_frames[["data_1992_mit_gym_os_oeff"]])
