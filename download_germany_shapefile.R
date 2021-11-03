
url <- paste0("https://gisco-services.ec.europa.eu/distribution/v2/",
              "nuts/shp/NUTS_RG_03M_2021_4326_LEVL_1.shp.zip")
tf <- tempfile(fileext=".zip")
download.file(url, tf) # 0.9 MB
unzip(tf, exdir="misc/vign")
base::file.copy(tf, "misc/germany_shapefile.zip")
