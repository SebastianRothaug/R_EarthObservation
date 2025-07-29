install.packages("terra")
library(terra)
install.packages("sf")
library(sf)
install.packages("geodata")
library(geodata)
install.packages("rnaturalearthdata")
library(rnaturalearthdata)

ger <- ne_countries(country="Germany", scale="medium",return="sf")
plot(ger)

clim <- geodata::worldclim_global(var = 'tmin', res = 10, download = T, path = 'C:/Users/User/Desktop/R Course Intro to Programming')
plot(clim)

ger.r <- st_transform(ger, st_crs(clim))

clim_ger_crop <- terra::crop(clim, ger.r)
plot(clim_ger_crop)

clim_ger_mask <- terra::mask(clim_ger_crop, ger.r)
plot(clim_ger_mask)

climGer_vect <- terra::extract(clim_ger_mask, ger, mean)
plot(unlist(climGer_vect[,2:13]))


ita <- ne_countries(country="Italy", scale="medium",return="sf")
plot(ita)


elevation <- geodata::elevation_global(res = 10, download = T, path = 'C:/Users/User/Desktop/???')
plot(elevation)

ita.r <- st_transform(ita, st_crs(elevation))

elevation_ita_crop <- terra::crop(elevation, ita.r)
plot(elevation_ita_crop)

elevation_ita_mask <- terra::mask(elevation_ita_crop, ita.r)
plot(elevation_ita_mask)

rus <- ne_countries(country="Russia", scale="medium",return="sf")
plot(rus)

rus.r <- st_transform(rus, st_crs(elevation))

elevation_rus_crop <- terra::crop(elevation, rus.r)
plot(elevation_rus_crop)

elevation_rus_mask <- terra::mask(elevation_rus_crop, rus.r)
plot(elevation_rus_mask)
