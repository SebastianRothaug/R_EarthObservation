### Working Directory

setwd("C:/Users/User/Desktop/???/")

library(terra)
library(raster)
library(sf)
#import raster preprocessed with QGIS
rast_new <- rast("China_S2_20251018.tif")
rast_old <- rast("China_S2_20151122.tif")


View(rast_new)

crs(rast_new)
crs(rast_old)

plotRGB(rast_old.r, r= 1, g = 2, b = 3, stretch="lin")
plotRGB(rast_new,r= 1, g = 2, b = 3, stretch="lin")


td_old <-  st_read("td_china_2015.gpkg")
td_new <-  st_read("td_china_2025.gpkg")


crs(td_old)
crs(td_new)

# check print td on plot
plot(st_geometry(td_new), add = T)

#supervised via toolbox
library(RStoolbox)
## new
rf_new <- superClass(rast_new, td_new)
plot(rf_new$map) #$map to print this because output is defined as a list
rf_new_raster <- rf_new$map

writeRaster(rf_new_raster, filename = "rf_new.tif", overwrite = TRUE)

new_rf <- rast("rf_new.tif")

plot(new_rf)

## old
rf_old <- superClass(rast_old, td_old)
plot(rf_old$map) #$map to print this because output is defined as a list
rf_old_raster <- rf_old$map

writeRaster(rf_new_raster, filename = "rf_new.tif", overwrite = TRUE)

new_rf <- rast("rf_new.tif")

plot(new_rf)



#unsupervised ohne td nicht brauchbar da 
#1. Ergebnis zu unterschiedlich
#2. Klassen Name/Nummerwillkürlich vergeben
unsup_old <- k_means(rast_old, centers = 5)
plot(unsup_old)
unsup_new <- k_means(rast_new, centers = 5)
plot(unsup_new)

