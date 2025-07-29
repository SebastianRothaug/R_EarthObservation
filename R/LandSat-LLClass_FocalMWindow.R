setwd("C:/Users/User/Desktop/")

library(RStoolbox)
library(raster)
library(rgdal)
library(terra)
library(caret)
library(sf)
install.packages("glcm")
library(glcm)

### Load in Landsat Data and Creat displayalbe raster
# Path to folder
landsat_path <- "C:/Users/User/Desktop/Landsat_Cairo/"

# Liste aller Dateien im Ordner (nur .tif-Dateien)
landsat_files <- list.files(landsat_path, pattern = "\\.TIF$", full.names = TRUE)
print(landsat_files)

# Die Bänder in einen RasterStack laden
landsat_stack <- stack(landsat_files)

# Stack anzeigen
print(landsat_stack)
# Plot Cairo RGB Map
plotRGB(landsat_stack, r = 4, g = 3, b = 2, stretch = "lin")



### ndvi

ndvi <- spectralIndices(landsat_stack, red = 4, nir = 5)

plot(ndvi["NDVI"])


### Classification

#Cairo Training Data 
td_cairo <- st_read("td_cairo.gpkg")
#plot Training Area on Map 
plot(st_geometry(td_cairo),add=TRUE)

#classification (superClass = package RSToolBox)
rf_cairo <- superClass(landsat_stack, td_cairo ,responseCol = "class")
 
#plot classification
plot(rf_cairo$map)

#transform coutput list to raster
rf_cairo_raster <- rf_cairo$map
#check class
class(rf_cairo_raster)

### save classifactaion as raster
writeRaster(rf_cairo_raster, filename = "Cairo_output/rf_cairo_raster.tif", overwrite = TRUE)

# load in classifactaion raster again (Save compution time)
rf_cairo_loadin <- rast("Cairo_output/rf_cairo_raster.tif")

plot(rf_cairo_loadin)


### Focal - Moving Window Analysis
# 5er Matrix
r5 <- focal(rf_cairo_loadin, w=matrix(1/25, nrow = 5, ncol = 5))
plot(r5)

# Gauß
fw <- terra::focalMat(rf_cairo_loadin, 2, "Gauss")
r0 <- focal(res_cairo_crop, W=fw)
plot(r0)

### glcm
# funktioniert nur wenn man ein echte Matrix oder Raster Package verwendet
rf_cairo_glcm <- as.matrix(rf_cairo_raster, wide = TRUE)

class(rf_cairo_glcm)

texture_results <- glcm(rf_cairo_glcm , window = c(3,3))

### Werte NAN in = umwandeln




