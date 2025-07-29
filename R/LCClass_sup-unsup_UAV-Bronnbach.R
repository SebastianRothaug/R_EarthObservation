setwd("C:/Users/User/Desktop/R_Course/Intro_R_EAGLE/Data")

library(terra)
library(sf)
#read raster with terra
raster <- rast("DJIM300AltumBronnbachOrthomosaic_clipped.tif")

print(raster)

#read vector with terra
clip_layer <-  st_read("Bronnbacher_Kloster_Clip.gpkg", layer = "Bronnbacher_Kloster_Clip") # layer = weil Geopackage mehrere Layer beinhalten könnte

#reprojection
raster_repo <- project(raster, "epsg:3035")

#for later imports: st_read("C:/User/... .gpkg", layer = "Training_Data")  %>% st_transform

#clipping = Rechteck Bounding Box des Cliping Layer
clipped_raster <- crop(raster_repo, clip_layer)
#masked = Original Cliping Layer
masked_raster <- mask(clipped_raster, clip_layer)


#plot with rgb
plotRGB(raster_repo,r= 3, g = 2, b = 1, stretch="lin") #stretch = stretching the histogram

plotRGB(clipped_raster,r= 3, g = 2, b = 1, stretch="lin")

plotRGB(masked_raster,r= 3, g = 2, b = 1, stretch="lin")

#Unsupervised classification
# k_means is in terra package
# centers is number of classes to detect
clustering <- k_means(masked_raster, centers = 5)
plot(clustering)


#Supervised classification
#read vector training data with terra
training_area <-  st_read("Training_Data.gpkg") #nur ein file deshalb kein layer = nötig
plot(training_area)
###Random Forest via Toolbox
library(RStoolbox)
supervised <- superClass(masked_raster, training_area)
plot(supervised$map) #$map to print this because output is defined as a list

### Random Forest via Manually

training_area <-  st_read("Training_Data.gpkg")

## Variant 1
#calculates the zonal statistics for the Training area and 
#saves them into the polygon attribute table of the Training Area
#This will be the statistical base for our later starting classification
training_area$mean_val <- zonal(masked_raster,vect(training_area), fun = mean)
View(training_area)

## Variant 2
#creates points and directly intersects and clipping them into the training area
point_grid <- st_make_grid(traning_area,2, what = "centers") %>% st_intersection(training_area)

plot(st_geometry(point_grid), add = T)


## Fill the Point Table with Data
## Pipe Version (Not Working yet)
#1. Teil: extracts the value of the raster to the attribute table of the points
#2. Teil: td_points lost the information of its geometry by the 2 Pipes %>% you make it as spatial dataframe again
#3. Joins this table and the Training_area column because you need the classes names (street, roof)
td_points <- extract(masked_raster,vect(point_grid)) %>% 
  data.frame(point_grid) %>% st_as_sf(td_points) %>% 
  st_join(td_points,training_area)

## Normal Way to Store the Data 
## other traditional way without pipes
td_points <- extract(masked_raster,vect(point_grid)) # extracts the value of the raster to the attribute table of the points
td_points <- data.frame(td_points,point_grid) # transform it into a dataframe
td_points <- st_as_sf(td_points) # make it a spatial data frame
td_points <- st_join(td_points,training_area) # join the drwan traning data cause of the classes name

### Random Forest ###
library(randomForest)
#clean the data
st_geometry(td_points) <- NULL #remove the geometry column cause RF can not work with it
td_points <- td_points[,-1] # remove the id column, already done in my data

#create factor the Classes tells the rf the classes
td_points$Class <- as.factor(td_points$Class)

#rf
rf <- randomForest(Class ~ ., td_points)

prediction <- predict(masked_raster,rf)

plot(prediction)

View(td_points)

#Starting NDVI Analysis
ndvi <-  function(nir, red){
  ndvi <- (nir - red)/(nir + red)
  return(ndvi)
}

ndvi_bron <- ndvi(masked_raster[[4]],masked_raster[[3]])
# [[]] weil die Datenbasis doppelt verpeackt ist. Check View(masked_raster)

plot(ndvi_bron)
plot(st_geometry(training_area), add = T) # Layer der Trainingsdata darüber legen zum gegenchecken

