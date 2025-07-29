### Similar to the Bronnbach Folder focusing on the Vegetation Indices

setwd("C:/Users/User/Desktop/???")

library(terra)
library(RStoolbox)
#read raster with terra
bronnbach <- rast("DJIM300AltumBronnbachOrthomosaic_clipped.tif")
print(bronnbach)

plotRGB(bronnbach,r= 3, g = 2, b = 1, stretch="lin")

#Manual NDVI
ndvi_man <- (bronnbach[[4]]-bronnbach[[3]])/(bronnbach[[4]]+bronnbach[[3]])
plot(ndvi_man)

#RS Toolbox Vegeatation Indices
VI <- RStoolbox::spectralIndices(bronnbach, red = 3, green = 2, blue = 1, nir = 5)

View(VI)

plot(VI[[8]])

print(VI[[8]])


##Classification
print(ndvi_man)
unsupervised <- k_means(ndvi_man, centers = 3)

plot(unsupervised)



