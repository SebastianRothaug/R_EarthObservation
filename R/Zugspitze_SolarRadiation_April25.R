# zugspitze Analysis
library(terra)

### Terrain x NDSI
SolarRadiation <- rast("C:/Users/User/Desktop/EO for Cold Regions April 1-3/April 2025 Our Final Data/SAGA_DEM_Direct_Insolation.tif")

NDSI_Change <- rast("C:/Users/User/Desktop/EO for Cold Regions April 1-3/April 2025 Our Final Data/NDSI_Change.tif")

plot(SolarRadiation)

plot(NDSI_Change)


SolarRadiation <- terra::project(SolarRadiation, crs(NDSI_Change))

SolarRadiation <- crop(SolarRadiation, NDSI_Change)
  
#corSolarNDSI<- rasterCorrelation(SolarRadiation, NDSI_Change, s = 3, type = "pearson")



# 1. Resample SolarRadiation to match NDSI_Change
SolarRadiation_resampled <- resample(SolarRadiation, NDSI_Change, method = "bilinear")

# 2. Stack both layers
r_stack <- c(SolarRadiation_resampled, NDSI_Change)

# 3. Convert to data.frame and compute correlation
r_df <- na.omit(as.data.frame(r_stack, xy = FALSE))
correlation <- cor(r_df[[1]], r_df[[2]])
print(correlation)
## Correlation coefficient r


# Apply local correlation using a moving window
cor_map <- focal(r_stack, w = 5, fun = function(x) {
  if (any(is.na(x))) return(NA)
  cor(x[1:(length(x)/2)], x[(length(x)/2 + 1):length(x)])
})

# Plot
plot(cor_map)
## Correlation Map


### Terrain x Snow Depth

SnowDepthFeb<- rast("C:/Users/User/Desktop/EO for Cold Regions April 1-3/February 2025/Clip_Snow_Depth_Old.tif")

plot(SnowDepthFeb)
plot(SolarRadiation)

SnowDepthFeb <- crop(SnowDepthFeb, SolarRadiation)

SolarRadiationXSDChange_resampled <- resample(SolarRadiation, SnowDepthChange, method = "bilinear")

r_stack3 <- c(SolarRadiationXSDChange_resampled, SnowDepthChange)


r_df3 <- na.omit(as.data.frame(r_stack3, xy = FALSE))
correlation3 <- cor(r_df3[[1]], r_df3[[2]])
print(correlation3)

r_squared <- correlation3^2
print(r_squared)

# Apply local correlation using a moving window
cor_map3 <- focal(r_stack3, w = 5, fun = function(x) {
  if (any(is.na(x))) return(NA)
  cor(x[1:(length(x)/2)], x[(length(x)/2 + 1):length(x)])
})

  # Plot
  plot(cor_map3)
  plot(cor_map3[[2]])
  
  ## Correlation Map
  
  str(cor_map3)
  library(terra)
  
  # Assuming cor_map2 is your SpatRaster object
  writeRaster(cor_map3, "C:/Users/User/Desktop/EO for Cold Regions April 1-3/April 2025 Our Final Data/cor_SolarXSnowDChange.tif", filetype = "GTiff", overwrite = TRUE)
  
  
  ### Terrain x Snow Depth Situation
  
  SnowDepthChange<- rast("C:/Users/User/Desktop/EO for Cold Regions April 1-3/April 2025 Our Final Data/SnowDepth_NEW-OLD_DSM.tif")
  
  plot(SnowDepthChange)
  plot(SolarRadiation)
  
  SnowDepthFeb <- crop(SnowDepthFeb, SolarRadiation)
  
  SolarRadiationXSDFeb_resampled <- resample(SolarRadiation, SnowDepthFeb, method = "bilinear")
  
  r_stack2 <- c(SolarRadiationXSDFeb_resampled, SnowDepthFeb)
  
  
  r_df2 <- na.omit(as.data.frame(r_stack2, xy = FALSE))
  correlation2 <- cor(r_df2[[1]], r_df2[[2]])
  print(correlation2)
  
  
  # Apply local correlation using a moving window
  cor_map2 <- focal(r_stack2, w = 5, fun = function(x) {
    if (any(is.na(x))) return(NA)
    cor(x[1:(length(x)/2)], x[(length(x)/2 + 1):length(x)])
  })
  
  # Plot
  plot(cor_map2)
  plot(cor_map2[[1]])
  
  ## Correlation Map
  
  str(cor_map2)
  library(terra)
  
  # Assuming cor_map2 is your SpatRaster object
  writeRaster(cor_map2, "C:/Users/User/Desktop/EO for Cold Regions April 1-3/February 2025/cor_SolarSnowDFeb.tif", filetype = "GTiff", overwrite = TRUE)
  