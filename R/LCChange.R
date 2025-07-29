#Land Cover Change Analysis : postclassification approach 

#load require packages 

library(raster)
library(sf) #to convert rgdal 
library(RStoolbox)

#### Data Preparation #### =>  Without 6 Band (Thermal)

####Old: 1988####

#load raster (brick function : "raster" library)
lsat88_test <- brick("C:/Users/User/Desktop/R_ChangeAnalysis_Marine/1988/1988_test.tif")
#plot raster
plot(lsat88_test)
plot(lsat88_test[[2]])#Just one band display 

#load vector 
td_88_test <- st_read("C:/Users/User/Desktop/R_ChangeAnalysis_Marine/1988/training_1988.shp")
#plot vector 
plot(st_geometry(td_88_test),add=TRUE)

#classification (superClass = package RSToolBox)
sc_88_test <- superClass(lsat88_test, trainData = td_88_test,responseCol = "class_name")
sc_88_test #infos about the classification 
plot(sc_88_test$map, main = "SC_88")


####New: 2011####

#load raster (brick function : "raster" library)
lsat11_test <- brick("C:/Users/User/Desktop/R_ChangeAnalysis_Marine/2011/2011_test.tif")
#plot raster
plot(lsat11_test)
plot(lsat11_test[[2]])#Just one band display 

#load vector 
td_11_test <- st_read("C:/Users/User/Desktop/R_ChangeAnalysis_Marine/2011/training_2011.shp")
#plot vector 
plot(st_geometry(td_11_test),add=TRUE)

#classification
sc_11_test <- superClass(lsat11_test, trainData = td_11_test,responseCol = "class_name")
sc_11_test
plot(sc_11_test$map, main = "SC_11")


### Land Cover Change new

# Change Detection 
#copy classified maps
class_1988_copy <- sc_88_test$map
class_2011_copy <- sc_11_test$map

#multiply nominal values by 10
class_1988_copy <- class_1988_copy *10

#perform change detection 
change_map <- class_1988_copy + class_2011_copy

#legend labels
legend_labels <- c(
  "11" = "Forest to Forest (no change)",
  "12" = "Forest to Noforest",
  "13" = "Forest to Water",
  "21" = "Noforest to Forest",
  "22" = "Noforest to Noforest (no change)",
  "23" = "Noforest to Water",
  "31" = "Water to Forest",
  "32" = "Water to Noforest",
  "33" = "Water to Water (no change)"
)

#map custom colors to the legend labels
colors <- c(
  "11" = "white",
  "12" = "red",
  "13" = "pink",
  "21" = "green",
  "22" = "gray",
  "23" = "black",
  "31" = "green",
  "32" = "black",
  "33" = "gray"
)

terra::plot(change_map, type = "classes", col=colors)

#ggplot.component abels= "legend_labels", values="colors"

# Convert SpatRaster to a data frame
change_map_df <- as.data.frame(change_map, xy = TRUE)
colnames(change_map_df) <- c("x", "y", "change_class")
change_map_df$change_class <- as.factor(change_map_df$change_class)

#Plot with ggplot
ggplot(change_map_df, aes(x = x, y = y, fill = change_class)) +
  geom_raster() +
  scale_fill_manual(
    values = colors,
    labels = legend_labels,
    name = "Change Classes"
  ) +
  coord_equal() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    title = "Change Detection Map",
    x = "Longitude",
    y = "Latitude"
  )

