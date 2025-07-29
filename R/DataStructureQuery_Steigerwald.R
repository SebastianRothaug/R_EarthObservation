setwd("C:/Users/User/Desktop/R_Course/Intro_R_EAGLE/Data")

library(ggplot2)

#week3
forest <- read.csv("Steigerwald_sample_points_all_data_subset_withNames.csv")

head(forest)
summary(forest)
View(forest)

str(forest) # giving structure just as int, num, character

forest[,c('S2.1','S2.2')]
forest[,4:13]  #all sentinel data
forest[1:10,]

#scatterplot
plot(forest[,c("L8.ndvi","SRTM")])

plot(forest$SRTM > 400)  # $ = True / False abfrage

#ggplot
ggplot(forest, aes(x=L8.ndvi, y=L8.savi, colour = SRTM)) + 
  geom_point() + geom_smooth() + facet_wrap(~LCname)

x11()

#week3 slides page 138
ggplot(forest, aes(x=LCname, y=L8.savi))+geom_boxplot(alpha=.5)

ggplot(forest, aes(x=LCname, y=L8.savi))+geom_boxplot(alpha=.5) +
  geom_point(aes(colour = SRTM), alpha=.7, size=1.5, position=position_jitter(width=.25, height=0))


ggplot(forest) + aes(x=LCname, y=SRTM, colour = L8.ndvi) + 
  geom_hex()
                                                                              

ggplot(forest, aes(SRTM,L8.ndvi)) + geom_line(colour="red") + geom_smooth() + 
  ggtitle('Titel', forest$L8.ndvi)

#Homework Training 10.-11.Nov.

ggplot(forest, aes(L8.ndvi,SRTM))+ geom_area(alpha=.6) + geom_point(size=.9,colour="brown",alpha=0.5) +
  geom_smooth(colour="green") + labs(title = "NDVI to Elevation", x = "NDVI", y = "Elevation")

ggplot(forest, aes(L8.ndvi,SRTM))+ geom_area(alpha=.6,fill="grey") + geom_point(size=.9,alpha=0.5, aes(color=LCname)) +
  geom_smooth(colour="green") + ggtitle("NDVI to Elevation")+ xlab("NDVI")+ ylab("Elevation")     #aes allways refers to the data to colour

ggplot(forest) + geom_bar(aes("L8.ndvi")) + facet_grid(. ~ LCname)

ggplot(forest, aes(x=SRTM,y=LCname)) + geom_boxplot() + geom_jitter(aes(alpha=.1,color=SRTM))

#week2
X <-  seq(1,100, by=2.5)
X

X[7]

X[length(X)] #length giving you the entries (the volume) of the list at the same time its the last index
X[length(X)-1] #second to last

X[-1] #everything expect the first position

idx <- c(1,2,6)
X[idx] #giving only these defined positions
X[-idx]


X > 20
(X<= 10) | (X>=20)  # will get as True / False cause of the or


X2 <- numeric(length(X))
X2
X2[X<30] <- 1 # X<30 refering to the values smaller than 30 cause of X in it 
              # otherwise for replacing the first 30 position of the list X[1:30] <- 1
X2

#Matrix
m1 <- matrix(c(4,7,3,8,9,2), nrow = 2)
m1

m1[,2] #2 colm



numbers_1 <- rnorm(80,mean=0,sd=1)
numbers_1

mat_1 <- matrix(numbers_1,nrow=20,ncol=4)
#Transform into a Matrix with 20 rows and 4 columns
mat_1

df_1 <- data.frame(mat_1)
df_1

#Data Frame

df0 <- data.frame(A=c(1,2,3),B=c("aB1","aB2","aB3"))
df0
df0["A"]

df0$B[3] #Abfrage mit Bedingung



#week2 slides 104
df_a <- data.frame(plot="location_name_1", measure1=runif(100)*1000,measure2=round(runif(100)*100), value=rnorm(100,2,1) ,ID=rep(LETTERS,100)[1:100])
df_a

df_b <- data.frame(plot="location_name_2", measure1=runif(50)*100,measure2=round(runif(50)*10), value=rnorm(50) ,ID=rep(LETTERS,50)[1:50])
df_b

#Connect Data Frames
df_combined <- rbind(df_a, df_b) # row bind ; cbind = column combination
df_combined
summary(df_combined)

df_combined[,c('plot','measure1','measure2')] # [, column query c is list what column names to be querried]

df_combined[1:5,c('plot','measure1','measure2')] # [row queryndefinition linr 1 to 5 , column query]

df_combined[,c('plot','measure1','measure2')]


# Query Structure !!!
df_combined['measure2'][df_combined['measure2']>5] # selcting a vector with  Column/List measure2 greater 5
#first [] jumps into the first Column/List, second [] is the condition to select features in that list/column
df_combined['measure2']>5 # just giving back True / False and no selection


v <- data.frame(querry = df_combined['measure2'][df_combined['measure2']>5]) #create a new data.frame with coulmn named querry put in the selecting condition
v


# 03.12.2024
# Convert into Spatial data (read.csv forest  ausführen)

forest.sf <- forest # copy data frame

View(forest.sf)

library(sf)

projcrs <- st_crs(32632) # define projection system

forest.sf <- st_as_sf(x = forest.sf, coords = c("x","y"), crs = projcrs)
# convert normal dataframe into spatial frame (the forest.sf copy will be overwritten as real .sf)

plot(forest.sf) # output is now a map

st_write(forest.sf, "steigerwald_sf.gpkg") # write the spatial data into a Geopackage or .shp and open in QGIS



# spatial vector of Germany

germanyQuery <- germany[germany$Name == "Bavaria", ]
