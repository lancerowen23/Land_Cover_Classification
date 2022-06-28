library(raster)
library(rgdal)
library(tigris)
library(sf)
library(sp)
library(tidyverse)
library(rasterVis)
library(rpart)
library(randomForest)
library(rasclass)
library(ExtractTrainData)
library(e1071)
library(caret)
library(ranger)

# load raster layer and assign new name to names attribute
nlcd <- brick("C:/Users/.../Downloads/nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img")
names(nlcd) <- "nlcd2019"

# Create data frame of class names and codes for plotting
nlcdclass <- c("Open Water", 
               "Developed, Open Space", 
               "Developed, Low Intensity", 
               "Developed, Medium Intensity", 
               "Developed, High Intensity", 
               "Barren Land", 
               "Deciduous Forest",
               "Evergreen Forest",
               "Mixed Forest",
               "Shrubs",
               "Grassland/Herbaceous",
               "Pasture",
               "Cultivated Crops",
               "Woody Wetlands",
               "Emergent Herbaceous Wetlands")
classdf <- data.frame(ID = c(11,21,22,23,24,31,41,42,43,52,71,81,82,90,95), 
                      classnames1 = nlcdclass)

#load counties, filter to Fulton County, GA, and reproject to match NLCD raster
counties <- counties(cb = FALSE, resolution = "500k", year = NULL)
fulton <- counties %>% filter(GEOID == "13121")
fulton <- st_transform(fulton, crs = crs(nlcd))

#crop faster to extent of Fulton County
nlcd_fulton <- crop(nlcd, fulton)

# Now we ratify (RAT = "Raster Attribute Table") the clipped raster (define RasterLayer as a categorical variable). This is helpful for plotting.
nlcd_fulton2019 <- nlcd_fulton[[1]]
nlcd_fulton2019 <- ratify(nlcd_fulton2019)
# get levels (land cover values) for the clipped raster
rat <- levels(nlcd_fulton2019)[[1]]
# left join the land cover values and names to the rat data frame and incorporate into raster attributes of California raster
rat <- dplyr::left_join(rat, classdf)
levels(nlcd_fulton2019) <- rat
# Assign hex codes of colors for each land cover class based on NLCD legend color scheme
classcolor <- c("#5475A8", 
                "#E8D1D1",
                "#E29E8C", 
                "#FF0000", 
                "#B50000", 
                "#D2CDC0", 
                "#85C77E",
                "#38814E",
                "#D4E7B0",
                "#DCCA8F",
                "#FDE9AA",
                "#FBF65D",
                "#CA9146",
                "#C8E6F8",
                "#64B3D5")
#plot to verify
levelplot(nlcd_fulton2019, col.regions = classcolor, main = 'ATLANTA | 2019 Land Cover Classes')


#reclassify values to consolidate categories from 15 to 8.

# Create data frame of class names and codes for plotting
nlcdreclass <- c("Open Water", 
                 "Developed", 
                 "Barren", 
                 "Forest",
                 "Shrubland",
                 "Herbaceous",
                 "Pasture/Crops",
                 "Wetlands")

reclass_raster <- c(20,25,24, #Developed
                    40,44,43, #Forest
                    50,53,52, #Shrubland
                    70,75,74, #Herbaceous
                    80,83,82, #Planted/Cultivated
                    90,96,90) #Wetlands

reclass_matrix <- matrix(reclass_raster,
                         ncol = 3,
                         byrow = TRUE)
nlcd_fulton_reclass <- reclassify(nlcd_fulton,
                                  reclass_matrix)
nlcd_fulton2019_reclass <- nlcd_fulton_reclass[[1]]
nlcd_fulton2019_reclass <- ratify(nlcd_fulton2019_reclass)
# get levels (land cover values) for the clipped raster
rat <- levels(nlcd_fulton2019_reclass)[[1]]
reclassdf <- data.frame(ID = rat, 
                        classnames1 = nlcdreclass)
# left join the land cover values and names to the rat data frame and incorporate into raster attributes of California raster
rat <- dplyr::left_join(rat, reclassdf)
levels(nlcd_fulton2019_reclass) <- rat

# Assign hex codes of colors for each land cover class based on NLCD legend color scheme
reclasscolor <- c("#5475A8", 
                  "#BF081B", 
                  "#D2CDC0", 
                  "#85C77E",
                  "#DCCA8F",
                  "#D1D182",
                  "#CA9146",
                  "#C8E6F8")

#plot to verify
levelplot(nlcd_fulton2019_reclass, col.regions = reclasscolor, main = 'ATLANTA | 2019 Land Cover Classes')


#reclassify values to consolidate all but developed categories.
# Create data frame of class names and codes for plotting
nlcdreclass <- c("Open Water or Wetlands", 
                 "Developed, Open Space", 
                 "Developed, Low Intensity", 
                 "Developed, Medium Intensity", 
                 "Developed, High Intensity", 
                 "Barren Land", 
                 "Forest",
                 "Shrubland/Grassland/Pasture/Crops")

reclass_raster <- c(40,44,43, #Forest
                    50,83,71, #Shrubland/Crops/Pasture
                    89,96,11) #Wetlands assigned to Water

reclass_matrix <- matrix(reclass_raster,
                         ncol = 3,
                         byrow = TRUE)
nlcd_fulton_reclass <- reclassify(nlcd_fulton,
                                  reclass_matrix)
nlcd_fulton2019_reclass <- nlcd_fulton_reclass[[1]]
nlcd_fulton2019_reclass <- ratify(nlcd_fulton2019_reclass)
# get levels (land cover values) for the clipped raster
rat <- levels(nlcd_fulton2019_reclass)[[1]]
reclassdf <- data.frame(ID = rat, 
                        classnames1 = nlcdreclass)
# left join the land cover values and names to the rat data frame and incorporate into raster attributes of California raster
rat <- dplyr::left_join(rat, reclassdf)
levels(nlcd_fulton2019_reclass) <- rat

# Assign hex codes of colors for each land cover class based on NLCD legend color scheme
reclasscolor <- c("#5475A8", 
                  "#E8D1D1",
                  "#E29E8C", 
                  "#FF0000", 
                  "#B50000", 
                  "#D2CDC0",
                  "#3AB784",
                  "#FDE9AA")

#plot to verify
levelplot(nlcd_fulton2019_reclass, col.regions = reclasscolor, main = 'ATLANTA | 2019 Land Cover Classes')


atlN_7 <-  raster("C:/Users/psx8/Downloads/LC08_L2SP_019036_20190925_20200826_02_T1_SR_B7.TIF")
atlN_6 <-  raster("C:/Users/psx8/Downloads/LC08_L2SP_019036_20190925_20200826_02_T1_SR_B6.TIF")
atlN_5 <-  raster("C:/Users/psx8/Downloads/LC08_L2SP_019036_20190925_20200826_02_T1_SR_B5.TIF")
atlN_4 <-  raster("C:/Users/psx8/Downloads/LC08_L2SP_019036_20190925_20200826_02_T1_SR_B4.TIF")
atlN_3 <-  raster("C:/Users/psx8/Downloads/LC08_L2SP_019036_20190925_20200826_02_T1_SR_B3.TIF")
atlN_2 <-  raster("C:/Users/psx8/Downloads/LC08_L2SP_019036_20190925_20200826_02_T1_SR_B2.TIF")
atlN_1 <-  raster("C:/Users/psx8/Downloads/LC08_L2SP_019036_20190925_20200826_02_T1_SR_B1.TIF")
atlN <-  stack(atlN_1, atlN_2, atlN_3, atlN_4, atlN_5, atlN_6, atlN_7)

plotRGB(atlN, scale=65535, stretch="lin")

atlS_7 <-  raster("C:/Users/psx8/Downloads/LC08_L2SP_019037_20190925_20200826_02_T1_SR_B7.TIF")
atlS_6 <-  raster("C:/Users/psx8/Downloads/LC08_L2SP_019037_20190925_20200826_02_T1_SR_B6.TIF")
atlS_5 <-  raster("C:/Users/psx8/Downloads/LC08_L2SP_019037_20190925_20200826_02_T1_SR_B5.TIF")
atlS_4 <-  raster("C:/Users/psx8/Downloads/LC08_L2SP_019037_20190925_20200826_02_T1_SR_B4.TIF")
atlS_3 <-  raster("C:/Users/psx8/Downloads/LC08_L2SP_019037_20190925_20200826_02_T1_SR_B3.TIF")
atlS_2 <-  raster("C:/Users/psx8/Downloads/LC08_L2SP_019037_20190925_20200826_02_T1_SR_B2.TIF")
atlS_1 <-  raster("C:/Users/psx8/Downloads/LC08_L2SP_019037_20190925_20200826_02_T1_SR_B1.TIF")
atlS <-  stack(atlS_1, atlS_2, atlS_3, atlS_4, atlS_5, atlS_6, atlS_7)

plotRGB(atlS, scale=65535, stretch="lin")

#combine rasters, reproject, and plot
atl_landsat_mosaic <- mosaic(atlN, atlS, fun = mean)
atl_landsat_proj <- projectRaster(atl_landsat_mosaic, crs = crs(nlcd))
plotRGB(atl_landsat_proj, scale=65535, stretch="lin")

#calculate ndvi
ndvi <- overlay(atl_landsat_proj$layer.5, atl_landsat_proj$layer.4, fun=function(x,y){(x-y)/(x+y)})
# plot to check 
plot(ndvi)

# stack ndvi into raster with other bands
atl_landsat_final <- stack(atl_landsat_proj, ndvi)
names(atl_landsat_final)
# rename added layer
names(atl_landsat_final) <- c(names(atl_landsat_final)[1:7],"NDVI")
plotRGB(atl_landsat_final, scale=65535, stretch="lin")

# use pair function to check relationships between bands
pairs(atl_landsat_final, maxpixels = 5000)


# Sampling to get points with values from NLCD raster
set.seed(99)
samp2019_fulton_reclass <- raster::sampleStratified(nlcd_fulton2019_reclass, size =5000, na.rm = TRUE, sp = TRUE)
table(samp2019_fulton_reclass$nlcd2019)
# convert spatial points to sf dataframe
sample_sf <- st_as_sf(samp2019_fulton_reclass,coords = 1:2)

# Extract the layer values for the locations
samp_values <- raster::extract(atl_landsat_final, sample_sf, df = TRUE, sp = TRUE)
# remove the ID column
#sampvals <- samp_values[, -1]
# merge the class information with extracted values
sample_data <- data.frame(classvalue = samp2019_fulton_reclass$nlcd2019, samp_values)
# set nlcd2019 to factor
sample_data$nlcd2019 <- as.factor(sample_data$nlcd2019)
# drop extra columns so that only 3 bands and nlcd classification are present
#sample_data_final <- sample_data[, 3:10]
sample_data <- drop_na(sample_data)

rf <- train(nlcd2019~layer.1 + layer.2 + layer.3 + layer.4 + layer.5 + layer.6 + layer.7 + NDVI, 
            data = sample_data, 
            method = 'rf',
            trControl = trainControl(method = "boot"),
            mtry = ncol(train)-1,
            verbose=FALSE)

rf_2 <- train(nlcd2019~layer.1 + layer.2 + layer.3 + layer.4 + layer.5 + layer.6 + layer.7 + NDVI, 
                    data = sample_data, 
                    method = 'rf',
                    trControl = trainControl(method = "cv"),
                    verbose=FALSE)

#use to predict rectilinear extent of Fulton county
fulton_landsat_final <- crop(atl_landsat_final, fulton)

predLC <-raster::predict(fulton_landsat_final, rf_2)

plot(predLC, col = reclasscolor, main = 'ATLANTA | 2019 Land Cover Predictions')



