

### Step 1: search and replace all the species names (for example "Leptopelis_nordequatorialis"). You might replace with any of the other species in the excel list I sent you. Always use the Genus and species name seperated by "_". 
### There are two places in this script were you need to manually change the species name because it is NOT seperated by an underscore. These are at line 17 and line 32. 
### You will need to make folders for each species you make a model for. In this folder you need to add the file "cropAlt.aci". You will also need to download the distribition shape (.shp) file from IUCN redlist. 


library(raster); #For raster-based loading, calculations, and mapping
library(rgdal); #For reading the M polygon
library(caret)
library(rgbif);
library(spocc); #For getting georefernces
library(raster); #For loading and manipulating rasters
library(scrubr); #Package introduced for data cleaning
library(spatstat); #Spatial statistics package with method for calculating nearest neighbor distance
library(maptools)
library(rgdal)
library(rgeos)
library(sp)


setwd("/Research/AFROBAT_ENM/AFROBATRACHIA/Leptopelis_nordequatorialis/")
getwd()

#SECTION 1
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#MINE THE OCCURRENCE DATA. 

setwd("/Research/AFROBAT_ENM/AFROBATRACHIA/Leptopelis_nordequatorialis/")
getwd()

#RGBIF
#This package works specifically for querying the GBIF database.
rGBIFLeptopelis_nordequatorialis <- occ_search(scientificName = "Leptopelis nordequatorialis", hasCoordinate = T);
#Searches all GBIF records for squirrels, only returns occurrences with coordinates
head(rGBIFLeptopelis_nordequatorialis$data);
#Shows you all 101 columns in the GBIF record table. That is probably more than you want, so we'll simplify this to the bare essentials.
tablerGBIFLeptopelis_nordequatorialis <- cbind(rGBIFLeptopelis_nordequatorialis$data$name, rGBIFLeptopelis_nordequatorialis$data$decimalLongitude, rGBIFLeptopelis_nordequatorialis$data$decimalLatitude, rGBIFLeptopelis_nordequatorialis$data$institutionCode); 
dim(tablerGBIFLeptopelis_nordequatorialis)
#This puts species name, longitude, latitude, and source institution into a table. There are TONS (101) of options for data fields, including dates, if you want to try making time structured models for your project.
tablerGBIFLeptopelis_nordequatorialisClean <- tablerGBIFLeptopelis_nordequatorialis[complete.cases(tablerGBIFLeptopelis_nordequatorialis),];
dim(tablerGBIFLeptopelis_nordequatorialisClean)
#Removes incomplete records
colnames(tablerGBIFLeptopelis_nordequatorialisClean) <- c("Species", "Latitude", "Longitude", "Institution");
#Names columns in table
write.csv(tablerGBIFLeptopelis_nordequatorialisClean, "Leptopelis_nordequatorialisRGBIF.csv", row.names = F);
#Writes the resulting table to your working directory.

#SPOCC
#This package queries ten different biodiversity databases.
spoccLeptopelis_nordequatorialis <- occ(query = "Leptopelis nordequatorialis", from = c('gbif', 'ecoengine', 'vertnet', 'idigbio'), has_coords = T);
#Searches all appropriate databases for records of frogs, only returns occurrences with coordinates. Note: if you have marine species, ants, birds, etc, there are other databases available through spocc. Check the tutorial for a complete list.
head(spoccLeptopelis_nordequatorialis);
#you will note the structure of the object for spocc is much different than that of rgbif, with separate slots for each of the databases that were queried, and not all the databases may return results.
spoccLeptopelis_nordequatorialisDF <- occ2df(spoccLeptopelis_nordequatorialis);
dim(spoccLeptopelis_nordequatorialisDF)
#This function merges results from all of the databases you queries in a simplified format.
head(spoccLeptopelis_nordequatorialisDF)
#Species name, longitude, latitude, and source database are now in the same data table. Note that the institution is not included. That is, for example, GBIF is an aggregator of other data sources.
spoccLeptopelis_nordequatorialisDFClean <- spoccLeptopelis_nordequatorialisDF[complete.cases(spoccLeptopelis_nordequatorialisDF),];
#Removes incomplete records
colnames(spoccLeptopelis_nordequatorialisDFClean) <- c("Species", "Longitude", "Latitude", "Database");
#Names columns in table
write.csv(spoccLeptopelis_nordequatorialisDFClean, "Leptopelis_nordequatorialisSPOCC.csv", row.names = F);
#Writes the resulting table to your working directory.

#Merging data into a single file.
allLeptopelis_nordequatorialis <- rbind(as.matrix(tablerGBIFLeptopelis_nordequatorialisClean[,1:3]), as.matrix(spoccLeptopelis_nordequatorialisDFClean[,1:3]))
#Puts results of rGBIF and spocc into a single table
colnames(allLeptopelis_nordequatorialis) <- c("Species", "Longitude", "Latitude");
allLeptopelis_nordequatorialis
write.csv(allLeptopelis_nordequatorialis, "All_Leptopelis_nordequatorialis.csv", row.names = F);
#Writes the resulting table to your working directory. Note that there will likely be duplicates between rGBIF and spocc. We will work on cleaning out duplicate occurrences in a future lab.
dim(allLeptopelis_nordequatorialis)

#SECTION 2
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#CLEAN THE OCCURRENCE DATA. 

#Get your data from a .csv
rawLeptopelis_nordequatorialis <- read.csv("Leptopelis_nordequatorialisRGBIF.csv");
dfLeptopelis_nordequatorialis <- dframe(rawLeptopelis_nordequatorialis);

#Step 1: Using scrubr
#scrubbedLeptopelis_nordequatorialis <- dfLeptopelis_nordequatorialis[,-6]; #Removes unique, database-specific keys
scrubbedLeptopelis_nordequatorialis <- coord_incomplete(dfLeptopelis_nordequatorialis); #Removes data with incomplete coordinates
scrubbedLeptopelis_nordequatorialis <- coord_unlikely(scrubbedLeptopelis_nordequatorialis); #Removes data with unlikely coordinates (i.e. 0,0)
paste("Step 1: Post scrubr: ", nrow(scrubbedLeptopelis_nordequatorialis), " points left.", sep = "");
write.csv(scrubbedLeptopelis_nordequatorialis, "Scrubbed_Leptopelis_nordequatorialis.csv", row.names = F);

#Step 2: Removing duplicate dates and localities
uniqueLeptopelis_nordequatorialis <- unique(scrubbedLeptopelis_nordequatorialis); #Removes duplicate points (not considering state)
paste("Step 2: Removing duplicate dates and localities: ", nrow(uniqueLeptopelis_nordequatorialis), " points left.", sep = "");
colnames(uniqueLeptopelis_nordequatorialis) <- c("Species", "Longitude", "Latitude");
write.csv(uniqueLeptopelis_nordequatorialis, "Unique_Leptopelis_nordequatorialis.csv", row.names = F);
UniquePoints <- "Unique_Leptopelis_nordequatorialis.csv"
dim(allLeptopelis_nordequatorialis)
dim(uniqueLeptopelis_nordequatorialis)


##***********************CHECK THAT YOU HAVE ADDED ANY NON-DATABASED DATA NOW (EXAMPLE GJ FIELD 2018)***********************##


#SECTION 3A
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#CREATE M (WITH BUFFER). THAT IS THE LAYER WE WILL EXPLORE POTENTIAL DISTRIBUTION ACROSS

#Load the libraries you'll need.


#RETRIEVE CROPPED RASTER MAP, ALREADY IN YOUR WORKING DIRECTORY. 
CropAfrica <- raster(x = "cropAlt.asc")
crs(CropAfrica) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
plot(CropAfrica)

#Read points
Leptopelis_nordequatorialis <- read.csv(file = "Unique_Leptopelis_nordequatorialis.csv", header = T);
head(Leptopelis_nordequatorialis); #Check out what the datapoints look like
points(Leptopelis_nordequatorialis[,2:3], pch = 16, cex = 1.5)

#Reading shapefiles
LcalShape1 <- readOGR("data_0.shp"); #Automatically reads .prj file
LcalShape1; #Check out what the metadata for the file look like
plot(LcalShape1)

#Reprojecting shapefile
training2 <- spTransform(LcalShape1, CRSobj = crs(CropAfrica));

#Plotting shapefiles
plot(CropAfrica)
plot(training2, add = TRUE); #Layers shapefile on top of raster.
points(Leptopelis_nordequatorialis[,2:3], pch = 16, cex = 1, add = TRUE); #Layers points on top.

############### If no buffer jump to line 151 ###################
#ADD BUFFER TO SHAPEFILE (> GREATER THAN 1 M ASL). 
############### If no buffer jump to line 151 ###################
trainingBuff <- readOGR("data_0.shp");
crs(trainingBuff) <-CRS("+proj=utm +zone=10 +datum=WGS84")
b.r <- buffer(trainingBuff, width = 1, dissolve = T)
crs(b.r) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#gb.r <- gBuffer(trainingBuff, width = 0.3, byid = T)
plot(b.r, add = TRUE)
b.r
#TRYING TO EXPORT AS SHAPE! 

###############  NO BUFFER OPTIONS  ##############
#b.r <- (LcalShape1)
#plot(b.r)


#CROP IUCN RANGE TO STUDY AREA
CropFrog <- crop(b.r, CropAfrica)
plot(CropFrog)


#BRING IT ALL TOGETHER NOW
plot(CropAfrica)
plot(LcalShape1, add = TRUE)
plot(CropFrog, add = TRUE)
points(Leptopelis_nordequatorialis[,2:3], pch = 16, cex = 1.5)
dim(Leptopelis_nordequatorialis)


############### If no buffer jump to line 185 ###################
#CROP POINTS OUTSIDE OF BUFFERED AREA
############### If no buffer jump to line 185 ###################

LGFpoints <- extract(CropFrog, Leptopelis_nordequatorialis[2:3], pch = 16, cex = 1.5); #Extract raster values to locality points.
points(Leptopelis_nordequatorialis[,2:3], pch = 16, cex = 1.2)
IUCNunique <- cbind(Leptopelis_nordequatorialis, LGFpoints); #Merges extracts with locality points.
head(IUCNunique); #See? Now there's a new column with the altitude at each locality point.
LGFonly <- IUCNunique[complete.cases(IUCNunique),]; #Removes points with no altitude data (i.e. outside raster).
dim(Leptopelis_nordequatorialis)
dim(LGFonly)
plot(CropAfrica)
points(LGFonly[,2:3], pch = 16, cex = 1.5)
write.csv(LGFonly, file = "zLGF.csv", row.names = F);

###############  NO BUFFER OPTIONS  ##############
#LGFonly <- read.csv("Unique_Leptopelis_nordequatorialis.csv")
#points(LGFonly[,2:3], pch = 16, cex = 3)
#write.csv(LGFonly, file = "zLGF.csv", row.names = F);

#Making a map
plot(CropAfrica); #Plot raster
plot(CropFrog, add = TRUE); #Layer shapefile on top of raster.
points(LGFonly[,2:3], pch = 16, cex = 1); #Layer points onto map.

#check scale. 
#GABON <- readOGR("GAB_outline.shp"); #Automatically reads .prj file
#plot(GABON, add = TRUE)
#getwd()

#SECTION 4
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#TRAINNING OUR BIOCLIM LAYERS

setwd("/Research/AFROBAT_ENM/BIOCLIM/uncorrelated/reduced")
list.files(path = ".", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
#Navigate to folder containing environmental data

#Loading a raster stack
envtListGJ <- list.files(pattern = ".asc"); #Gets a list of .asc files
envtStack <- stack(envtListGJ); #Reads in .asc files as a raster stack
crs(envtStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers
plot(envtStack); #Plots all the layers of the raster stack object

###Get M shapefile ##GJ Note: I am not using the buffered IUCN layer created above (obj = b.r). 
setwd("/Research/AFROBAT_ENM/AFROBATRACHIA/Leptopelis_nordequatorialis/zMAXENT") #Changing working directory to the folder where the shapefile of your M from created in Part One of the lab.
getwd()
#LcalShape1 <- readOGR("species_56272.shp"); #Reads in your shapefile ************************************DISABLE IF USING BUFFER
#crs(LcalShape1) <- crs(envtStack); #Defines Copper Pheasant M projection as identical to envtStack ******DISABLE IF USING BUFFER

###Masking your environmental variables to M 
envtStack <- crop(envtStack, CropFrog) #Crops raster stack to the extent of the shapefile you're using to mask your data
LcalTraining <- mask(envtStack, CropFrog) #Masking the raster stack
plot(LcalTraining)
writeRaster(LcalTraining, filename = "Leptopelis_nordequatorialis", format = "ascii", bylayer = T, suffix=names(envtStack), NAFlag = "-9999", overwrite = T); 
#Saves all the layers in the stack, with "SyrmaticusSoemmerringii" as a prefix



#SECTION 5
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#CLIP BIAS LAYER TO M LAYER. 

setwd("/Research/AFROBAT_ENM/AFROBATRACHIA/Leptopelis_nordequatorialis/zMAXENT")
Test2 <- raster(x = "./Leptopelis_nordequatorialis_reduced_bio1.asc")
finalBias <- raster("/Research/AFROBAT_ENM/AFROBATRACHIA/Leptopelis_nordequatorialis/biasLayerTRUE.asc")

CropX <- crop(finalBias, CropFrog) #Crops raster stack to the extent of the shapefile you're using to mask your data
MaskX <- mask(CropX, CropFrog) #Masking the raster stack
plot(MaskX)
MaskX
Test2
getwd()
writeRaster(MaskX, "BIAS_Leptopelis_nordequatorialis.asc", format = "ascii", NAFlag = "-9999", overwrite = T)

#RESTRICT DIMENSIONS TO 6 DECIMALS. 


#SECTION 5
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#======================================#======================================##======================================#======================================#
#THRESHOLDS

setwd("/Research/AFROBAT_ENM/AFROBATRACHIA/Leptopelis_nordequatorialis/zMAXENT")
getwd()

#First you will get the necessary data. You will need the occurrence point .csv file you used to train the model and an ascii file produced by Maxent (it will have the same name as the .png of your model results you submitted for lab last week).
LboulRaster <- raster("./Leptopelis_nordequatorialis_avg.asc"); #This is an ascii produced by Maxent with your model results
LboulPoints <- read.csv("/Research/AFROBAT_ENM/AFROBATRACHIA/Leptopelis_nordequatorialis/zLGF.csv"); #These are the points you used to train the model
points(LboulPoints[,2:3], pch = 16, cex = 2)

#First you will calculate the thresholds you will use. In this case, you'll find thresholds based on score quantiles from your results raster.
threshold <- quantile(LboulRaster);

#Generates plots of thresholds and statistics to illustrate sensitivity and specificity tradeoffs.
proportionPresent <- vector(mode = "list", length(threshold))
truePresences <- vector(mode = "list", length(threshold))
count <- 1;
while (count <= length(threshold)){
  m <- c(0, threshold[count], 0,  threshold[count], 1, 1); #This vector tells R that from 0 to the threshold, reclassify the raster cells as 0s, and from the threshold to 1, reclassify the cells as 1s.
  rclmat <- matrix(m, ncol=3, byrow=TRUE); #This turns the vector into a matrix
  threshed <- reclassify(LboulRaster, rcl = rclmat); #This reclassifies your raster.
  plot(threshed, main = paste("Threshold: ", threshold[count], sep = "")); #Plot the resulting raster, with the occurrence points.
  points(LboulPoints[,2:3], pch = ".") #Plots your occurrence points
  proportionPresent[[count]] <- table(values(threshed))[2] / (table(values(threshed))[1] + table(values(threshed))[2]) #Calculates the proportion of the training region over which presences are predicted
  truePresences[[count]] <- sum(na.omit(extract(threshed, LboulPoints[,2:3]))) #Counts the number of true presences predicted by the model
  count <- count + 1;
}

m <- c(0, threshold[3], 0,  threshold[3], 1, 1); #This vector tells R that from 0 to the threshold, reclassify the raster cells as 0s, and from the threshold to 1, reclassify the cells as 1s.
rclmat <- matrix(m, ncol=3, byrow=TRUE); #This turns the vector into a matrix
threshed <- reclassify(LboulRaster, rcl = rclmat); #This reclassifies your raster.
plot(threshed)
#points(LboulPoints[,2:3], pch = 16, cex = .8)



#Making and saving the threshold table
thresholdTable <- cbind(threshold, truePresences, proportionPresent) #Puts the results of your loop into a table
write.csv(thresholdTable, "ThresholdTable.csv", row.names = F) #Writes the results into a table
thresholdTable


#GJ <- extend(threshed, CropAfrica)
#crs(GJ) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
r <- raster(ncol=312, nrow=528) 
bb <- extent(7, 20, -10, 12) 
extent(r) <- bb 
r <- setExtent(r, bb)
GJ2 <- extend(threshed, r)
plot(GJ2)
GJ2

GJ3 <- crop(GJ2, CropAfrica)
crs(GJ3) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
plot(GJ3)
GJ3

newFrogRaster <- reclassify(GJ3, cbind(NA, 0))


setwd("/Research/AFROBAT_ENM/ARTHROLEPTID/BIODIV_INPUT/2") #for biodiverse program
writeRaster(newFrogRaster, filename="Leptopelis_nordequatorialis_BIAS.ascii", format="ascii", overwrite=TRUE)


