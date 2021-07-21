install.packages("spDataLarge")

library(sf)
library(ggplot2)
library(spData)
library(raster)
library(dplyr)
library(tmap)
library(leaflet)
library(cartogram)
library(scales)
library("spDataLarge")
library(rgdal)
library(maps)
library(mapdata)
library(climateStability)

library(RColorBrewer)
elevation <- raster("/RESEARCH/2020_PDtree/Contemp_Enviro/reduced/reduced_terrestrialAltitude.asc")
crs(elevation) <- "+proj=longlat +datum=WGS84"
elevation <- crop(elevation, stability)
plot(elevation)
my.palette <- brewer.pal(n = 9, name = "YlOrBr")
plot(elevation, col = my.palette)

#borders 2
Borders2 <- readOGR("/RESEARCH/2020_PDtree/Map_Figure/GJ_Africa_shp_simple/GJ_borders.shp")
crs(Borders2) <- "+proj=longlat +datum=WGS84"
cropBord <- crop(Borders2, x)
crs(cropBord) <- "+proj=longlat +datum=WGS84"
plot(cropBord, col = NA,  border= "grey50")

#Rivers
Rivers2 <- readOGR("/RESEARCH/2020_PDtree/Map_Figure/world_rivers/GJ_river_edit.shp")
crs(Rivers2) <- "+proj=longlat +datum=WGS84"
cropRiv <- crop(Rivers2, stability)
crs(cropRiv) <- "+proj=longlat +datum=WGS84"
plot(cropRiv, col= "blue", add = T)

#Refugia
refAnuf <- readOGR("/RESEARCH/2020_PDtree/Map_Figure/Refugia_Shape_GJ/Anuf_R1.shp")
crs(refAnuf) <- "+proj=longlat +datum=WGS84"
plot(refAnuf, border = 'grey80', lwd=3, add = T)

refMaley <- readOGR("/RESEARCH/2020_PDtree/Map_Figure/Refugia_Shape_GJ/MaleyR2.shp")
cropMaley <- crop(refMaley, stability)
crs(cropMaley) <- "+proj=longlat +datum=WGS84"
plot(cropMaley, border = 'grey80', lwd=3, add = T)

##Forest
forest <- raster("/RESEARCH/2020_PDtree/aJan2021/SAR_Jan26/Dist2Refuge/ContemporaryLGF-forest.asc")
plot(forest, add = T)
forest[forest > 60000] = NA
plot(forest)
forest[forest < 1] = NA
forest[forest > 100] = 1
plot(forest)
setwd("/RESEARCH/2020_PDtree/Map_Figure")
writeRaster(forest, filename="Forestlayer", format='ascii', overwrite=TRUE)

forest2 <-raster("Forestlayer.asc")
forest2 <- crop(forest2, stability)
crs(forest2) <- "+proj=longlat +datum=WGS84"
col3 <- alpha("chartreuse4", 0.25)
plot(forest2, col = col3, add = T, legend = FALSE)
#forestPoly <- rasterToPolygons(forest2)
#plot(forestPoly)
#writeOGR(forestPoly, dsn = '.', layer = 'poly', driver = "ESRI Shapefile")
#Forestshp <- readOGR("poly.shp")
#plot(Forestshp, col = col3)


#other features
CVL <- readOGR("/RESEARCH/2020_PDtree/Map_Figure/CVL.shp")
crs(CVL) <- "+proj=longlat +datum=WGS84"
plot(CVL, lty = 2, add = T)
MtA <- readOGR("/RESEARCH/2020_PDtree/Map_Figure/MontAlen.shp")
crs(MtA) <- "+proj=longlat +datum=WGS84"
plot(MtA, pch = 2, cex = 1.7, add = T)


#####All together
#svg("./draft/my_map2.svg")
#pdf("./draft/plots3.pdf", unit = 'cm',  compression = "zip")
pdf("./draft/plots5.pdf")
plot(elevation, col = my.palette, legend = F)
plot(forest2, col = col3, add = T, legend = FALSE)
plot(CVL, lty = 2, lwd = 1.7, add = T)
plot(cropBord, col = NA,  border= "grey50", add = T)
plot(cropRiv, col= "blue", add = T)
plot(refAnuf, border = 'grey50', lwd=1.5, density=c(20,40,60), col="grey50", add = T)
plot(cropMaley, lwd=2.5, add = T)
plot(MtA, pch = 24, cex = 1.7, lwd = 1.5, col="white", add = T)
plot(elevation, col = my.palette, legend.only = TRUE, pt.cex = 0.2, cex = 1, horizontal = TRUE, legend.width=1, legend.shrink=0.1, smallplot=c(0.34,.41, .2,.22),
     legend.args = list(text='Altitude (m)', side = 1, line = 2))
north.arrow(xb=7, yb=-6, len=0.22, lab="N") 

dev.off() 


#inset
Africa <- readOGR("/RESEARCH/2020_PDtree/Map_Figure/GJ_Africa_shp_simple/GJ2_Africa_shp_simpler.shp")
crs(Africa) <- "+proj=longlat +datum=WGS84"
plot(Africa, col = NA,  border= "grey50")

square_shape <- readOGR("/RESEARCH/2020_PDtree/Map_Figure/test_square/GOOD_SQAURE.shp")
col4 <- alpha("green", 0.25)
crs(square_shape) <- "+proj=longlat +datum=WGS84"
plot(square_shape, col = col4, lwd = 1, add=T)

pdf("./draft/inset3.pdf")
plot(Africa, col = NA,  border= "grey50")
plot(square_shape, col = col4, lwd = 1, add=T)

legend("bottomright", 
       legend = c("Rivers", "Camroon Volcanic Line", "Monte Alen"),
       col = c("blue", "black", "black"), 
       lty = c(1,2,NA),
       pch = c(NA,NA,24),
       bty = "n", 
       lwd = c(2,2,2),
       pt.cex = c(3,3,2), cex = 1,
       text.font = 1, inset = c(0.6, 0.115))

legend("bottomright", 
       legend = c("Forest Cover", "Forest Refugia (Maley 1996)", 
                  "Forest Refugium (Anuf 2006)"),
       col = c("chartreuse4", "black", "grey50"), 
       fill = c(col3,"NA","grey50"), 
       lty = c(NA,NA,NA),
       bty = "n", 
       density=c(NA,NA,20), 
       lwd = c(3),
       cex_size <- c(3,3,3), 
       pt.cex = 10, cex = 1,
       text.font = 1, inset = c(0.555, 0.25))

dev.off() 



#Stability

stability <- raster("/RESEARCH/2020_PDtree/Map_Figure/ForestStability.asc")
crs(stability) <- "+proj=longlat +datum=WGS84"
rescale(stability)
plot(stability)
plot(cropBord, col = NA,  border= "grey50", add = T)

x <- raster(ncol=528, nrow=312, xmn=7, xmx=20, ymn=-10, ymx=9)

stability <- crop(stability, x)
scaleStability <- rescale0to1(stability)
plot(scaleStability)

writeRaster(scaleStability, filename="cForestStability", format='ascii', overwrite=TRUE)

plot(stability)
plot(cropBord, col = NA,  border= "grey50", add = T)
plot(refAnuf, border = 'grey50', lwd=1.5, density=c(20,40,60), col="grey50", add = T)
plot(cropMaley, lwd=2.5, add = T)
scalebar(100)

pdf("./draft/stabilityall.pdf")
plot(scaleStability)
plot(cropBord, col = NA,  border= "grey50", add = T)
plot(refAnuf, border = 'grey50', lwd=1.5, density=c(20,40,60), col="grey50", add = T)
plot(cropMaley, lwd=2.5, add = T)
scalebar(100)
dev.off() 

# RESULTS MAP (ABOVE FOR RESOURCES)
##########################
##########################
##########################
##########################
##########################
##########################
# RESULTS MAP (ABOVE FOR RESOURCES)

x <- raster(ncol=528, nrow=312, xmn=7, xmx=20, ymn=-10, ymx=9)

#Richness
Rich <- raster("/RESEARCH/2020_PDtree/Map_Results/inputRaster/richness.asc")
Rich <- crop(Rich, x)
plot(Rich)
plot(Rich, col = NA,  border= "grey50", add = T)



#PD
PD <- raster("/RESEARCH/2020_PDtree/Map_Results/inputRaster/pd.asc")
PD <- crop(PD, x)
plot(PD)
plot(PD, col = NA,  border= "grey50", add = T)

#RPD
RPD <- raster("/RESEARCH/2020_PDtree/Map_Results/inputRaster/rpd.asc")
RPD <- crop(RPD, x)
plot(RPD)
plot(cropBord, col = NA,  border= "grey50", add = T)

#PE
PE <- raster("/RESEARCH/2020_PDtree/Map_Results/inputRaster/pe.asc")
PE <- crop(PE, x)
plot(PE)
plot(cropBord, col = NA,  border= "grey50", add = T)

#pdf("./draft/Diversity4.pdf")
#par(mfrow=c(1,1))
par(mfcol = c(1,4))
plot(Rich, box=FALSE, legend = F)
plot(Rich, legend.only = TRUE, pt.cex = 0.2, cex = 1, legend.width=1, legend.shrink=0.2, smallplot=c(0.30,.32, .3,.34))
plot(cropBord, col = NA,  border= "grey50", add = T)
plot(PD, axes=FALSE, box=FALSE)
plot(cropBord, col = NA,  border= "grey50", add = T)
plot(RPD, axes=FALSE, box=FALSE)
plot(cropBord, col = NA,  border= "grey50", add = T)
plot(PE, axes=FALSE, box=FALSE)
plot(cropBord, col = NA,  border= "grey50", add = T)
dev.off() 


