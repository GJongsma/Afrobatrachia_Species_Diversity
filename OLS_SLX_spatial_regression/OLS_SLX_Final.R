# OLS and SLX Analysis
library(raster)
library(spdep)
library(spatialreg)
library(RColorBrewer)
library(rgdal)
library(ggpubr)
library(devtools)


setwd("/RESEARCH/2021/CHAPTER1_FINAL/RASTERS/OLS_SLX/ALL_variables")
# Load raster data (predictors and responders) and convert into spatial polygon data frame ----
asciiList <- list.files(path = "/RESEARCH/2021/CHAPTER1_FINAL/RASTERS/OLS_SLX/ALL_variables", 
                        pattern = ".asc", 
                        full.names = T, 
                        recursive = T) # Gets all raster file locations
rasterVars <- stack(asciiList)
rasterVars <- scale(rasterVars) # Done because 1) it's good practice for linear models and 2) it prevents calculation errors with the spatial regression
rasterVars <- mask(rasterVars, mask = rasterVars$rpd)
rasterVars <- aggregate(rasterVars, fact = 5) # Upscales the resolution of the dataset; done because otherwise my computer runs out of memory
spdfVars <- rasterToPolygons(rasterVars)
crs(spdfVars) <- "+proj=longlat +datum=WGS84"
writeOGR(spdfVars, ".", "5agg_spdVars.shp", driver="ESRI Shapefile")

# What does it look like?
str(slot(spdfVars,"data"))
names(spdfVars)

spplot(spdfVars, "richness")


# OLS ----

###### All predictor variables
richness.lm <- lm(richness~bio1+bio12+bio1Stability+bio12Stability+con_forest+elev+ForestStability, 
                  data=spdfVars@data)
summary(richness.lm)

###### Contemporary
richness.lm <- lm(pe~bio1+bio12+con_forest, data=spdfVars@data)
summary(richness.lm)

###### Historic
richness.lm <- lm(pe~bio1Stability+bio12Stability+ForestStability, data=spdfVars@data)
summary(richness.lm)

##### Only forest stability
richness.lm <- lm(richness~ForestStability, 
                  data=spdfVars@data)
summary(richness.lm)
plot(richness ~ ForestStability, data=spdfVars@data)
abline(richness.lm, col="red", lwd = 4)

install.packages("AER")
library(AER)

quadratic_model <- lm(richness ~ ForestStability + I(ForestStability^2), data = spdfVars@data)
coeftest(quadratic_model, vcov. = vcovHC, type = "HC1")

order_id <- order(spdfVars$ForestStability)
lines(x = spdfVars$ForestStability[order_id], 
      y = fitted(quadratic_model)[order_id],
      col = "red", 
      lwd = 4) 

ggplot(spdfVars@data,aes(y=richness,x=ForestStability))+geom_point()+geom_smooth(method="lm", formula = y ~ splines::bs(x, 1)) 
ggplot(spdfVars@data,aes(y=richness,x=bio1Stability))+geom_point()+geom_smooth(method="lm", formula = y ~ splines::bs(x, 1)) 
ggplot(spdfVars@data,aes(y=richness,x=bio12Stability))+geom_point()+geom_smooth(method="lm", formula = y ~ splines::bs(x, 1)) 
ggplot(spdfVars@data,aes(y=richness,x=con_forest))+geom_point()+geom_smooth(method="lm", formula = y ~ splines::bs(x, 1)) 
ggplot(spdfVars@data,aes(y=richness,x=con_forest))+geom_point()+geom_smooth(method="lm")

#RICHNESS Historic
h1rich<-ggplot(spdfVars@data,aes(y=richness,x=ForestStability))+geom_point()+geom_smooth(method="lm") 
h2rich<-ggplot(spdfVars@data,aes(y=richness,x=bio1Stability))+geom_point()+geom_smooth(method="lm") 
h3rich<-ggplot(spdfVars@data,aes(y=richness,x=bio12Stability))+geom_point()+geom_smooth(method="lm") 
#RICHNESS Contemporary
c1rich<-ggplot(spdfVars@data,aes(y=richness,x=con_forest))+geom_point()+geom_smooth(method="lm") 
c2rich<-ggplot(spdfVars@data,aes(y=richness,x=bio1))+geom_point()+geom_smooth(method="lm") 
c3rich<-ggplot(spdfVars@data,aes(y=richness,x=bio12))+geom_point()+geom_smooth(method="lm")

#PD Historic
h1pd<-ggplot(spdfVars@data,aes(y=pd,x=ForestStability))+geom_point()+geom_smooth(method="lm") 
h2pd<-ggplot(spdfVars@data,aes(y=pd,x=bio1Stability))+geom_point()+geom_smooth(method="lm") 
h3pd<-ggplot(spdfVars@data,aes(y=pd,x=bio12Stability))+geom_point()+geom_smooth(method="lm") 
#PD Contemporary
c1pd<-ggplot(spdfVars@data,aes(y=pd,x=con_forest))+geom_point()+geom_smooth(method="lm") 
c2pd<-ggplot(spdfVars@data,aes(y=pd,x=bio1))+geom_point()+geom_smooth(method="lm") 
c3pd<-ggplot(spdfVars@data,aes(y=pd,x=bio12))+geom_point()+geom_smooth(method="lm")

#RPD Historic
h1rpd<-ggplot(spdfVars@data,aes(y=rpd,x=ForestStability))+geom_point()+geom_smooth(method="lm") 
h2rpd<-ggplot(spdfVars@data,aes(y=rpd,x=bio1Stability))+geom_point()+geom_smooth(method="lm") 
h3rpd<-ggplot(spdfVars@data,aes(y=rpd,x=bio12Stability))+geom_point()+geom_smooth(method="lm") 
#RPD Contemporary
c1rpd<-ggplot(spdfVars@data,aes(y=rpd,x=con_forest))+geom_point()+geom_smooth(method="lm") 
c2rpd<-ggplot(spdfVars@data,aes(y=rpd,x=bio1))+geom_point()+geom_smooth(method="lm") 
c3rpd<-ggplot(spdfVars@data,aes(y=rpd,x=bio12))+geom_point()+geom_smooth(method="lm")

#PE Historic
h1pe<-ggplot(spdfVars@data,aes(y=pe,x=ForestStability))+geom_point()+geom_smooth(method="lm") 
h2pe<-ggplot(spdfVars@data,aes(y=pe,x=bio1Stability))+geom_point()+geom_smooth(method="lm") 
h3pe<-ggplot(spdfVars@data,aes(y=pe,x=bio12Stability))+geom_point()+geom_smooth(method="lm") 
#PE Contemporary
c1pe<-ggplot(spdfVars@data,aes(y=pe,x=con_forest))+geom_point()+geom_smooth(method="lm") 
c2pe<-ggplot(spdfVars@data,aes(y=pe,x=bio1))+geom_point()+geom_smooth(method="lm") 
c3pe<-ggplot(spdfVars@data,aes(y=pe,x=bio12))+geom_point()+geom_smooth(method="lm")


ggarrange(h1rich,h1pd,h1rpd,h1pe, ncol = 4, nrow = 1)
ggarrange(c1rich,c1pd,c1rpd,c1pe, ncol = 4, nrow = 1)
ggarrange(c3rich,c3pd,c3rpd,c3pe, ncol = 4, nrow = 1)

ggarrange(c2rich,c2pd,c2rpd,c2pe, ncol = 4, nrow = 1)
ggarrange(h2rich,h2pd,h2rpd,h2pe, ncol = 4, nrow = 1)
ggarrange(h3rich,h3pd,h3rpd,h3pe, ncol = 4, nrow = 1)

richness.lm <- lm(richness~bio1Stability, 
                  data=spdfVars@data)
summary(richness.lm)
plot(richness ~ bio12Stability, data=spdfVars@data)
abline(richness.lm, col="red", lwd = 4)



pd.lm <- lm(pd~bio1+bio12+bio1Stability+bio12Stability+con_forest+elev+ForestStability, 
            data=spdfVars@data)
summary(pd.lm)

pe.lm <- lm(pd~bio1+bio12+bio1Stability+bio12Stability+con_forest+elev+ForestStability, 
            data=spdfVars@data)
summary(pe.lm)

rpd.lm <- lm(rpd~bio1+bio12+bio1Stability+bio12Stability+con_forest+elev+ForestStability, 
             data=spdfVars@data)
summary(rpd.lm)

##### GJ MORAN I
lm.LMtests(richness.lm, nblist, test = "all", zero.policy = T)


# SLX ----
# Get neighbors and format object for input into LMlag

coords <- coordinates(spdfVars)
Wdist <- dnearneigh(coords,0,2,longlat = FALSE)
nblist<- nb2listw(Wdist, zero.policy = T)
plot(nblist,coordinates(spdfVars))


# Test for significance of spatial lag contribution
sar.richness <- lm.LMtests(richness.lm, nblist, test = "all", zero.policy = T)
summary(sar.richness) # There's spatial lag here.

sar.pd <- lm.LMtests(pd.lm, nblist, test = "all", zero.policy = T)
summary(sar.pd) # There's spatial lag here.

sar.pe <- lm.LMtests(pe.lm, nblist, test = "all", zero.policy = T)
summary(sar.pe) # There's spatial lag here.

sar.rpd <- lm.LMtests(rpd.lm, nblist, test = "all", zero.policy = T)
summary(sar.rpd) # There's spatial lag here.

# Since there's significant spatial lag for all of these, let's do spatial regressions


#####OLS & SLX
names(spdfVars)


##########   Richness

#####OLS
richness.lm5 <- lm(richness~bio1, data=spdfVars@data)
summary(richness.lm5)
richness.lm6 <- lm(richness~bio12, data=spdfVars@data)
summary(richness.lm6)
richness.lm4 <- lm(richness~con_forest, data=spdfVars@data)
summary(richness.lm4)
richness.lm2 <- lm(richness~bio1Stability, data=spdfVars@data)
summary(richness.lm2)
richness.lm3 <- lm(richness~bio12Stability, data=spdfVars@data)
summary(richness.lm3)
richness.lm <- lm(richness~ForestStability, data=spdfVars@data)
summary(richness.lm)

#Historic
LM.srH <- lm(richness~ForestStability+bio12Stability+bio1Stability, data=spdfVars@data)
summary(LM.srH)
#Contemporary
LM.srC <- lm(richness~con_forest+bio12+bio1, data=spdfVars@data)
summary(LM.srC)


######## SLX
SLX.sr5 <- lmSLX(richness~bio1, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.sr5)
impacts(SLX.sr5, nblist)

SLX.sr6 <- lmSLX(richness~bio12, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.sr6)
impacts(SLX.sr6, nblist)

SLX.sr4 <- lmSLX(richness~con_forest, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.sr4)
impacts(SLX.sr4, nblist)

SLX.sr2 <- lmSLX(richness~bio1Stability, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.sr2)
impacts(SLX.sr2, nblist)

SLX.sr3 <- lmSLX(richness~bio12Stability, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.sr3)
impacts(SLX.sr3, nblist)

SLX.sr <- lmSLX(richness~ForestStability, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.sr)
impacts(SLX.sr, nblist)

#Historic
SLX.srH <- lmSLX(richness~ForestStability+bio12Stability+bio1Stability, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.srH)
impacts(SLX.srH, nblist)

#Contemporary
SLX.srC <- lmSLX(richness~con_forest+bio12+bio1, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.srC)
impacts(SLX.srC, nblist)


#################### Phylogenetic Diversity
pd.lm <- lm(pd~ForestStability, data=spdfVars@data)
summary(pd.lm)
pd.lm2 <- lm(pd~bio1Stability, data=spdfVars@data)
summary(pd.lm2)
plot(pd~bio1Stability, data=spdfVars@data)
abline(pd.lm2, col="red", lwd = 4)
pd.lm3 <- lm(pd~bio12Stability, data=spdfVars@data)
summary(pd.lm3)
pd.lm4 <- lm(pd~con_forest, data=spdfVars@data)
summary(pd.lm4)
pd.lm5 <- lm(pd~bio1, data=spdfVars@data)
summary(pd.lm5)
pd.lm6 <- lm(pd~bio12, data=spdfVars@data)
summary(pd.lm6)

#Historic
LM.pdH <- lm(pd~ForestStability+bio12Stability+bio1Stability, data=spdfVars@data)
summary(LM.pdH)
#Contemporary
LM.pdC <- lm(pd~con_forest+bio12+bio1, data=spdfVars@data)
summary(LM.pdC)

######individial SLX
SLX.pd <- lmSLX(pd~ForestStability, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.pd)
impacts(SLX.pd, nblist)

SLX.pd2 <- lmSLX(pd~bio1Stability, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.pd2)
impacts(SLX.pd2, nblist)

SLX.pd3 <- lmSLX(pd~bio12Stability, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.pd3)
impacts(SLX.pd3, nblist)

SLX.pd4 <- lmSLX(pd~con_forest, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.pd4)
impacts(SLX.pd4, nblist)

SLX.pd5 <- lmSLX(pd~bio1, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.pd5)
impacts(SLX.pd5, nblist)

SLX.pd6 <- lmSLX(pd~bio12, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.pd6)
impacts(SLX.pd6, nblist)

#Historic
SLX.pd <- lmSLX(pd~ForestStability+bio12Stability+bio1Stability, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.pd)
impacts(SLX.pd, nblist)

#Contemporary
SLX.pd <- lmSLX(pd~con_forest+bio12+bio1, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.pd)
impacts(SLX.pd, nblist)


################ Relative Phylogenetic Diversity
rpd.lm5 <- lm(rpd~bio1, data=spdfVars@data)
summary(rpd.lm5)
rpd.lm6 <- lm(rpd~bio12, data=spdfVars@data)
summary(rpd.lm6)
rpd.lm4 <- lm(rpd~con_forest, data=spdfVars@data)
summary(rpd.lm4)
rpd.lm2 <- lm(rpd~bio1Stability, data=spdfVars@data)
summary(rpd.lm2)
rpd.lm3 <- lm(rpd~bio12Stability, data=spdfVars@data)
summary(rpd.lm3)
rpd.lm <- lm(rpd~ForestStability, data=spdfVars@data)
summary(rpd.lm, zstats=TRUE)

#Historic
LM.rpdH <- lm(rpd~ForestStability+bio12Stability+bio1Stability, data=spdfVars@data)
summary(LM.rpdH)
#Contemporary
LM.rpdC <- lm(rpd~con_forest+bio12+bio1, data=spdfVars@data)
summary(LM.rpdC)

########## SLX
SLX.rpd5 <- lmSLX(rpd~bio1, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.rpd5)
impacts(SLX.rpd5, nblist)

SLX.rpd6 <- lmSLX(rpd~bio12, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.rpd6)
impacts(SLX.rpd6, nblist)

SLX.rpd4 <- lmSLX(rpd~con_forest, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.rpd4)
impacts(SLX.rpd4, nblist)

SLX.rpd2 <- lmSLX(rpd~bio1Stability, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.rpd2)
impacts(SLX.rpd2, nblist)

SLX.rpd3 <- lmSLX(rpd~bio12Stability, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.rpd3)
impacts(SLX.rpd3, nblist)

SLX.rpd <- lmSLX(rpd~ForestStability, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.rpd)
impacts(SLX.rpd, nblist)

#Historic
SLX.rpdH <- lmSLX(rpd~ForestStability+bio12Stability+bio1Stability, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.rpdH)
impacts(SLX.rpdH, nblist)
summary(impacts(SLX.rpdH,nblist,R=100),zstats=TRUE)

#Contemporary
SLX.rpdC <- lmSLX(rpd~con_forest+bio12+bio1, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.rpdC)
impacts(SLX.rpdC, nblist)
summary(impacts(SLX.rpdC,nblist,R=100),zstats=TRUE)

################### Phylogenetic Endemism
pe.lm5 <- lm(pe~bio1, data=spdfVars@data)
summary(pe.lm5)
pe.lm6 <- lm(pe~bio12, data=spdfVars@data)
summary(pe.lm6)
pe.lm4 <- lm(pe~con_forest, data=spdfVars@data)
summary(pe.lm4)
pe.lm2 <- lm(pe~bio1Stability, data=spdfVars@data)
summary(pe.lm2)
pe.lm3 <- lm(pe~bio12Stability, data=spdfVars@data)
summary(pe.lm3)
pe.lm <- lm(pe~ForestStability, data=spdfVars@data)
summary(pe.lm)

SLX.pe5 <- lmSLX(pe~bio1, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.pe5)
impacts(SLX.pe5, nblist)

SLX.pe6 <- lmSLX(pe~bio12, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.pe6)
impacts(SLX.pe6, nblist)
summary(impacts(SLX.pe6,nblist))

SLX.pe4 <- lmSLX(pe~con_forest, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.pe4)
impacts(SLX.pe4, nblist)

SLX.pe2 <- lmSLX(pe~bio1Stability, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.pe2)
impacts(SLX.pe2, nblist)

SLX.pe3 <- lmSLX(pe~bio12Stability, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.pe3)
impacts(SLX.pe3, nblist)

SLX.pe <- lmSLX(pe~ForestStability, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.pe)
impacts(SLX.pe, nblist)

#Historic
SLX.peH <- lmSLX(pe~ForestStability+bio12Stability+bio1Stability, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.peH)
impacts(SLX.peH, nblist)

#Contemporary
SLX.peC <- lmSLX(pe~con_forest+bio12+bio1, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.peC)
impacts(SLX.peC, nblist)

#All
SLX.peA <- lmSLX(pe~con_forest+bio12+bio1+ForestStability+bio12Stability+bio1Stability, data=spdfVars@data, nblist, zero.policy = T)
summary(SLX.peA)

##########STEPWISE MODEL TESTS / BEST SUBSET
library(olsrr)

### Richness
lm.srA <- lm(richness~con_forest+bio12+bio1+ForestStability+bio12Stability+bio1Stability, data=spdfVars@data)
summary(lm.srA)

FWDfit.sr <- ols_step_forward_p(lm.srA, penter = .05)
FWDfit.sr2 <- ols_step_best_subset(lm.srA)
plot(FWDfit.sr)
plot(FWDfit.sr2)
FWDfit.sr
FWDfit.sr2

#### PD
lm.pdA <- lm(pd~con_forest+bio12+bio1+ForestStability+bio12Stability+bio1Stability, data=spdfVars@data)
summary(lm.pdA)

FWDfit.pd <- ols_step_forward_p(lm.pdA, penter = .005)
FWDfit.pd2 <- ols_step_best_subset(lm.pdA)
plot(FWDfit.pd)
plot(FWDfit.pd2)
FWDfit.pd
FWDfit.pd2

#### RPD
lm.rpdA <- lm(rpd~con_forest+bio12+bio1+ForestStability+bio12Stability+bio1Stability, data=spdfVars@data)
summary(lm.rpdA)

FWDfit.rpd <- ols_step_forward_p(lm.rpdA, penter = .05)
FWDfit.rpd2<- ols_step_best_subset(lm.rpdA)
plot(FWDfit.rpd)
plot(FWDfit.rpd2)
FWDfit.rpd
FWDfit.rpd2

#### PE
lm.peA <- lm(pe~con_forest+bio12+bio1+ForestStability+bio12Stability+bio1Stability, data=spdfVars@data)
summary(lm.peA)

FWDfit.pe <- ols_step_forward_p(lm.peA, penter = .05)
FWDfit.pe2 <- ols_step_best_subset(lm.peA)
plot(FWDfit.pe)
plot(FWDfit.pe2)
FWDfit.pe
FWDfit.pe2

####### Summary mixed-models
summary(lm.srA)
summary(lm.pdA)
summary(lm.rpdA)
summary(lm.peA)

#Summary of stepwise
FWDfit.sr
FWDfit.pd
FWDfit.rpd
FWDfit.pe

#Summary best-subsets
FWDfit.sr2
FWDfit.pd2
FWDfit.rpd2
FWDfit.pe2


########## FOREST STABILITY REMOVED
lm.srAns <- lm(richness~con_forest+bio12+bio1+bio12Stability+bio1Stability, data=spdfVars@data)
summary(lm.srAns)
lm.pdAns <- lm(pd~con_forest+bio12+bio1+bio12Stability+bio1Stability, data=spdfVars@data)
summary(lm.pdAns)
lm.rpdAns <- lm(rpd~con_forest+bio12+bio1+bio12Stability+bio1Stability, data=spdfVars@data)
summary(lm.rpdAns)
lm.peAns <- lm(pe~con_forest+bio12+bio1+bio12Stability+bio1Stability, data=spdfVars@data)
summary(lm.peAns)






