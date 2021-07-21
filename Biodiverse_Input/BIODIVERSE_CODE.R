#Combining .csv for BioDiverse

library(raster)
library(sp)


#JULIE ALLEN"S
setwd("/Research/AFROBAT_ENM/ARTHROLEPTID/BIODIV_INPUT/2")
getwd()

list<-list.files(pattern = ".asc")
list
frogStack<-stack(list)
frogStack
crs(frogStack) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Defines projection of layers
#frogStack2<-stack(list, quick = TRUE)
#plots all of the maps -- check to make sure they look ok
plot(frogStack)
## adds all the rasters together to make a species richness map
sum<-sum(frogStack)
#sum2<-sum(LcalT)
## plot to mke sure ok
plot(sum)
#plot(sum, col = topo.colors(20))
#sum
#plot(sum, col = topo.colors(20)) #change color ramp to compare to TCN map. 
#  make a dataframe of all the points from the stack
df<-rasterToPoints(frogStack)
## write that to a csv file
write.csv(df,file="frog.csv",row.names=FALSE)

## run Naraynis function to make a dataset compatible to biodiverse -- code below

  
  BDCompatibleSpList_New <- function(InpFile)
  {
    FL_PAM = read.table(InpFile, header=TRUE, sep = ",")
    totCols = dim(FL_PAM)[2]
    OpMat = matrix(0,nrow=0,ncol=3)
    #totCols = 4
    for (i in 3:totCols)
    {
      OneSpData = FL_PAM[, c(1,2,i)]
      ColName = names(OneSpData)[3]
      print(ColName)
      #GenSpName = gsub("_ThrESS", "", ColName)
      genName = strsplit(ColName, "_")[[1]][1]
      spName = strsplit(ColName, "_")[[1]][2]
      SpPresence = which(OneSpData[,3]==1)
      OneSpMat = cbind(paste(genName,spName, sep="_"), OneSpData[SpPresence,1], OneSpData[SpPresence,2])
      OpMat = rbind(OpMat, OneSpMat)
    }
    return(OpMat)
  }


Op1=BDCompatibleSpList_New("frog.csv")
#write out the csv file
setwd("/Research/AFROBAT_ENM/ARTHROLEPTID/BIODIV_INPUT/2")
write.csv(Op1,file="GJFrogBiodiverse.data_new.csv",row.names=FALSE)


#Gabon specific
#############++++++++++++++++++++++++######################
#############++++++++++++++++++++++++######################


presentx <- crop(sum, GABON)
presentMas <- mask(presentx, GABON)
plot(presentMas, col = blue2red(10))
