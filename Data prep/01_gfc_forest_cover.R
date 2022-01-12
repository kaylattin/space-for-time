# Load in libraries
library(gfcanalysis)
library(rgdal)
library(openxlsx)
library(tidyverse)
library(sf)
library(sp)
library(raster)

# Load data in
d <- readOGR("buffer_NA_dataset.shp") # dataset where bbs coords <= 1 km from line

## OR, if doing stops:
d <- readOGR("~/manuscript/Shapefiles/buffer_100m_dataset.shp") # dataset of stops buffered by 100m

d_proj <- sp::spTransform(d, CRS("+proj=longlat +datum=WGS84 +no_defs"))
# d_proj <- d_proj[!duplicated(d_proj$rteno),] # do only if using routes, not stops


setwd("D:/gfcanalysis_v3")

# Determine which tiles in the GFC are needed to cover area of interest ~~~~~~~~~~~~~~~~~~~~~~~~~
tiles <- calc_gfc_tiles(d)
plot(tiles) # Plot to check
plot(d) # Plot to check

# Download the files
download_tiles(
  tiles,
  output_folder = "D:/gfcanalysis_v3",
  images = c("treecover2000", "lossyear", "gain", "datamask"),
  dataset = "GFC-2019-v1.7"
)

#  Extract stack of 4 raster layers for the US & Canada ~~~~~~~~~~~~~~~~~~~~~~~~~
forest <- extract_gfc(
  d,
  data_folder = "D:/forestlayers_check",
  to_UTM = FALSE,
  stack = "change",
  dataset = "GFC-2019-v1.7"
)
writeRaster(forest, "gfc_analysis.tif", format= "GTiff", progress = "text", options = c("INTERLEAVE=BAND", "COMPRESS=LZW"))


# Create a thresholded raster layer product, where 50% tree canopy = forested ~~~~~~~~~~~~~~~~~~~~~~~~~
threshold_forest <- threshold_gfc(forest, forest_threshold = 50)
writeRaster(threshold_forest, filename = "gfc_analysis_threshold.tif", format = "GTiff", progress = "text", options = c("INTERLEAVE=BAND", "COMPRESS=LZW"))


# Load in the thresholded raseter stack as a RasterBrick (optional, if working in 2 parts)
threshold_forest <- brick("gfc_analysis_threshold.tif")

# Assign layers of the brick to different R objects
cover <- threshold_forest[[1]]
loss <- threshold_forest[[2]]
gain <- threshold_forest[[3]]
lossgain <- threshold_forest[[4]]

# Save objects as separate Rasters
writeRaster(cover, "threshold_cover2000.tif", format = "GTiff", options="COMPRESS=LZ77")
writeRaster(loss, "threshold_loss.tif", format = "GTiff", options="COMPRESS=LZ77")
writeRaster(gain, "threshold_gain.tif", format = "GTiff", options="COMPRESS=LZ77")


### ~~~~~~~~~~~~~~~~~~~~~~~~~ ### ANNUAL FOREST COVER FROM 2000 TO 2019 ### ~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract pixel counts of cover2000
exCover <- raster::extract(cover, d_proj, method = "simple")
# save(exCover, file = "extract_cover.RData")

# Extract pixel counts of loss
exLoss <- raster::extract(loss, d_proj, method = "simple")
# save(exLoss, file = "extract_loss.RData")

# Extract pixel counts of gain 
exGain <- raster::extract(gain, d_proj, method = "simple")
# save(exGain, file = "extract_gain.RData")

# Ceate a list of frequency tables for each route
coverTab <- lapply(exCover, table)
lossTab <- lapply(exLoss, table)
gainTab <- lapply(exGain, table)

# Initialize empty lists
forest <- vector("list") # list of forest cover for a single route
newForest <- vector("list")
elseForest <- vector("list")
pForest <- vector("list")
forestMat <- vector("list") # list of forest cover for each route; list of "forest"
pixelList <- vector("list")
percentList <- vector("list")

# Create a new loss dataframe for each route (in a more accessible format for the codes that follow)
newLoss <- vector("list")
routes <- d_proj@data$rteno

for(i in 1:3558) {
  u <- data.frame(lossTab[[i]])
  
  if (length(u$Freq) >= 1) {
    u$rte = routes[i]
  }
  else {
    u <- routes[i]
  }
  newLoss[[i]] <- u
}

# Bind dataframes into one dataframe
newLoss <- do.call("rbind", newLoss)

# Convert to wide format so that now every route has information on # pixels lost for every year - even if 0 
# i.e., years aren't skipped in the matrix, data entered for 2000 to 2019 inclusive so all tables are n = 19
newLoss_wide <- reshape(newLoss, direction = "wide", idvar = "rte", timevar = "Var1")
newLoss_wide[is.na(newLoss_wide)]<-0 


# The code below sequentially tabulates the number and overall percentage of forested pixels in each of the 3558 routes
# Starting with 2000, loss is subtracted to created a forest cover count for 2001; loss is subtracted again to create forest cover for 2002; and so on
# In 2012, gained pixels (over the last 12 years) are added, and then loss is subtracted, and the calculation continues as before
for(i in 1:3558) {
  
  coverMat <- matrix(coverTab[[i]])
  lossMat <- newLoss_wide[i,]
  gainMat <- matrix(gainTab[[i]])
  
  if (length(coverMat) == 2) {
    
  # num total pixels in the site
  tot <- sum(coverMat)
    
  # initiate forest list for the site in 2001
  f2000 <- coverMat[2, 1]
  p2000 <- f2000 / tot
  
  # set up 2001 in forest list
  newForest[[1]] <- f2000 - lossMat[,1+2]
  pForest[[1]] <- newForest[[1]] / tot
  
  # calculate cover - loss for 2002 to 2011
      for(k in 2:11) {
       newForest[[k]] <- newForest[[k-1]] - lossMat[,k+2]
       pForest[[k]] <- newForest[[k]] / tot
      }
  
          if (length(gainMat) == 2) {
          # add gain to 2012, then subtract loss
          newForest[[12]] <- newForest[[11]] + gainMat[2,1]
          pForest[[12]] <- (newForest[[12]] - lossMat[, 14]) / tot
          } else {
          newForest[[12]] <- newForest[[11]] - lossMat[, 14]
          pForest[[12]] <- newForest[[12]] / tot 
        }
  
  # continue with years 2013 to 2019
     for(n in 13:19) {
       newForest[[n]] <- newForest[[n-1]] - lossMat[, n+2]
       pForest[[n]] <- newForest[[n]] / tot
      }
  
  pixelList[[i]] <- newForest
  percentList[[i]] <- pForest
  
  
  t <- unlist(pForest)
  percent <- append(p2000, t)
  year <- seq(from = 2000, to = 2019)
  rte <- newLoss_wide$rte[i]
  forestMat[[i]] <- data.frame(rte, year, percent)

  }
     # if no cover in 2000, and no gain in 2012, then assign 0
      else {
        
        rte <- newLoss_wide$rte[i]
        year <- seq(from = 2000, to = 2019)
        percent <- 0
        forestMat[[i]] <- data.frame(rte, year, percent)

      }
 
  }
  
# Compile final master sheet
final <- do.call("rbind", forestMat) 
finalWide <- reshape(final, direction = "wide", idvar = "rte", timevar = "year")

# Write to xlsx - issues with writing to .csv (values no longer matched what was stored here in R)
write.xlsx(finalWide,"finalWide_check.xlsx")


### ~~~~~~~~~~~~~~~~~~~~~~~~~ ### ANNUAL COVER FOR 100 M STOP BUFFERS ### ~~~~~~~~~~~~~~~~~~~~~~~~~

# Before running the code below, run the code above using the R object dStops in order to get forest cover estimates for each 100m buffer
# Then, continue:
for(n in 1:nrow(final))
if(final$forest[n] >= 0.60){ # If forest cover in the stop is >= 60%, tag it as forested
  final$Forested[n] = 1
}else if(final$forest[n] <= 0.40){ # else if forest cover <= 40%, tag is as open
  final$Forested[n] = 0
}

# Find forested stops
stopsInForest<- vector("list") # Initailize list
for( i in rtes ){
  f <- final %>% filter(rteno == i)
  stops_in_forest[i] <- f %>% filter(Forested == 1)
}

stopsFinal <- do.call("rbind", stopsInForest)
stopsFinal$stop <- paste("Stop", stopsFinal$stop, sep="")
write.csv(stopsFinal, "stopsInForest.csv")

# Find open habitat stops
stopsInOpen<- vector("list") # Initailize list
for( i in rtes ){
  f <- final %>% filter(rteno == i)
  stopsInOpen[i] <- f %>% filter(Forested == 0)
}

stopsFinal <- do.call("rbind", stopsInOpen)
stopsFinal$stop <- paste("Stop", stopsFinal$stop, sep="")
write.csv(stopsFinal, "stopsInOpen.csv")
