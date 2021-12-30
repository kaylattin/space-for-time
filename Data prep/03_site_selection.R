# Load in libraries
library(sp)
library(sf)
library(rgdal)
library(tidyverse)
library(rgeos)
library(openxlsx)

# Note - run through this code twice: once for forest bird richness, and another for open bird richness
# Then proceed to code 05 to compile the final datasets and match total abundance datasets accordingly

# Load in data
bbs <- read.csv("BaseDataset_RF.csv", header = T) # Loading in base dataset for forest bird richness
bbs$Transect <- paste(bbs$RouteNumber, bbs$Year.x, sep = ".") # Re-make Transect id column
Ecoregions <- read.csv("Ecoregions.csv", header = T)
shp <- st_read("buffer_NA_dataset.shp")

bbs <- merge(bbs, Ecoregions, by = "RouteNumber", all.x = FALSE) # Merge base dataset with ecoregions



## TEMPORAL SITE SELECTION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The code below identifies the routes in which forest change has occurred >= 20% over the 19-year time period from 2000 to 2019, either as a gain or a loss
# Afterward, spatial site selection is done for each of the temporal sites identified

bbs$rte_id <- as.integer(as.factor(bbs$RouteNumber)) # Set up index for routes
rtes <- distinct(bbs, RouteNumber, rte_id, NA_L1CODE, NA_L1NAME) # Get a dataframe for all distinct bbs routes
nrtes <- length(unique(bbs$rte_id)) # Get list of unique bbs routes

# Find the new time ranges from 2000 to 2019 for routes after accounting for years with BBS data. Some routes don't have BBS data for every year in the range.
# Then, find the corresponding forest change that occurred in that new time range
for(i in 1:nrtes) {
  print(paste0("Progress: ", round(i/nrtes*100, 2), "% finished."))
  r <- bbs[which(bbs$rte_id == i),]
  n <- nrow(r)
    # Find minimum and maximum years, i.e. time range for that route
  min <- min(r$Year.x)
  max <- max(r$Year.x)
  
  for(n in 1:n) {
  # If the year on iteration n is the minimum (earliest) year for that route, then copy over its % forest cover
  if(r$Year.x[n] == min) {
    rtes$firstyear[i] <- r$Year.x[n]
    rtes$firstcover[i] <- r$Forest.cover[n]
  }
  # If the year on iteration n is the maximum (latest) year for that route, then copy over its % forest cover
  if(r$Year.x[n] == max){
    rtes$lastyear[i] <- r$Year.x[n]
    rtes$lastcover[i] <- r$Forest.cover[n]
  }
  }
}

# Find the change in % forest between first and last years with bbs data
rtes$change <- rtes$firstcover - rtes$lastcover

# Select for sites with forest cover change >= 20% within the time range (ideally from 2000 to 2019)
loss <- rtes[which(rtes$change >= 0.20), ]
gain <- rtes[which(rtes$change <= -0.20),]

# Filter the base dataset to contain routes identified as losing >= 20% over 19 years
time1 <- bbs %>% filter(RouteNumber %in% loss$RouteNumber)
time1 <- time1 %>% group_by(RouteNumber) %>% summarize(nyears = n_distinct(Year.x))

# Filter the base dataset to contain routes identified as gaining >= 20% over 19 years
time2 <- bbs %>% filter(RouteNumber %in% gain$RouteNumber)
time2 <- time2 %>% group_by(RouteNumber) %>% summarize(nyears = n_distinct(Year.x))

# In both loss and gain datasets, select for routes with at least 15 years of data in the bbs and cover at least 2000 to 2018 (18-19 years ideal)
loss <- time1 %>% filter(nyears >= 15)
gain <- time2 %>% filter(nyears >= 15)

temporal <- rbind(loss, gain) # Bind dataframes together to get an overall list of eligible temporal sites

write.csv(temporal, "list_of_temporal_sites_open.csv")




## SPATIAL SITE SELECTION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The code below cycles through each temporal site identified by the code above and selects eligible bbs routes, with data in 2019, that:
# 1) fall within the same ecoregion as the temporal site;
# 2) are within 300 km of the temporal site;
# 3) fall within the same forest cover gradient established by the temporal site's first and last year forest cover value
# There are as many sets of spatial sites as there are temporal sites (1 temporal site has 1 set of X number of spatial sites)


# Find bbs routes with data in 2019 and omit all the temporal sites identified above
sp2019 <- bbs[which(bbs$Year == 2019) , ]
sp2019 <- sp1997 %>% filter(!RouteNumber %in% temporal$RouteNumber)
sp2019 <- distinct(sp2019, RouteNumber, Year, Forest.cover, NA_L1CODE, NA_L1NAME)

ntemp <- nrow(temporal)
spEco.list <- vector("list")
spEco.list <- vector("list")

# Find lists of routes that are in the same ecoregion as each temporal route and fall within the same % forest cover range

#### LOSS ####
for(i in 18:43) { # replace this range with the number of temporal sites with LOSS
  tempSite <- temporal[i,]
  tempEco <- tempSite$NA_L1CODE
  
  # Select for sites that fall in the same ecoregion and fall in the same forest cover gradient established by the temporal site's first and last year forest cover
  # give or take 5% - lower gives me not a lot to work with and still probably represents forest cover gradient well; 10% was prob too big
  spEco.list[[paste(tempSite$RouteNumber)]] <- sp2019 %>% filter(NA_L1CODE == tempEco) %>% filter(Forest.cover >= (tempSite$firstcover+0.05)) %>% filter(Forest.cover <= (tempSite$lastcover-0.05))
  
}

#### GAIN ####
for(i in 1:17) { # replace this range with the number of temporal sites with GAIN
  tempSite <- temporal[i,]
  tempEco <- tempSite$NA_L1CODE
  
  # Select for sites that fall in the same ecoregion and fall in the same forest cover gradient established by the temporal site's first and last year forest cover
  # give or take 5% - lower gives me not a lot to work with and still probably represents forest cover gradient well; 10% was prob too big
  spEco.list[[paste(tempSite$RouteNumber)]] <- sp2019 %>% filter(NA_L1CODE == tempEco) %>% filter(Forest.cover <= (tempSite$firstcover-0.05)) %>% filter(Forest.cover >= (tempSite$lastcover+0.05))
  
}

temp_sites <- as.vector(temporal$RouteNumber) # Get list of temporal sites
shpSF <- st_as_sf(shp) # Convert route shapefile into a spatial feature layer - allows us to use dpylr:: functions on attribute table
spDist.list <- vector("list") # Initialize list
buff <- st_read("tempBuff_64_300.shp") # Read in shapefile filtered for the temporal sites identified in the step above, **buffered by 300 km**
buffSF <- st_as_sf(buff)

for(i in temp_sites){
  tempBuff <- buffSF %>% filter(rteno == i) # Get buffer for temporal site i
  tempBuff <- as(tempBuff, "Spatial") # Convert to spatial
  
  if(nrow(spEco.list[[paste(i)]]) > 0){
  
  spDF <- data.frame(spEco.list[[paste(i)]]) # Extract dataframe of spatial candidates for the temporal site identified above (same ecoregion)
  spList <- spDF$RouteNumber # Get list of spatial site candidates
  spShp_sf <- shpSF %>% filter(rteno %in% spList) # Filter the original shapefile containing all routes to be those identified above
  spShp <- as(spShp_sf, "Spatial") # Convert back to spatial object
  
  # Find spatial candidates that fall within the 300 km distance (buffered ArcMap in the shapefile "buff")
  dist <- spShp[tempBuff,]
  plot(dist) # Plot to verify working
  plot(tempBuff, add=TRUE) # Plot to verify working
  mtext(paste(i))
  
  if(nrow(st_as_sf(dist)) > 0) { # If error
    dist_sf <- st_as_sf(dist)
    dist_sf$ref <- i
    dist_df <- data.frame(dist_sf) %>% dplyr::select(rteno, ref)
    spDist.list[[paste(i)]] <- dist_df
  }
  
  }else{ # If error
    spDist.list[[paste(i)]] <- "no matches!"
}
}


## Get summary table of spatial sites per temporal site
sp.list <- vector("list") # Initialize list
for(i in sites){
  dummy <- data.frame(spDist.list[[paste(i)]])
  
  if(nrow(dummy) >= 15) {
  dummy <- dummy$rteno
  }
  else{
  dummy <- "less than 15!"
  }
  sp.list[[paste(i)]] <- dummy
}

n <- 65 # n = length of longest list in spDist.list (replace as needed if changing criteria above)
for(i in sites){
  df <- unlist(sp.list[[paste(i)]])
  length(df) <- n
  sp.list[[paste(i)]] <- df
}

# Cbind list of lists into wide format. Export to Excel and manually note which temporal sites to exclude because <15 spatial site matches
spatialList <- mapply(cbind, sp.list)
remove <- read.csv("TempSiteRemove.csv", header = T) # Import the list of temporal sites to remove because <15 spatial site matches, with header "ref"
write.csv(final, "spatial_candidates_open_october.csv")

### END OF SITE SELECTION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




# Get data into long format
spatialLong <- do.call("rbind", spDist.list)
final <- final %>% dplyr::filter(!ref %in% remove)

### FILTER BASE DATASETS
# Space
spatial <- do.call("rbind", spDist.list)
spatial$Transect <- paste(spatial$rteno, "1997", sep=".")
spatial_merge <- base::merge(spatial, bbs, by = "Transect")
spatial_merge$space.time <- rep(2)
spatial_merge <- spatial_merge %>% filter(!ref %in% remove)
write.csv(spatial_merge, "SpatialDataset_RF.csv")

# Time
temporal <- bbs %>% filter(RouteNumber %in% temp_sites)
temporal$space.time <- rep(1)
temporal <- temporal_f %>% filter(!RouteNumber %in% remove)
write.csv(temporal, "TemporalDataset_RF.csv")

# Will likely need to adjust the column order manually in Excel, re-import into R, and rbind:
space <- read.csv("SpatialDataset_RF.csv")
time <- read.csv("TemporalDataset_RF.csv")
RF <- rbind(space, time)
write.csv(RF, "FinalDataset_RF.csv")
