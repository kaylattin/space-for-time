# Load in library
library(tidyverse)
library(sp)
library(sf)

# Load in initial files
Species <- read.csv("BBS_SpeciesList.csv")
Species <- Species %>% select(AOU, English_Common_Name)
Obs <- read.csv("BBS_Weather.csv", header=T)

# -----------------------------------------------------------------------------------------------------------
# --------------------------#
#      PREP RAW BBS DATA    | 
# -------------------------#

# Load in raw 2019 BBS data
d1 <- read.csv("fifty1.csv", header = T)
d2 <- read.csv("fifty2.csv", header = T)
d3 <- read.csv("fifty3.csv", header = T)
d4 <- read.csv("fifty4.csv", header = T)
d5 <- read.csv("fifty5.csv", header = T)
d6 <- read.csv("fifty6.csv", header = T)
d7 <- read.csv("fifty7.csv", header = T)
d8 <- read.csv("fifty8.csv", header = T)
d9 <- read.csv("fifty9.csv", header = T)
d10  <- read.csv("fifty10.csv",header = T)

# Rename column in d8 so column names match across dataframes
names(d8)[names(d8)=="statenum"] <- "StateNum"

# Create one dataframe
d <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10)

# Create RouteNumber field, combining 'Route' and 'StateNum' fields
d$Placeholder <- paste(d$StateNum, d$Route, sep=".") # create unique placeholder ID for statenum + route
r <- distinct(d, StateNum, Route, .keep_all = TRUE) # Create another df to get a list of unique Route & StateNum's

# Convert Route to a RouteNum that combines StateNum and the individual Route within the state
# i.e. Statenum = 04 and Route = 001 becomes RouteNumber = 4001
# StateNum goes up to 2 digits and Route goes up to 3 digits
nrows <- length(r$Route)
  
for(i in 1:nrows) {
  print(paste0("Progress: ", round(i/nrows*100, 2), "% finished."))
  if(r$Route[i] < 100 & r$Route[i] >= 10) {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep = "0")
  } 
  else if(r$Route[i] < 10) {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep = "00")
  }
  else {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep = "")
  }
}
  
r <- r %>% select(Placeholder, RouteNumber) # Select out the new field
d <- merge(d, r, by = "Placeholder") # Merge with original df
d$Transect <- paste(d$RouteNumber, d$Year, sep=".") # Create unique transect column using RouteNumber
d <- select(d, c(CountryNum, StateNum, Route, Year, AOU, RouteNumber, Transect, Stop1, Stop2, Stop3, Stop4, Stop5, Stop6, Stop7, Stop8, Stop9, Stop10, Stop11)) 
d <- merge(d, Species, by = "AOU") # Match common bird names by AOU

write.csv(d, "rawdata.csv")

# -----------------------------------------------------------------------------------------------------------
# -------------------------#
#    CLEAN UP SPECIES     | 
# ------------------------#

d <- read.csv("rawdata.csv")

# Merge subspecies and hybrids into 1 species category for numerous species 
d$English_Common_Name <- gsub(".+? Yellow-rumped Warbler", "Yellow-rumped Warbler", d$English_Common_Name)
d$English_Common_Name <- gsub(".+? Northern Flicker", "Northern Flicker", d$English_Common_Name)
d$English_Common_Name <- gsub(".+? Dark-eyed Junco", "Dark-eyed Junco", d$English_Common_Name)
d$English_Common_Name <- gsub("African Collared Dove .+?", "African Collared Dove", d$English_Common_Name)
d$English_Common_Name <- gsub(".+? Great Blue Heron", "Great Blue Heron", d$English_Common_Name)
d$English_Common_Name<- gsub("Black Brant", "Brant", d$English_Common_Name)

# Remove unid. observations and hybrids
d <- d %>%  filter(!grepl('unid.|hybrid', English_Common_Name))

# Move into long format
d_long <- reshape(d, v.names = "Count", varying = 8:18, timevar = "Stop", times = names(dd)[8:18], direction='long')

# Get rid of extra text string in the Stop field
d_long$Stop <- str_remove(d_long$Stop, "Stop")


write.csv(dd_long, "d_long.csv")

# -----------------------------------------------------------------------------------------------------------
# ---------------------------#
#   FOREST vs OPEN BIRDS    | 
# --------------------------#
# Repeat the following code twice: once to get a base dataset for forest birds, and another to get a base dataset for open habitat birds


# Load csv file back in
d_long <- read.csv("d_long.csv")


# Read in forest bird guild designations
forestcodes <- read.csv("ForestCodes.csv", header = T)
forestcodes <- forestcodes %>% dplyr::select(English_Common_Name, new.status) %>% distinct(English_Common_Name, new.status)

# Merge forest codes into dd_long
df <- merge(d_long, forestcodes, by = "English_Common_Name")

## Select for either forest or open birds
df <- df %>% filter(new.status == "F") ## if doing forest birds at forest stops
#ddf <- ddf %>% filter(new.status == c("O")) ## if doing open birds at open stops


# Write to csv file to save progress
write.csv(df, "d_long_forest.csv")


# -----------------------------------------------------------------------------------------------------------
# ---------------------------#
#   SPECIES RESPONSES        | 
# --------------------------#
# Repeat the following code a total of 4 times: 1) forest bird richness, 2) open bird richness; 3) forest bird abundance, 4) open bird abundance
df <- read.csv("~/manuscript/Derived data products/d_long_open.csv")

# Sum across the x stops that are forested >60% within 100m 
# stopFO <- read.csv("stopsInForest.csv")   ## if doing forest birds at forest stops
stopFO<- read.csv("~/manuscript/Derived data products/stopsInOpen.csv")   ## if doing open birds at open stops

# Quick dataset clean-up of .csv file, obtained in code 02
stopFO$rte = gsub('0+$', '', stopFO$rte)
stopFO$RouteNumber <- sub("\\.[0-9]+$", "", stopFO$rte)
stopFO$Stop <- sub(".*\\.", "", stopFO$rte)

# Because of renaming of duplicate species (as subspecies or hybrids) above, counts for the same species can appear twice but not in the same record
# e.g., Northern Flicker, Yellow-Rumped Warbler, and Dark-eyed Junco might have more than 1 record per stop
# Group by my columns down to the stop-level and summarize the counts (only 1 record per species at a stop)
df <- df %>% group_by(Transect, RouteNumber, Year, CountryNum, English_Common_Name, Stop) %>% summarize(Count = sum(Count))

# Get list of routes
shp <- st_read("~/manuscript/Shapefiles/buffer_NA_dataset.shp")
shp <- as(shp, "Spatial")
rteno <- shp@data$rteno
ddf <- vector("list") # Initialize list

### BEGINNING OF IF FUNCTION for **SPECIES RICHNESS** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The code below calculates total richness or abundance across forested (or open, depending on the file loaded in above) stops
# for a given year at a given site 

# Suppress the summarise() messages so that the progress gets printed in the console
options(dplyr.summarise.inform = FALSE)

for( i in rteno ){
# Filter forested or open stop dataset to focus on route i
s <- stopFO %>% filter(RouteNumber == i )

  if(nrow(s) > 0){
    stop <- unique(s$Stop) # Identify stops (from 1 to 11) that are forested (or open) at route i
    f <- df %>% filter( RouteNumber == i ) %>% filter( Stop %in% stop ) # In the base dataset, for route i, keep only the stops that are forested
    years <- unique(f$Year) # Identify the years (from 2000 to 2019) that there is data for at route i 
    f_list <- vector("list") # Initialize list

    for( y in years ){
      syears <- s %>% filter(year == y ) # For year y in route i, identify further which stops are forested

      if(nrow(syears) > 0){
        stopyears <- unique(syears$Stop) 
        fyears <- f %>% filter( Year == y ) %>% filter( Stop %in% stopyears )  # In the base dataset, keep only the stops, in route i year y, that are forested (or open)

        # Calculate species richness at each stop within a site
        fyears <- fyears %>% group_by(Transect, RouteNumber, Year, CountryNum, Stop) %>% summarize(Richness = n_distinct(which(Count >= 1)))
        fyears$NumStops <- n_distinct(stopyears) # Calculate how many stops contribute to the calculation above
        f_list[[paste(y)]] <- fyears # Paste this dataset portion into the list initialized above, categorized by year

      }else{
        # If year y has no data in the bbs, just fill with dummy data
        Transect = NA
        RouteNumber = i
        Year = NA
        CountryNum = NA
        Stop = NA
        Richness = 0
        NumStops = 0

        f_list[[paste(y)]] <- data.frame(Transect, RouteNumber, Year, CountryNum, Stop, Richness, NumStops) # Paste dummy data for that year into list
      }
    }

    f <- do.call("rbind", f_list) # Rbind the list containing dataset for route i, separated by year y, into a big dataframe
    ddf[[paste(i)]] <- f # Paste the new dataframe for route i into another initialized list
    

  }else{
    # If the route has no forested (or open) stops, just fill with dummy data
    Transect = NA
    RouteNumber = i
    Year = NA
    CountryNum = NA
    Stop = NA
    Richness = 0
    NumStops = 0

    ddf[[paste(i)]] <- data.frame(Transect, RouteNumber, Year, CountryNum, Stop, Richness, NumStops)
  }

  if(! i %% 100){
    print(paste0("Progress: ", round((which(rteno == i)/length(rteno))*100, 2), "% finished."))
    flush.console()
  }
}

## END OF IF FUNCTION FOR **SPECIES RICHNESS** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### BEGINNING OF IF FUNCTION for **TOTAL ABUNDANCE** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The code below calculates total richness or abundance across forested (or open, depending on the file loaded in above) stops
# for a given year at a given site 

# Suppress the summarise() messages so that the progress gets printed in the console
options(dplyr.summarise.inform = FALSE)

for( i in rteno ){
  s <- stopFO %>% filter(RouteNumber == i )
  
  if(nrow(s) > 0){
    stop <- unique(s$Stop) # Identify stops (from 1 to 11) that are forested (or open) at route i
    f <- df %>% filter( RouteNumber == i ) %>% filter( Stop %in% stop ) # In the base dataset, for route i, keep only the stops that are forested
    years <- unique(f$Year) # Identify the years (from 2000 to 2019) that there is data for at route i 
    f_list <- vector("list") # Initialize list
    
    for( y in years ){
      syears <- s %>% filter(year == y ) # For year y in route i, identify further which stops are forested
      
      if(nrow(syears) > 0){
        stopyears <- unique(syears$Stop) 
        fyears <- f %>% filter( Year == y ) %>% filter( Stop %in% stopyears )  # In the base dataset, keep only the stops, in route i year y, that are forested (or open)
        
        # Calculate species richness at each stop within a site
        fyears <- fyears %>% group_by(Transect, RouteNumber, Year, CountryNum, Stop) %>% summarize(TA = sum(Count))
        fyears$NumStops <- n_distinct(stopyears) # Calculate how many stops contribute to the calculation above
        f_list[[paste(y)]] <- fyears # Paste this dataset portion into the list initialized above, categorized by year
        
      }else{
        # If year y has no data in the bbs, just fill with dummy data
        Transect = NA
        RouteNumber = i
        Year = NA
        CountryNum = NA
        Stop = NA
        TA = 0
        NumStops = 0
        
        f_list[[paste(y)]] <- data.frame(Transect, RouteNumber, Year, CountryNum, Stop, TA, NumStops) # Paste dummy data for that year into list
      }
    }
    
    f <- do.call("rbind", f_list) # Rbind the list containing dataset for route i, separated by year y, into a big dataframe
    ddf[[paste(i)]] <- f # Paste the new dataframe for route i into another initialized list
    
    
  }else{
    # If the route has no forested (or open) stops, just fill with dummy data
    Transect = NA
    RouteNumber = i
    Year = NA
    CountryNum = NA
    Stop = NA
    TA = 0
    NumStops = 0
    
    ddf[[paste(i)]] <- data.frame(Transect, RouteNumber, Year, CountryNum, Stop, TA, NumStops)
  }
  
  if(! i %% 100){
    print(paste0("Progress: ", round((which(rteno == i)/length(rteno))*100, 2), "% finished."))
    flush.console()
  }
}

## END OF IF FUNCTION FOR **TOTAL ABUNDANCE** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Rbind list containing datasets for each route i together into a dataframe
summarize_df <- do.call("rbind", ddf)

# Identify sites with no forested stops and remove them from the dataset
noforeststops <- summarize_df %>% filter(NumStops == 0) %>% distinct(RouteNumber)
summarize_df <- summarize_df %>% filter(!NumStops == 0)

# Tabulate total responses across stops for each route 
# summarize_df <- summarize_df %>% group_by(Transect, RouteNumber, Year, CountryNum, NumStops) %>% summarize(Richness = sum(Richness))
summarize_df <- summarize_df %>% group_by(Transect, RouteNumber, Year, CountryNum, NumStops) %>% summarize(TA = sum(TA))

# Write to csv to save progress
write.csv(summarize_df, "~/manuscript/derived data products/summarize_df_TO.csv")


# -----------------------------------------------------------------------------------------------------------
# -----------------------------#
#    OBSERVERS & WEATHER       | 
# ----------------------------#

# Create RouteNumber field, combining 'Route' and 'StateNum' fields
Obs$Placeholder <- paste(obs$StateNum, obs$Route, sep=".")  # Create unique Placeholder ID for statenum + route
r <- distinct(obs, StateNum, Route, .keep_all = TRUE) # Create another df to get a list of unique Route & StateNum's
r <- r %>% select (StateNum, Route) # Select columns of interest

# Convert Route to a RouteNum that combines State/Province number and the individual Route within the state
# i.e. Statenum = 04 and Route = 001 becomes RouteNumber = 4001
# StateNum goes up to 2 digits and Route goes up to 3 digits
nrows <- length(r$Route)

for(i in 1:nrows) {
  print(paste0("Progress: ", round(i/nrows*100, 2), "% finished."))
  if(r$Route[i] < 100 & r$Route[i] >= 10) {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep="0")
  } 
  else if(r$Route[i] < 10) {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep="00")
  }
  else {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep="")
  }
  
}

r <- dplyr::select(r, -c(RouteDataID, StateNum, Route, RunType))
Obs <- merge(Obs, r, by = "Placeholder", all.x = FALSE)
Obs$Transect <- paste(obs$RouteNumber, obs$Year, sep=".")





## Identify whether a route was an observer's first ever route in the bbs
Obs_id <- as.vector(unlist(Obs %>% distinct(ObsN)))
Obs_list <- vector("list") # Initialize list

for( i in Obs_id ){
  # Filter for all observations done by an observer in the whole history of bbs
  o <- Obs %>% filter(ObsN == i )
  nrow <- nrow(o)
  min <- min(o$Year)
  max <- max(o$Year)
  
  for( n in 1:nrow ){
    # Find the first year the observer surveyed for bbs and mark it as a first observation
    if( o$Year[n] == min ){
      o$FirstObs[n] = 2
    }else{
      o$FirstObs[n] = 1
    }
    
  }
  obs_list[[paste(i)]] <- o
}


Obs <- do.call("rbind", obs_list) # Rbind into a dataframe
Obs_clean <- Obs %>% dplyr::select(c(Transect, ObsN, StartWind, RunType, FirstObs)) # Select out columns of interest
Obs_clean <- Obs_clean[!duplicated(obs_clean$Transect), ]  # Remove duplicated transects - some had shared observers, I'll just go with 1


summarize_df$Transect <- paste(summarize_df$RouteNumber, summarize_df$Year, sep=".") # Re-make transect column of base dataset, sometimes it gets messed up
dddf <- merge(summarize_df, Obs_clean, by = "Transect", all.x = FALSE)   # Merge together
dddf <- dddf %>% filter(Year >= 2000) # Select years >= 2000


# -----------------------------------------------------------------------------------------------------------
# -----------------------------#
#        FOREST COVER          | ------------------------------------------------------------------------------------------
# ----------------------------#
forest <- read.csv("ForestCoverLong.csv")
forest$Transect <- paste(forest$routes, forest$Year, sep=".") # Create transect id column
ddddf <- merge(dddf, forest, by = "Transect", all.x = FALSE) # Merge forest data for each route into base dataset

# Save final base dataset (repeat 4 times for richness-forest bird, richness-open bird, abundance-forest bird, abundance-open bird)
write.csv(ddddf, "BaseDataset_RF.csv")
