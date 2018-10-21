# This purpose of this script is to loop through and extract 
# Landsat 7 & 8 geotiff raster data at given coordinates (points of intrest).

#rm(list=ls())   #remove all the variables from the workspace

# create list of dependant packages
list.of.packages <- c("sp",
                      "rgdal",
                      "rgeos",
                      "raster",
                      "tidyverse",
                      "data.table", 
                      "here")

# install any missing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load list of dependant packages
lapply(list.of.packages, library, character.only = TRUE)

# **NOTE: the landsat data is not inlcluded in this repo b/c of its large size. 
# **Download desired landsat data separately

# store the folder location that has all the subfolders of the landsat data
# there are two immideate subfolders, one for each satellite, 
# e.g. "landsat/Landsat7" for all L7 data
# each subfolder in the satellite folder should be the orginal untarred/unzipped geotiff files
# such that each subfolder is for a seperate day and each file in the folder should be a raster 
# of a satellite band for the relevant location(s)

# store names of sub folder for each satellite
satellites <- list.dirs(here("data/landsat"), recursive = F)          

# load csv with site lat longs (change to your points of intreset)
my.sites <- read.csv(here("in-situ/my_sites.csv"))

# convert data frame of lat-long points to coordinates (SpatialPointDataFrame)
sites.proj <- my.sites

# assign X & Y
coordinates(sites.proj) <- c("X", "Y")

# project points to long-lat projection WSG84
proj4string(sites.proj) <- CRS("+proj=longlat +datum=WGS84") 

# check coords are correct
print(sites.proj@coords, digits=10)

# create blank data frame for to be extracted data. 
# Each column is a site and each row is data from a specific band at that site for a date/time
all.site.data <- data.frame(matrix(vector(mode = 'numeric', length = nrow(my.sites)), nrow = 1, ncol = nrow(my.sites)))[0,]

# start loop through major satellite subfolders
# WARNING: this part may take a while depending on your computer specs 
# and how much data you are looping through
for(satellite in satellites){
  
  # list of sub folders where each (sub) sub folder contains all geotifs from a specific day for that satellite
  sub.folders <- list.dirs(satellite, recursive = T)[-1]  
  
  # list of just the name of each (sub) sub folder (to call by name)
  folder.names <- list.dirs(satellite, recursive = T, full.names =  F)[-1]  

  # loop through these (sub) sub folders, create raster stacks of geotifs in each subfolder, 
  # and extract data at the sites we are looking at (my.sites)
  for(a in 1:length(sub.folders)){
    
    # list of geotif files in sub folder of interation 'a'
    tif.list <- list.files(sub.folders[a], recursive= T, pattern="tif$", full.names= T)  
    
    # stack list of geotifs into a raster stack for sub folder of interation 'a'
    raster.stack <- stack(tif.list) 

    # convert my.sites to projection of current ARD geotifs
    sites.crs <- spTransform(sites.proj, crs(raster.stack))
    
    # extract and bind data to previous iteration (first iteration binds to empty data frame)
    
    # bind the data to the master data frame
    all.site.data <- rbind(all.site.data,
                           t(as.data.frame(raster::extract(raster.stack, sites.crs, method = 'simple', buffer = 30, small = F, cellnumbers = F,
                                                 fun = mean, na.rm = T, 1, nl = length(raster.stack@layers), df = F, factors = F))))
  }
  
  # force negative values to NAs  (https://landsat.usgs.gov/why-negative-values-over-water-landsat-surface-reflectance)
  all.site.data <- apply(all.site.data, c(1,2), function(x) {ifelse(x < 0, NA, x)}) 
  
  # write all data from each satellite to a csv
  colnames(all.site.data) <- as.character(my.sites$Location)
  write.csv(all.site.data, here(paste0("outputs/my_sites_", basename(satellite),"_data.csv")))
  saveRDS(all.site.data, here(paste0("outputs/my_sites_", basename(satellite),"_data.rds")))
  
  # empty the data frame and loop to populate from the next satellite
  all.site.data <- all.site.data[0,]
}

# because the previous loop can be time intensive, we save it incase we need to reload from here
save.image(file = here("outputs/landsat_data_extract.RData"))

####################################################
# data analysis for chloro & TSS

#rm(list=ls())   # remove all the variables from the workspace 
#gc() # refresh memory

# store today's data
#today <- format(Sys.Date(),"%Y%m%d")

# store the date of the past run (loop above) to make sure you load the most recent data
# by default this is todays date, change to a string of an older date if you are reloading an old run
#past.run <- today

# read chloro & TSS data
chloro.data <- read.csv(here("data/in-situ/chlorophyll_data.csv"))
TSS.data <- read.csv(here("data/in-situ/TSS_data.csv"))

# merge sample data
sample.data <- merge(chloro.data,TSS.data, by = c("Site","Date"), all = T)
rm(chloro.data,TSS.data)

# filtering to sites of interest in sample data
# Bartlett1 = B1 | Pleasant1 = P1 | Pleasant2 = P4 | Saguaro1 = S1 | Saguaro2 = S2 | Saguaro3 = S5
sample.data <- sample.data[sample.data$Site %in% c("B1","P1","P4","S1","S2","S5"),]

# read back in landsat data extracted for our sites
#L7.data <- read.csv(paste0("R_Codes/R_Outputs/",past.run,"_my_sites_extract_data_Landsat7.csv"), stringsAsFactors = F)
#L8.data <- read.csv(paste0("R_Codes/R_Outputs/",past.run,"_my_sites_extract_data_Landsat8.csv"), stringsAsFactors = F)
L7.data <- readRDS(here("outputs/my_sites_Landsat7_data.rds"))
L8.data <- readRDS(here("outputs/my_sites_Landsat8_data.rds"))

# add column for satellite and reorder cols in old data
L7.data$Sat <- 7
L8.data$Sat <- 8

# bind landsat data together
all.data <- rbind.fill(L7.data,L8.data)

# extract Date and Band from name ('X' column)
names(all.data)[names(all.data) == 'X'] <- "Date"
all.data$Band <- substr(all.data$Date, 42, nchar(all.data$Date))
all.data$Date <- substr(all.data$Date,16,23)

# convert the date sting to class date (better for matching and time differences)
all.data$Date <- as.Date(all.data$Date, "%Y%m%d")
sample.data$Date <- as.Date(as.character(sample.data$Date), "%Y%m%d")

# melt landsat data (breakdown to reformat)
all.data <- melt(all.data, id = c("Date","Band","Sat"), variable.name = "Site")

# cast landsat data (reformat)
all.data <- dcast(all.data, Date + Site + Sat ~ Band, fun.aggregate = mean)

# adjust bands on Landsat 8
all.data$SRB1 <- ifelse(all.data$Sat == 8, all.data$SRB2, all.data$SRB1)
all.data$SRB2 <- ifelse(all.data$Sat == 8, all.data$SRB3, all.data$SRB2)
all.data$SRB3 <- ifelse(all.data$Sat == 8, all.data$SRB4, all.data$SRB3)
all.data$SRB4 <- ifelse(all.data$Sat == 8, all.data$SRB5, all.data$SRB4)
all.data$SRB5 <- ifelse(all.data$Sat == 8, all.data$SRB6, all.data$SRB5)

# first merge satellite data to sample data by nearest day 
setDT(all.data)
setDT(sample.data)

setkey(sample.data, Site, Date)[, SampleDate:=Date]
setkey(all.data, Site, Date)[, SatDate:=Date]

merged.nearest <- all.data[sample.data, roll = 'nearest']

# add variable of day gap between sample and satellite pass
merged.nearest$DayDif <- abs(difftime(merged.nearest$SatDate, merged.nearest$SampleDate, units = "days"))

# drop original date column and reorder columns
merged.nearest$Date <- NULL

# merged within 1 day
merged.nearest.1day <- merged.nearest[merged.nearest$DayDif <= 1,]

# write files out
write.csv(merged.nearest, here("outputs/merged_nearest_data.csv"))
write.csv(merged.nearest.1day, here("outputs/merged_1day_data.csv"))
write.csv(all.data, here("outputs/all_extracted_sat_data.csv"))

# END