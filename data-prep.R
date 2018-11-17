
#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$
#$%   J. B. Russell Data Prep for Figures  $%#$
#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$

#rm(list=ls())   # remove all the variables from the workspace
# create list of dependant packages
list.of.packages <- c("data.table",
                      "plyr",
                      "tidyverse",
                      "reshape2",
                      "foreign",
                      "lubridate",
                      "ggExtra",
                      "grid",
                      "gridExtra",
                      "scales",
                      "RColorBrewer",
                      "ggthemes",
                      "extrafont",
                      "units",
                      "here")

# install any missing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load list of dependant packages
invisible(lapply(list.of.packages, library, character.only = T))


#######################################
### data prep for FIGS 1,2,3 (in-situ)
#######################################

# get list files for in-situ data
temp <- list.files(path = here("data/in-situ/vars-by-lake"), pattern = "*.csv", full.names = T)

# create list of names of files
names <- list.files(path = here("data/in-situ/vars-by-lake"), pattern = "*.csv", full.names = F)
names <- substr(names,1,nchar(names)-4)

# get list of files
myfiles = lapply(temp, read.delim)

for (i in 1:length(temp)){
  df <- read.csv(temp[i])
  df$Date <- as.Date(substr(df$Date1,1,8), format = "%Y%m%d")
  df[setdiff("Std..Error",names(df))] <- NA
  df$SE <- ifelse(df$Std..Error == 'NULL', NA, df$Std..Error)
  df <- df[,c("Date","Site","Value","Variable","SE")]
  assign(names[i], df)
} 

# bind data into one data.frame
meta.stats <- do.call(rbind, lapply(paste(names), get) )

# remove unused objects
rm(list = ls()[!ls() %in% c("meta.stats","today")])

# create factors to order and rename for plotting
meta.stats$Variable <- factor(meta.stats$Variable, levels = c('chloro','TSS','temp','DO'))
meta.stats$Variable <- mapvalues(meta.stats$Variable, from = c('chloro','TSS','temp','DO'), to = c("Chlorophyll", "TSS", "Temperature", "Dissolved Oxygen"))
meta.stats$Site <- factor(meta.stats$Site, levels = c('B1','P1','S2'))
meta.stats$Site <- mapvalues(meta.stats$Site, from = c('B1','P1','S2'), to = c("Bartlett", "Pleasant", "Saguaro"))
setDT(meta.stats) # make into data.table for easier refrencing



#######################################
### data prep for FIGS 4,5,6 (algae)
#######################################

# load data for algae plots
algae.data.new <- read.csv(here("data/in-situ/Surface_Counts.csv"))
algae.data.old <- read.csv(here("data/in-situ/Surface_Counts_PREVIOUS.csv"))

# clean up some variables for plotting
algae.data.new$Site <- substr(algae.data.new$id,9,10)
algae.data.new$Site <- factor(algae.data.new$Site, levels = c('B','P','S'))
algae.data.new$Site <- mapvalues(algae.data.new$Site, from = c('B','P','S'), to = c("Bartlett", "Pleasant", "Saguaro"))
algae.data.new$Date <- as.Date(substr(algae.data.new$id,1,8), format = "%Y%m%d")

algae.data.old$Site <- substr(algae.data.old$id,9,10)
algae.data.old$Site <- factor(algae.data.old$Site, levels = c('B','P','S'))
algae.data.old$Site <- mapvalues(algae.data.old$Site, from = c('B','P','S'), to = c("Bartlett", "Pleasant", "Saguaro"))
algae.data.old$Date <- as.Date(substr(algae.data.old$id,1,8), format = "%Y%m%d")

# calculate correction factor based on count type
algae.data.new$Count_Type <- gsub(" ","", algae.data.new$Count_Type)
algae.data.new$correction <- NA
algae.data.new$correction <- ifelse(algae.data.new$Count_Type == "stripe", 54, algae.data.new$correction)
algae.data.new$correction <- ifelse(algae.data.new$Count_Type == "field", 4746, algae.data.new$correction)
algae.data.new$correction <- ifelse(algae.data.new$Count_Type == "large_square", 40807, algae.data.new$correction)
algae.data.new$correction <- ifelse(algae.data.new$Count_Type == "small_square", 389859, algae.data.new$correction)

algae.data.old$Count_Type <- gsub(" ","", algae.data.old$Count_Type)
algae.data.old$correction <- NA
algae.data.old$correction <- ifelse(algae.data.old$Count_Type == "stripe", 54, algae.data.old$correction)
algae.data.old$correction <- ifelse(algae.data.old$Count_Type == "field", 4746, algae.data.old$correction)
algae.data.old$correction <- ifelse(algae.data.old$Count_Type == "large_square", 40807, algae.data.old$correction)
algae.data.old$correction <- ifelse(algae.data.old$Count_Type == "small_square", 389859, algae.data.old$correction)

# calculate abundance and upper/lower 95% CI
algae.data.new$Abundance <- algae.data.new$Cell_Number * algae.data.new$correction / algae.data.new$Count_Number / algae.data.new$Volume
algae.data.new$cell_u95 <- algae.data.new$Cell_Number + 2.42 + (1.96 * sqrt(algae.data.new$Cell_Number + 1.5))
algae.data.new$cell_l95 <- algae.data.new$Cell_Number + 1.42 - (1.96 * sqrt(algae.data.new$Cell_Number + 0.5))
algae.data.new$abun_u95 <- algae.data.new$cell_u95 * algae.data.new$correction / algae.data.new$Count_Number / algae.data.new$Volume
algae.data.new$abun_l95 <- algae.data.new$cell_l95 * algae.data.new$correction / algae.data.new$Count_Number / algae.data.new$Volume

algae.data.old$Abundance <- algae.data.old$Cell_Number * algae.data.old$correction / algae.data.old$Count_Number / algae.data.old$Volume
algae.data.old$cell_u95 <- algae.data.old$Cell_Number + 2.42 + (1.96 * sqrt(algae.data.old$Cell_Number + 1.5))
algae.data.old$cell_l95 <- algae.data.old$Cell_Number + 1.42 - (1.96 * sqrt(algae.data.old$Cell_Number + 0.5))
algae.data.old$abun_u95 <- algae.data.old$cell_u95 * algae.data.old$correction / algae.data.old$Count_Number / algae.data.old$Volume
algae.data.old$abun_l95 <- algae.data.old$cell_l95 * algae.data.old$correction / algae.data.old$Count_Number / algae.data.old$Volume

# fix underscore to space in "Filamentous Cyanbacteria"
algae.data.new$Class <- mapvalues(algae.data.new$Class, from = c("Chlorophytes", "Cryptophytes", "Diatoms", "Euglenoids", "Filamentous_Cyanbacteria", "Flagelletes", "Prymnesiophytes", "Synechococcus") , to = c("Chlorophytes", "Cryptophytes", "Diatoms", "Euglenoids", "Filamentous Cyanbacteria", "Flagelletes", "Prymnesiophytes", "Synechococcus") )
algae.data.old$Class <- mapvalues(algae.data.old$Class, from = c("Chlorophytes", "Cryptophytes", "Diatoms", "Euglenoids", "Filamentous_Cyanbacteria", "Flagelletes", "Prymnesiophytes", "Synechococcus") , to = c("Chlorophytes", "Cryptophytes", "Diatoms", "Euglenoids", "Filamentous Cyanbacteria", "Flagelletes", "Prymnesiophytes", "Synechococcus") )

# exclude a few of the classes
exclude <- c("Cryptophytes","Euglenoids","Flagelletes")
algae.data.new <- algae.data.new[!algae.data.new$Class %in% exclude,]
algae.data.old <- algae.data.old[!algae.data.old$Class %in% exclude,]

setDT(algae.data.new) # set as data.table
setDT(algae.data.old)

algae.data.new.agg <- algae.data.new %>% group_by(Date, Class, Site) %>% summarise_at(vars(Abundance, abun_u95, abun_l95), sum)
setDT(algae.data.new.agg) # set a data.table

# color schemes for algae
cols <- c("Chlorophytes" = "#000000", "Diatoms" = "#FF0400", "Filamentous Cyanbacteria" = "#BC9600", "Prymnesiophytes" = "#00638E", "Synechococcus" = "#005104")



#######################################
### data prep for FIG 7,8,9 (biomass)
#######################################

# calc bio-volume and 95% CI
algae.data.new$biovol <- algae.data.new$Abundance * algae.data.new$Cell_Volume
algae.data.new$bio_u95 <- algae.data.new$abun_u95 * algae.data.new$Cell_Volume
algae.data.new$bio_l95 <- algae.data.new$abun_l95 * algae.data.new$Cell_Volume

# aggregate
algae.data.agg2 <- algae.data.new %>% group_by(Date, Class, Site) %>% summarise_at(vars(biovol, bio_u95, bio_l95), sum)
setDT(algae.data.agg2) # set a data.table

# add NA of Abundance to end of old data for each Class & each Site (to create a manual break in the lines)
algae.data.old <- as.data.frame(algae.data.old)
old.end <- nrow(algae.data.old)
algae.data.old[nrow(algae.data.old)+15,] <- NA
algae.data.old[(old.end+1):(old.end+5),c("Site")] <- "Bartlett"
algae.data.old[(old.end+6):(old.end+10),c("Site")] <- "Saguaro"
algae.data.old[(old.end+11):(old.end+15),c("Site")] <- "Pleasant"
algae.data.old[(old.end+1):(old.end+15),c("Date")] <- max(algae.data.old$Date, na.rm = T) %m+% days(1) # make NA date 1 day after last sample
algae.data.old[(seq((old.end+1),(old.end+11),5) + 0),c("Class")] <- "Filamentous Cyanbacteria"
algae.data.old[(seq((old.end+1),(old.end+11),5) + 1),c("Class")] <- "Synechococcus"
algae.data.old[(seq((old.end+1),(old.end+11),5) + 2),c("Class")] <- "Chlorophytes"
algae.data.old[(seq((old.end+1),(old.end+11),5) + 3),c("Class")] <- "Diatoms"
algae.data.old[(seq((old.end+1),(old.end+11),5) + 4),c("Class")] <- "Prymnesiophytes"

# mark old and new data
algae.data.new$when <- "new"
algae.data.old$when <- "old"

# combine old and new data
algae.data.combo <- rbind(algae.data.old,algae.data.new[,-c("biovol","bio_u95","bio_l95")])

# aggregate
algae.data.agg.combo <- algae.data.combo %>% group_by(Date, Class, Site, when) %>% summarise_at(vars(Abundance, abun_u95, abun_l95), sum)
algae.data.agg.combo$when <- factor(algae.data.agg.combo$when, levels = c('old','new'))
setDT(algae.data.agg.combo) # set a data.table



#######################################
### data prep FIG 10,11,12 (abundance)
#######################################

# create a fixed date for labeling old dates to display correct dates 
algae.data.agg.combo$label.fix <- algae.data.agg.combo$Date

# manually adjust dates for old data to remove the large gap
algae.data.agg.combo$Date[algae.data.agg.combo$when == "old"] <- algae.data.agg.combo$Date[algae.data.agg.combo$when == "old"] %m+% years(7) 
algae.data.agg.combo$label.fix <- format(as.Date(algae.data.agg.combo$Date), "%b %Y")


#######################################
### data prep FIGS 13,14,15 (estimates)
#######################################

# read old data / all data
#meta.old <- read.csv("X:/Dropbox (ASU)/Chris-Jaz/Landsat_data_all/20180529_all_old_data.csv", stringsAsFactors = F)
meta.old <- read.csv(here("outputs/all_extracted_sat_data.csv"), stringsAsFactors = F)
new.chloro <- read.csv(here("data/in-situ/chlorophyll_data.csv"), stringsAsFactors = F)
new.tss <- read.csv(here("data/in-situ/TSS_data.csv"), stringsAsFactors = F)

# remove unused objects
#rm(list = ls()[!ls() %in% c("meta.old","today")])

# create factors to order and rename for plotting
meta.old <- meta.old[meta.old$Site == 'B1' | meta.old$Site == 'P1' | meta.old$Site == 'S2',]

# format Date
meta.old$Date <- as.Date(meta.old$Date)
new.chloro$Date <- as.Date(strptime(new.chloro$Date, format = "%Y%m%d"), format = "%Y-%m-%d")
new.tss$Date <- as.Date(strptime(new.tss$Date, format = "%Y%m%d"), format = "%Y-%m-%d")

# create adjusted variables
meta.old$sag_chl_est <- (1/8) * (289 - 5 * sqrt(8793 - 6400 * (meta.old$SRB2 / meta.old$SRB1)))
meta.old$sag_tss_est <- (1/88) * (1673 - sqrt(5) * sqrt(793901 - 352000 * (meta.old$SRB2 / meta.old$SRB1)))
meta.old$bar_chl_est <-  (1/2) * (sqrt(100000 * (meta.old$SRB3 / meta.old$SRB1) - 72845) - 15)
meta.old$bar_tss_est <- (1/162) * (1609 - (sqrt(5) * sqrt(709973 - 648000 * (meta.old$SRB3 / meta.old$SRB1))))
meta.old$ple_tss_est <- (53950 - sqrt(1736203771 - (24283000 * (meta.old$SRB4 - meta.old$SRB3)))) / 24283

#setDT(meta.old) # make into data.table for easier refrencing
#quantile(meta.old$chl_est[meta.old$Site == "Saguaro"], probs = c(0.05,0.95), na.rm = T)  #calc percentils

# add dots for actual values
#actual.val <- read.csv("Chl_bartlett.csv")

# merge satellite data to sample data by nearest day 
new.data <- merge(new.chloro, new.tss, by = c("Date", "Site"), all = T)
setDT(meta.old)
setDT(new.data)

setkey(new.data, Site, Date)[, SampleDate:=Date]
setkey(meta.old, Site, Date)[, SatDate:=Date]

merged.nearest <- meta.old[new.data, roll = 'nearest']

# add variable of day gap between sample and satellite pass
merged.nearest$DayDif <- abs(difftime(merged.nearest$SatDate, merged.nearest$SampleDate, units = "days"))

# create factor on site
meta.old$Site <- factor(meta.old$Site, levels = c('B1','P1','S2'))
meta.old$Site <- mapvalues(meta.old$Site, from = c('B1','P1','S2'), to = c("Bartlett", "Pleasant", "Saguaro"))

save.image(here("outputs/figure-data.RData"))

