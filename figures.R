
#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#
#$%   J. Russell Thesis Figures   $%#
#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#

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
invisible(lapply(list.of.packages, library, character.only = TRUE))

options(scipen=10000)  # supress scientific notation
#font_import(prompt = F) # import local fonts (only necessary once per device, time consuming)
loadfonts(device = "win") # load fonts

# get list of files
temp <-  list.files(path = here("data/in-situ/vars-by-lake"), pattern = "*.csv", full.names = T)

# create list of names of files
names <-  list.files(path = here("data/in-situ/vars-by-lake"), pattern = "*.csv", full.names = F)
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

### FIG 1: BARTLETT time series cholro, TSS, temp, & DO
{
  Fig.1a <- (ggplot(data = meta.stats[Site == "Bartlett" & Variable == "Chlorophyll"])
             + geom_ribbon(aes(x = Date, ymin = Value - (1.96 * SE), ymax = Value + (1.96 * SE)), fill = "grey70")
             + geom_line(aes(x = Date, y = Value), size = 1)
             + geom_point(aes(x = Date, y = Value), size = 1.4)
             #+ geom_segment(aes(x = 1900, y = 0, xend = 2020, yend = 0))
             #+ geom_segment(aes(x = 1900, y = 0, xend = 1900, yend = 8000000))
             + scale_y_continuous(name = "Chlorophyll \n (?g/L)")
             + scale_x_date(limits = c(min(meta.stats$Date[meta.stats$Site == "Bartlett"]), max(meta.stats$Date[meta.stats$Site == "Bartlett"])))
             #+ ggtitle("")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 10, colour = "black"),
                     axis.text.y = element_text(size = 10, colour = "black"),
                     axis.line = element_line(size = .5, color = "black"),
                     plot.margin=unit(c(1, 5, 1, 1), units = "mm")  # add a little space to right side of plot
             )
  )
  
  Fig.1b <- (ggplot(data = meta.stats[Site == "Bartlett" & Variable == "TSS"])
             + geom_ribbon(aes(x = Date, ymin = Value - (1.96 * SE), ymax = Value + (1.96 * SE)), fill = "grey70")
             + geom_line(aes(x = Date, y = Value), size = 1)
             + geom_point(aes(x = Date, y = Value), size = 1.4)
             + scale_y_continuous(name = "TSS \n (mg/L)")
             + scale_x_date(limits = c(min(meta.stats$Date[meta.stats$Site == "Bartlett"]), max(meta.stats$Date[meta.stats$Site == "Bartlett"])))
             #+ ggtitle("")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 10, colour = "black"), 
                     axis.text.y = element_text(size = 10, colour = "black"),
                     axis.line = element_line(size = .5, color = "black"),
                     plot.margin=unit(c(1, 5, 1, 1), units = "mm")  # add a little space to right side of plot
             )
  )
  
  Fig.1c <- (ggplot(data = meta.stats[Site == "Bartlett" & Variable == "Temperature"])
             + geom_ribbon(aes(x = Date, ymin = Value - (1.96 * SE), ymax = Value + (1.96 * SE)), fill = "grey70")
             + geom_line(aes(x = Date, y = Value), size = 1)
             + geom_point(aes(x = Date, y = Value), size = 1.4)
             + scale_y_continuous(name = "Temperature \n (? C)")
             + scale_x_date(limits = c(min(meta.stats$Date[meta.stats$Site == "Bartlett"]), max(meta.stats$Date[meta.stats$Site == "Bartlett"])))
             #+ ggtitle("")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 10, colour = "black"), 
                     axis.text.y = element_text(size = 10, colour = "black"),
                     axis.line = element_line(size = .5, color = "black"),
                     plot.margin=unit(c(1, 5, 1, 1), units = "mm")  # add a little space to right side of plot
             )
  )
  
  Fig.1d <- (ggplot(data = meta.stats[Site == "Bartlett" & Variable == "Dissolved Oxygen"])
             + geom_ribbon(aes(x = Date, ymin = Value - (1.96 * SE), ymax = Value + (1.96 * SE)), fill = "grey70")
             + geom_line(aes(x = Date, y = Value), size = 1)
             + geom_point(aes(x = Date, y = Value), size = 1.4)
             + scale_y_continuous(name = "Dissolved Oxygen \n (%)")
             + scale_x_date(limits = c(min(meta.stats$Date[meta.stats$Site == "Bartlett"]), max(meta.stats$Date[meta.stats$Site == "Bartlett"])))
             #+ ggtitle("")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 10, colour = "black", margin = unit(c(2, 0, 2, 0), "mm")), # add a little space between x axis title), 
                     axis.text.y = element_text(size = 10, colour = "black"),
                     axis.line = element_line(size = .5, color = "black"),
                     plot.margin=unit(c(1, 5, 1, 1), units = "mm")  # add a little space to right side of plot
             )
  )
  
  Fig.1a <- arrangeGrob(Fig.1a, top = textGrob("(a)", x = unit(5, "mm"), y   = unit(3, "mm"), just = c("left","top"),
                                               gp = gpar(col="black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
  
  Fig.1b <- arrangeGrob(Fig.1b, top = textGrob("(b)", x = unit(5, "mm"), y = unit(3, "mm"), just = c("left","top"),
                                               gp = gpar(col = "black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
  
  Fig.1c <- arrangeGrob(Fig.1c, top = textGrob("(c)", x = unit(5, "mm"), y  = unit(5, "mm"), just = c("left","top"),
                                               gp = gpar(col = "black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
  
  Fig.1d <- arrangeGrob(Fig.1d, top = textGrob("(d)", x = unit(5, "mm") , y = unit(7, "mm"), just = c("left","top"),
                                               gp = gpar(col = "black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
  
  Fig.1 <- grid.arrange(Fig.1a, Fig.1b, Fig.1c, Fig.1d, nrow = 4,
                        bottom = textGrob("Date", gp = gpar(fontface = "plain", fontfamily = "Times New Roman", fontsize = 14))
  )
}

### FIG 2: SAGUARO time series cholro, TSS, temp, & DO
{
  Fig.2a <- (ggplot(data = meta.stats[Site == "Saguaro" & Variable == "Chlorophyll"])
             + geom_ribbon(aes(x = Date, ymin = Value - (1.96 * SE), ymax = Value + (1.96 * SE)), fill = "grey70")
             + geom_line(aes(x = Date, y = Value), size = 1)
             + geom_point(aes(x = Date, y = Value), size = 1.4)
             + scale_y_continuous(name = "Chlorophyll \n (?g/L)")
             + scale_x_date(limits = c(min(meta.stats$Date[meta.stats$Site == "Saguaro"]), max(meta.stats$Date[meta.stats$Site == "Saguaro"])))
             #+ ggtitle("")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 10, colour = "black"),
                     axis.text.y = element_text(size = 10, colour = "black"),
                     axis.line = element_line(size = .5, color = "black"),
                     plot.margin=unit(c(1, 5, 1, 1), units = "mm")  # add a little space to right side of plot
             )
  )
  
  Fig.2b <- (ggplot(data = meta.stats[Site == "Saguaro" & Variable == "TSS"])
             + geom_ribbon(aes(x = Date, ymin = Value - (1.96 * SE), ymax = Value + (1.96 * SE)), fill = "grey70")
             + geom_line(aes(x = Date, y = Value), size = 1)
             + geom_point(aes(x = Date, y = Value), size = 1.4)
             + scale_y_continuous(name = "TSS \n (mg/L)")
             + scale_x_date(limits = c(min(meta.stats$Date[meta.stats$Site == "Saguaro"]), max(meta.stats$Date[meta.stats$Site == "Saguaro"])))
             #+ ggtitle("")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 10, colour = "black"), 
                     axis.text.y = element_text(size = 10, colour = "black"),
                     axis.line = element_line(size = .5, color = "black"),
                     plot.margin=unit(c(1, 5, 1, 1), units = "mm")  # add a little space to right side of plot
             )
  )
  
  Fig.2c <- (ggplot(data = meta.stats[Site == "Saguaro" & Variable == "Temperature"])
             + geom_ribbon(aes(x = Date, ymin = Value - (1.96 * SE), ymax = Value + (1.96 * SE)), fill = "grey70")
             + geom_line(aes(x = Date, y = Value), size = 1)
             + geom_point(aes(x = Date, y = Value), size = 1.4)
             + scale_y_continuous(name = "Temperature \n (? C)")
             + scale_x_date(limits = c(min(meta.stats$Date[meta.stats$Site == "Saguaro"]), max(meta.stats$Date[meta.stats$Site == "Saguaro"])))
             #+ ggtitle("")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 10, colour = "black"), 
                     axis.text.y = element_text(size = 10, colour = "black"),
                     axis.line = element_line(size = .5, color = "black"),
                     plot.margin=unit(c(1, 5, 1, 1), units = "mm")  # add a little space to right side of plot
             )
  )
  
  Fig.2d <- (ggplot(data = meta.stats[Site == "Saguaro" & Variable == "Dissolved Oxygen"])
             + geom_ribbon(aes(x = Date, ymin = Value - (1.96 * SE), ymax = Value + (1.96 * SE)), fill = "grey70")
             + geom_line(aes(x = Date, y = Value), size = 1)
             + geom_point(aes(x = Date, y = Value), size = 1.4)
             + scale_y_continuous(name = "Dissolved Oxygen \n (%)")
             + scale_x_date(limits = c(min(meta.stats$Date[meta.stats$Site == "Saguaro"]), max(meta.stats$Date[meta.stats$Site == "Saguaro"])))
             + ggtitle("")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 10, colour = "black", margin = unit(c(2, 0, 2, 0), "mm")), # add a little space between x axis title), 
                     axis.text.y = element_text(size = 10, colour = "black"),
                     axis.line = element_line(size = .5, color = "black"),
                     plot.margin=unit(c(1, 5, 1, 1), units = "mm")  # add a little space to right side of plot
             )
  )
  
  Fig.2a <- arrangeGrob(Fig.2a, top = textGrob("(a)", x = unit(5, "mm"), y   = unit(3, "mm"), just = c("left","top"),
                                               gp = gpar(col="black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
  
  Fig.2b <- arrangeGrob(Fig.2b, top = textGrob("(b)", x = unit(5, "mm"), y = unit(3, "mm"), just = c("left","top"),
                                               gp = gpar(col = "black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
  
  Fig.2c <- arrangeGrob(Fig.2c, top = textGrob("(c)", x = unit(5, "mm"), y  = unit(5, "mm"), just = c("left","top"),
                                               gp = gpar(col = "black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
  
  Fig.2d <- arrangeGrob(Fig.2d, top = textGrob("(d)", x = unit(5, "mm") , y = unit(7, "mm"), just = c("left","top"),
                                               gp = gpar(col = "black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
  
  Fig.2 <- grid.arrange(Fig.2a, Fig.2b, Fig.2c, Fig.2d, nrow = 4,
                        bottom = textGrob("Date", gp = gpar(fontface = "plain", fontfamily = "Times New Roman", fontsize = 14))
  )
}

### FIG 3: PLEASANT time series cholro, TSS, temp, & DO
{
  Fig.3a <- (ggplot(data = meta.stats[Site == "Pleasant" & Variable == "Chlorophyll"])
             + geom_ribbon(aes(x = Date, ymin = Value - (1.96 * SE), ymax = Value + (1.96 * SE)), fill = "grey70")
             + geom_line(aes(x = Date, y = Value), size = 1)
             + geom_point(aes(x = Date, y = Value), size = 1.4)
             + scale_y_continuous(name = "Chlorophyll \n (?g/L)")
             + scale_x_date(limits = c(min(meta.stats$Date[meta.stats$Site == "Pleasant"]), max(meta.stats$Date[meta.stats$Site == "Pleasant"])))
             #+ ggtitle("")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 10, colour = "black"),
                     axis.text.y = element_text(size = 10, colour = "black"),
                     axis.line = element_line(size = .5, color = "black"),
                     plot.margin=unit(c(1, 5, 1, 1), units = "mm")  # add a little space to right side of plot
             )
  )
  
  Fig.3b <- (ggplot(data = meta.stats[Site == "Pleasant" & Variable == "TSS"])
             + geom_ribbon(aes(x = Date, ymin = Value - (1.96 * SE), ymax = Value + (1.96 * SE)), fill = "grey70")
             + geom_line(aes(x = Date, y = Value), size = 1)
             + geom_point(aes(x = Date, y = Value), size = 1.4)
             + scale_y_continuous(name = "TSS \n (mg/L)", breaks = c(1,2,3))
             + scale_x_date(limits = c(min(meta.stats$Date[meta.stats$Site == "Pleasant"]), max(meta.stats$Date[meta.stats$Site == "Pleasant"])))
             #+ ggtitle("")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 10, colour = "black"), 
                     axis.text.y = element_text(size = 10, colour = "black"),
                     axis.line = element_line(size = .5, color = "black"),
                     plot.margin=unit(c(1, 5, 1, 1), units = "mm")  # add a little space to right side of plot
             )
  )
  
  Fig.3c <- (ggplot(data = meta.stats[Site == "Pleasant" & Variable == "Temperature"])
             + geom_ribbon(aes(x = Date, ymin = Value - (1.96 * SE), ymax = Value + (1.96 * SE)), fill = "grey70")
             + geom_line(aes(x = Date, y = Value), size = 1)
             + geom_point(aes(x = Date, y = Value), size = 1.4)
             + scale_y_continuous(name = "Temperature \n (? C)")
             + scale_x_date(limits = c(min(meta.stats$Date[meta.stats$Site == "Pleasant"]), max(meta.stats$Date[meta.stats$Site == "Pleasant"])))
             #+ ggtitle("")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 10, colour = "black"), 
                     axis.text.y = element_text(size = 10, colour = "black"),
                     axis.line = element_line(size = .5, color = "black"),
                     plot.margin=unit(c(1, 5, 1, 1), units = "mm")  # add a little space to right side of plot
             )
  )
  
  Fig.3d <- (ggplot(data = meta.stats[Site == "Pleasant" & Variable == "Dissolved Oxygen"])
             + geom_ribbon(aes(x = Date, ymin = Value - (1.96 * SE), ymax = Value + (1.96 * SE)), fill = "grey70")
             + geom_line(aes(x = Date, y = Value), size = 1)
             + geom_point(aes(x = Date, y = Value), size = 1.4)
             + scale_y_continuous(name = "Dissolved Oxygen \n (%)")
             + scale_x_date(limits = c(min(meta.stats$Date[meta.stats$Site == "Pleasant"]), max(meta.stats$Date[meta.stats$Site == "Pleasant"])))
             #+ ggtitle("")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 10, colour = "black", margin = unit(c(2, 0, 2, 0), "mm")), # add a little space between x axis title), 
                     axis.text.y = element_text(size = 10, colour = "black"),
                     axis.line = element_line(size = .5, color = "black"),
                     plot.margin=unit(c(1, 5, 1, 1), units = "mm")  # add a little space to right side of plot
             )
  )
  
  Fig.3a <- arrangeGrob(Fig.3a, top = textGrob("(a)", x = unit(5, "mm"), y   = unit(3, "mm"), just = c("left","top"),
                                               gp = gpar(col="black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
  
  Fig.3b <- arrangeGrob(Fig.3b, top = textGrob("(b)", x = unit(5, "mm"), y = unit(3, "mm"), just = c("left","top"),
                                               gp = gpar(col = "black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
  
  Fig.3c <- arrangeGrob(Fig.3c, top = textGrob("(c)", x = unit(5, "mm"), y  = unit(5, "mm"), just = c("left","top"),
                                               gp = gpar(col = "black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
  
  Fig.3d <- arrangeGrob(Fig.3d, top = textGrob("(d)", x = unit(5, "mm") , y = unit(6, "mm"), just = c("left","top"),
                                               gp = gpar(col = "black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
  
  Fig.3 <- grid.arrange(Fig.3a, Fig.3b, Fig.3c, Fig.3d, nrow = 4,
                        bottom = textGrob("Date", gp = gpar(fontface = "plain", fontfamily = "Times New Roman", fontsize = 14))
  )
}

#*** FIGS 4,5,6 (algae)

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

# FIG 4 - Bartlett Abundance
{
  Fig.4a <- (ggplot(data = algae.data.new.agg[Site == "Bartlett" & (Class == "Filamentous Cyanbacteria" | Class == "Synechococcus")])
             + geom_ribbon(data = algae.data.new.agg[Class == "Filamentous Cyanbacteria" & Site == "Bartlett"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_ribbon(data = algae.data.new.agg[Class == "Synechococcus" & Site == "Bartlett"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_line(aes(x = Date, y = Abundance, colour = Class), size = 1.2)
             + geom_point(aes(x = Date, y = Abundance, colour = Class), size = 1.5)
             + scale_y_continuous(name = "cells/ml", labels = comma)
             + scale_x_date(limits = c(min(algae.data.new.agg$Date[algae.data.new.agg$Site == "Bartlett"]), max(algae.data.new.agg$Date[algae.data.new.agg$Site == "Bartlett"])))
             + scale_colour_manual(values = cols)
             #+ ggtitle("Bartlett Algal Abundance")
             #+ facet_grid(~ Class, scales = "free")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 10, colour = "black"), 
                     axis.text.y = element_text(size = 10, colour = "black", margin = unit(c(0, 2, 0, 2), "mm")),
                     legend.position = "bottom",
                     plot.margin=unit(c(1, 6, 1, 6), units = "mm")  # add a little space to right side of plot
             )
  )
  
  Fig.4b <- (ggplot(data = algae.data.new.agg[Site == "Bartlett" & (Class == "Chlorophytes" | Class == "Diatoms" | Class == "Prymnesiophytes")])
             + geom_ribbon(data = algae.data.new.agg[Class == "Chlorophytes" & Site == "Bartlett"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_ribbon(data = algae.data.new.agg[Class == "Diatoms" & Site == "Bartlett"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_ribbon(data = algae.data.new.agg[Class == "Prymnesiophytes" & Site == "Bartlett"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_line(aes(x = Date, y = Abundance, colour = Class), size = 1.2)
             + geom_point(aes(x = Date, y = Abundance, colour = Class), size = 1.5)
             + scale_y_continuous(name = "cells/ml", labels = comma)
             + scale_x_date(limits = c(min(algae.data.new.agg$Date[algae.data.new.agg$Site == "Bartlett"]), max(algae.data.new.agg$Date[algae.data.new.agg$Site == "Bartlett"])))
             + scale_colour_manual(values = cols)
             #+ ggtitle("Bartlett Algal Abundance")
             #+ facet_grid(~ Class, scales = "free")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 10, colour = "black"), 
                     axis.text.y = element_text(size = 10, colour = "black", margin = unit(c(0, 2, 0, 2), "mm")),
                     legend.position = "bottom",
                     plot.margin=unit(c(1, 6, 1, 6), units = "mm")  # add a little space to right side of plot
             )
  )
  
  Fig.4a <- arrangeGrob(Fig.4a, top = textGrob("(a)", x = unit(1, "mm"), y  = unit(1, "mm"), just = c("left","top"),
                                               gp = gpar(col = "black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
  
  Fig.4b <- arrangeGrob(Fig.4b, top = textGrob("(b)", x = unit(1, "mm") , y = unit(1, "mm"), just = c("left","top"),
                                               gp = gpar(col = "black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
  
  Fig.4 <- grid.arrange(Fig.4a, Fig.4b, nrow = 2,
                        bottom = textGrob("Date", gp = gpar(fontface = "plain", fontfamily = "Times New Roman", fontsize = 14))
  )
}

# FIG 5 - Saguaro Abundance
{
  Fig.5a <- (ggplot(data = algae.data.new.agg[Site == "Saguaro" & (Class == "Filamentous Cyanbacteria" | Class == "Synechococcus")])
             + geom_ribbon(data = algae.data.new.agg[Class == "Filamentous Cyanbacteria" & Site == "Saguaro"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_ribbon(data = algae.data.new.agg[Class == "Synechococcus" & Site == "Saguaro"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_line(aes(x = Date, y = Abundance, colour = Class), size = 1.2)
             + geom_point(aes(x = Date, y = Abundance, colour = Class), size = 1.5)
             + scale_y_continuous(name = "cells/ml", labels = comma)
             + scale_x_date(limits = c(min(algae.data.new.agg$Date[algae.data.new.agg$Site == "Saguaro"]), max(algae.data.new.agg$Date[algae.data.new.agg$Site == "Saguaro"])))
             + scale_colour_manual(values = cols)
             #+ ggtitle("Saguaro Algal Abundance")
             #+ facet_grid(~ Class, scales = "free")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 10, colour = "black"), 
                     axis.text.y = element_text(size = 10, colour = "black", margin = unit(c(0, 2, 0, 2), "mm")),
                     legend.position = "bottom",
                     plot.margin=unit(c(1, 6, 1, 6), units = "mm")  # add a little space to right side of plot
             )
  )
  
  Fig.5b <- (ggplot(data = algae.data.new.agg[Site == "Saguaro" & (Class == "Chlorophytes" | Class == "Diatoms" | Class == "Prymnesiophytes")])
             + geom_ribbon(data = algae.data.new.agg[Class == "Chlorophytes" & Site == "Saguaro"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_ribbon(data = algae.data.new.agg[Class == "Diatoms" & Site == "Saguaro"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_ribbon(data = algae.data.new.agg[Class == "Prymnesiophytes" & Site == "Saguaro"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_line(aes(x = Date, y = Abundance, colour = Class), size = 1.2)
             + geom_point(aes(x = Date, y = Abundance, colour = Class), size = 1.5)
             + scale_y_continuous(name = "cells/ml", labels = comma)
             + scale_x_date(limits = c(min(algae.data.new.agg$Date[algae.data.new.agg$Site == "Saguaro"]), max(algae.data.new.agg$Date[algae.data.new.agg$Site == "Saguaro"])))
             + scale_colour_manual(values = cols)
             #+ ggtitle("Saguaro Algal Abundance")
             #+ facet_grid(~ Class, scales = "free")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 10, colour = "black"), 
                     axis.text.y = element_text(size = 10, colour = "black", margin = unit(c(0, 2, 0, 2), "mm")),
                     legend.position = "bottom",
                     plot.margin=unit(c(1, 6, 1, 6), units = "mm")  # add a little space to right side of plot
             )
  )
  
  Fig.5a <- arrangeGrob(Fig.5a, top = textGrob("(a)", x = unit(1, "mm"), y  = unit(1, "mm"), just = c("left","top"),
                                               gp = gpar(col = "black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
  
  Fig.5b <- arrangeGrob(Fig.5b, top = textGrob("(b)", x = unit(1, "mm") , y = unit(1, "mm"), just = c("left","top"),
                                               gp = gpar(col = "black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
  
  Fig.5 <- grid.arrange(Fig.5a, Fig.5b, nrow = 2,
                        bottom = textGrob("Date", gp = gpar(fontface = "plain", fontfamily = "Times New Roman", fontsize = 14))
  )
}

# FIG 6 - Pleasant Abundance
{
  Fig.6a <- (ggplot(data = algae.data.new.agg[Site == "Pleasant" & (Class == "Filamentous Cyanbacteria" | Class == "Synechococcus")])
             + geom_ribbon(data = algae.data.new.agg[Class == "Filamentous Cyanbacteria" & Site == "Pleasant"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_ribbon(data = algae.data.new.agg[Class == "Synechococcus" & Site == "Pleasant"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_line(aes(x = Date, y = Abundance, colour = Class), size = 1.2)
             + geom_point(aes(x = Date, y = Abundance, colour = Class), size = 1.5)
             + scale_y_continuous(name = "cells/ml", labels = comma)
             + scale_x_date(limits = c(min(algae.data.new.agg$Date[algae.data.new.agg$Site == "Pleasant"]), max(algae.data.new.agg$Date[algae.data.new.agg$Site == "Pleasant"])))
             + scale_colour_manual(values = cols)
             #+ ggtitle("Pleasant Algal Abundance")
             #+ facet_grid(~ Class, scales = "free")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 10, colour = "black"), 
                     axis.text.y = element_text(size = 10, colour = "black", margin = unit(c(0, 2, 0, 2), "mm")),
                     legend.position = "bottom",
                     plot.margin=unit(c(1, 6, 1, 6), units = "mm")  # add a little space to right side of plot
             )
  )
  
  Fig.6b <- (ggplot(data = algae.data.new.agg[Site == "Pleasant" & (Class == "Chlorophytes" | Class == "Diatoms" | Class == "Prymnesiophytes")])
             + geom_ribbon(data = algae.data.new.agg[Class == "Chlorophytes" & Site == "Pleasant"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_ribbon(data = algae.data.new.agg[Class == "Diatoms" & Site == "Pleasant"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_ribbon(data = algae.data.new.agg[Class == "Prymnesiophytes" & Site == "Pleasant"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_line(aes(x = Date, y = Abundance, colour = Class), size = 1.2)
             + geom_point(aes(x = Date, y = Abundance, colour = Class), size = 1.5)
             + scale_y_continuous(name = "cells/ml", labels = comma)
             + scale_x_date(limits = c(min(algae.data.new.agg$Date[algae.data.new.agg$Site == "Pleasant"]), max(algae.data.new.agg$Date[algae.data.new.agg$Site == "Pleasant"])))
             + scale_colour_manual(values = cols)
             #+ ggtitle("Pleasant Algal Abundance")
             #+ facet_grid(~ Class, scales = "free")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 10, colour = "black"), 
                     axis.text.y = element_text(size = 10, colour = "black", margin = unit(c(0, 2, 0, 2), "mm")),
                     legend.position = "bottom",
                     plot.margin=unit(c(1, 6, 1, 6), units = "mm")  # add a little space to right side of plot
             )
  )
  
  Fig.6a <- arrangeGrob(Fig.6a, top = textGrob("(a)", x = unit(1, "mm"), y  = unit(1, "mm"), just = c("left","top"),
                                               gp = gpar(col = "black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
  
  Fig.6b <- arrangeGrob(Fig.6b, top = textGrob("(b)", x = unit(1, "mm") , y = unit(1, "mm"), just = c("left","top"),
                                               gp = gpar(col = "black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
  
  Fig.6 <- grid.arrange(Fig.6a, Fig.6b, nrow = 2,
                        bottom = textGrob("Date", gp = gpar(fontface = "plain", fontfamily = "Times New Roman", fontsize = 14))
  )
}

### FIG 7,8,9 (biomass)

# calc bio-volume and 95% CI
algae.data.new$biovol <- algae.data.new$Abundance * algae.data.new$Cell_Volume
algae.data.new$bio_u95 <- algae.data.new$abun_u95 * algae.data.new$Cell_Volume
algae.data.new$bio_l95 <- algae.data.new$abun_l95 * algae.data.new$Cell_Volume

# aggregate
algae.data.agg2 <- algae.data.new %>% group_by(Date, Class, Site) %>% summarise_at(vars(biovol, bio_u95, bio_l95), sum)
setDT(algae.data.agg2) # set a data.table

# FIG 7 - Bartlett Biomass
{
  Fig.7 <- (ggplot(data = algae.data.agg2[Site == "Bartlett"])
            + geom_ribbon(data = algae.data.agg2[Class == "Chlorophytes" & Site == "Bartlett"], aes(x = Date, ymin = bio_l95, ymax = bio_u95), fill = "grey90", inherit.aes = FALSE)
            + geom_ribbon(data = algae.data.agg2[Class == "Diatoms" & Site == "Bartlett"], aes(x = Date, ymin = bio_l95, ymax = bio_u95), fill = "grey90", inherit.aes = FALSE)
            + geom_ribbon(data = algae.data.agg2[Class == "Filamentous Cyanbacteria" & Site == "Bartlett"], aes(x = Date, ymin = bio_l95, ymax = bio_u95), fill = "grey90", inherit.aes = FALSE)
            + geom_ribbon(data = algae.data.agg2[Class == "Prymnesiophytes" & Site == "Bartlett"], aes(x = Date, ymin = bio_l95, ymax = bio_u95), fill = "grey90", inherit.aes = FALSE)
            + geom_ribbon(data = algae.data.agg2[Class == "Synechococcus" & Site == "Bartlett"], aes(x = Date, ymin = bio_l95, ymax = bio_u95), fill = "grey90", inherit.aes = FALSE)
            + geom_line(aes(x = Date, y = biovol, colour = Class), size = 1.2)
            + geom_point(aes(x = Date, y = biovol, colour = Class), size = 1.5)
            + scale_y_continuous(name = "?m?", labels = comma)
            + scale_x_date(name = "Date", limits = c(min(algae.data.agg2$Date[algae.data.agg2$Site == "Bartlett"]), max(algae.data.agg2$Date[algae.data.agg2$Site == "Bartlett"])))
            + scale_colour_manual(values = cols)
            + theme_minimal()
            + guides(color = guide_legend(nrow = 2,byrow=TRUE))
            + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                    plot.title = element_text(hjust = 0.47, size = 11),
                    axis.title.y = element_text(size = 13, face = "plain"),
                    #axis.title.x = element_blank(),
                    axis.text.x = element_text(size = 10, colour = "black", margin = unit(c(0, 0, 2, 0), "mm")), 
                    axis.text.y = element_text(size = 10, colour = "black", margin = unit(c(0, 2, 0, 2), "mm")),
                    legend.position = "bottom",
                    plot.margin=unit(c(5, 6, 0, 1), units = "mm")  
            )
  )
}

# FIG 8 - Saguaro Biomass
{
  Fig.8 <- (ggplot(data = algae.data.agg2[Site == "Saguaro"])
            + geom_ribbon(data = algae.data.agg2[Class == "Chlorophytes" & Site == "Saguaro"], aes(x = Date, ymin = bio_l95, ymax = bio_u95), fill = "grey90", inherit.aes = FALSE)
            + geom_ribbon(data = algae.data.agg2[Class == "Diatoms" & Site == "Saguaro"], aes(x = Date, ymin = bio_l95, ymax = bio_u95), fill = "grey90", inherit.aes = FALSE)
            + geom_ribbon(data = algae.data.agg2[Class == "Filamentous Cyanbacteria" & Site == "Saguaro"], aes(x = Date, ymin = bio_l95, ymax = bio_u95), fill = "grey90", inherit.aes = FALSE)
            + geom_ribbon(data = algae.data.agg2[Class == "Prymnesiophytes" & Site == "Saguaro"], aes(x = Date, ymin = bio_l95, ymax = bio_u95), fill = "grey90", inherit.aes = FALSE)
            + geom_ribbon(data = algae.data.agg2[Class == "Synechococcus" & Site == "Saguaro"], aes(x = Date, ymin = bio_l95, ymax = bio_u95), fill = "grey90", inherit.aes = FALSE)
            + geom_line(aes(x = Date, y = biovol, colour = Class), size = 1.2)
            + geom_point(aes(x = Date, y = biovol, colour = Class), size = 1.5)
            + scale_y_continuous(name = "?m?", labels = comma)
            + scale_x_date(name = "Date", limits = c(min(algae.data.agg2$Date[algae.data.agg2$Site == "Saguaro"]), max(algae.data.agg2$Date[algae.data.agg2$Site == "Saguaro"])))
            + scale_colour_manual(values = cols)
            + theme_minimal()
            + guides(color = guide_legend(nrow = 2,byrow=TRUE))
            + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                    plot.title = element_text(hjust = 0.47, size = 11),
                    axis.title.y = element_text(size = 13, face = "plain"),
                    #axis.title.x = element_blank(),
                    axis.text.x = element_text(size = 10, colour = "black", margin = unit(c(0, 0, 2, 0), "mm")), 
                    axis.text.y = element_text(size = 10, colour = "black", margin = unit(c(0, 2, 0, 2), "mm")),
                    legend.position = "bottom",
                    plot.margin=unit(c(5, 6, 0, 1), units = "mm")  
            )
  )
}

# FIG 9 - Pleasant Biomass
{
  Fig.9 <- (ggplot(data = algae.data.agg2[Site == "Pleasant"])
            + geom_ribbon(data = algae.data.agg2[Class == "Chlorophytes" & Site == "Pleasant"], aes(x = Date, ymin = bio_l95, ymax = bio_u95), fill = "grey90", inherit.aes = FALSE)
            + geom_ribbon(data = algae.data.agg2[Class == "Diatoms" & Site == "Pleasant"], aes(x = Date, ymin = bio_l95, ymax = bio_u95), fill = "grey90", inherit.aes = FALSE)
            + geom_ribbon(data = algae.data.agg2[Class == "Filamentous Cyanbacteria" & Site == "Pleasant"], aes(x = Date, ymin = bio_l95, ymax = bio_u95), fill = "grey90", inherit.aes = FALSE)
            + geom_ribbon(data = algae.data.agg2[Class == "Prymnesiophytes" & Site == "Pleasant"], aes(x = Date, ymin = bio_l95, ymax = bio_u95), fill = "grey90", inherit.aes = FALSE)
            + geom_ribbon(data = algae.data.agg2[Class == "Synechococcus" & Site == "Pleasant"], aes(x = Date, ymin = bio_l95, ymax = bio_u95), fill = "grey90", inherit.aes = FALSE)
            + geom_line(aes(x = Date, y = biovol, colour = Class), size = 1.2)
            + geom_point(aes(x = Date, y = biovol, colour = Class), size = 1.5)
            + scale_y_continuous(name = "?m?", labels = comma)
            + scale_x_date(name = "Date", limits = c(min(algae.data.agg2$Date[algae.data.agg2$Site == "Pleasant"]), max(algae.data.agg2$Date[algae.data.agg2$Site == "Pleasant"])))
            + scale_colour_manual(values = cols)
            + theme_minimal()
            + guides(color = guide_legend(nrow = 2,byrow=TRUE))
            + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                    plot.title = element_text(hjust = 0.47, size = 11),
                    axis.title.y = element_text(size = 13, face = "plain"),
                    #axis.title.x = element_blank(),
                    axis.text.x = element_text(size = 10, colour = "black", margin = unit(c(0, 0, 2, 0), "mm")), 
                    axis.text.y = element_text(size = 10, colour = "black", margin = unit(c(0, 2, 0, 2), "mm")),
                    legend.position = "bottom",
                    plot.margin=unit(c(5, 6, 0, 1), units = "mm")  
            )
  )
}


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

# FIG 10 - Bartlett Abundance New + Old



# create a fixed date for labeling old dates to display correct dates 
algae.data.agg.combo$label.fix <- algae.data.agg.combo$Date

# manually adjust dates for old data to remove the large gap
algae.data.agg.combo$Date[algae.data.agg.combo$when == "old"] <- algae.data.agg.combo$Date[algae.data.agg.combo$when == "old"] %m+% years(7) 
algae.data.agg.combo$label.fix <- format(as.Date(algae.data.agg.combo$Date), "%b %Y")

{
  Fig.10a <- (ggplot(data = algae.data.agg.combo[Site == "Bartlett" & (Class == "Filamentous Cyanbacteria" | Class == "Synechococcus")])
             + geom_ribbon(data = algae.data.agg.combo[Class == "Filamentous Cyanbacteria" & Site == "Bartlett"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_ribbon(data = algae.data.agg.combo[Class == "Synechococcus" & Site == "Bartlett"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_line(aes(x = Date, y = Abundance, colour = Class), size = 1.2)
             + geom_point(aes(x = Date, y = Abundance, colour = Class), size = 1.5)
             + scale_y_continuous(name = "cells/ml", labels = comma)
             + scale_x_date(date_breaks = "1 month", expand = c(0.025,0.015) , date_labels = "%b %Y", limits = c(min(algae.data.agg.combo$Date[algae.data.agg.combo$Site == "Bartlett"]), max(algae.data.agg.combo$Date[algae.data.agg.combo$Site == "Bartlett"])))
             + scale_colour_manual(values = cols)
             #+ ggtitle("Bartlett Algal Abundance")
             #+ facet_grid(~ when, scales = "free_x", space = "free")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 9, colour = "black", angle = 90, vjust = 0.5), 
                     axis.text.y = element_text(size = 10, colour = "black", margin = unit(c(0, 2, 0, 2), "mm")),
                     legend.position = "bottom",
                     plot.margin=unit(c(1, 6, 1, 6), units = "mm")  # add a little space to right side of plot
             )
  )
  
  
  Fig.10b <- (ggplot(data = algae.data.agg.combo[Site == "Bartlett" & (Class == "Chlorophytes" | Class == "Diatoms" | Class == "Prymnesiophytes")])
             + geom_ribbon(data = algae.data.agg.combo[Class == "Chlorophytes" & Site == "Bartlett"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_ribbon(data = algae.data.agg.combo[Class == "Diatoms" & Site == "Bartlett"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_ribbon(data = algae.data.agg.combo[Class == "Prymnesiophytes" & Site == "Bartlett"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_line(aes(x = Date, y = Abundance, colour = Class), size = 1.2)
             + geom_point(aes(x = Date, y = Abundance, colour = Class), size = 1.5)
             #+ scale_y_continuous(name = "cells/ml", labels = comma)
             + scale_y_continuous(name = "cells/ml", labels = comma, limits = c(0,50000)) 
             + scale_x_date(date_breaks = "1 month", expand = c(0.025,0.015) , date_labels = "%b %Y", limits = c(min(algae.data.agg.combo$Date[algae.data.agg.combo$Site == "Bartlett"]), max(algae.data.agg.combo$Date[algae.data.agg.combo$Site == "Bartlett"])))
             + scale_colour_manual(values = cols)
             #+ ggtitle("Bartlett Algal Abundance")
             #+ facet_grid(~ Class, scales = "free")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 9, colour = "black", angle = 90, vjust = 0.5), 
                     axis.text.y = element_text(size = 10, colour = "black", margin = unit(c(0, 2, 0, 2), "mm")),
                     legend.position = "bottom",
                     plot.margin=unit(c(1, 6, 1, 6), units = "mm")  # add a little space to right side of plot
             )
  )
  
  Fig.10a <- arrangeGrob(Fig.10a, top = textGrob("(a)", x = unit(1, "mm"), y  = unit(1, "mm"), just = c("left","top"),
                                                 gp = gpar(col = "black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
  
  Fig.10b <- arrangeGrob(Fig.10b, top = textGrob("(b)", x = unit(1, "mm") , y = unit(1, "mm"), just = c("left","top"),
                                                 gp = gpar(col = "black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
 
  Fig.10 <- grid.arrange(Fig.10a, Fig.10b, nrow = 2,
                        bottom = textGrob("Date", gp = gpar(fontface = "plain", fontfamily = "Times New Roman", fontsize = 14))
                        #,layout_matrix = lay
  )
}

# FIG 11 - Saguaro Abundance
{
  Fig.11a <- (ggplot(data = algae.data.agg.combo[Site == "Saguaro" & (Class == "Filamentous Cyanbacteria" | Class == "Synechococcus")])
             + geom_ribbon(data = algae.data.agg.combo[Class == "Filamentous Cyanbacteria" & Site == "Saguaro"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_ribbon(data = algae.data.agg.combo[Class == "Synechococcus" & Site == "Saguaro"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_line(aes(x = Date, y = Abundance, colour = Class), size = 1.2)
             + geom_point(aes(x = Date, y = Abundance, colour = Class), size = 1.5)
             + scale_y_continuous(name = "cells/ml", labels = comma)
             + scale_x_date(date_breaks = "1 month", expand = c(0.025,0.015) , date_labels = "%b %Y", limits = c(min(algae.data.agg.combo$Date[algae.data.agg.combo$Site == "Saguaro"]), max(algae.data.agg.combo$Date[algae.data.agg.combo$Site == "Saguaro"])))
             + scale_colour_manual(values = cols)
             #+ ggtitle("Saguaro Algal Abundance")
             #+ facet_grid(~ Class, scales = "free")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 9, colour = "black", angle = 90, vjust = 0.5),
                     axis.text.y = element_text(size = 10, colour = "black", margin = unit(c(0, 2, 0, 2), "mm")),
                     legend.position = "bottom",
                     plot.margin=unit(c(1, 6, 1, 6), units = "mm")  # add a little space to right side of plot
             )
  )
  
  Fig.11b <- (ggplot(data = algae.data.agg.combo[Site == "Saguaro" & (Class == "Chlorophytes" | Class == "Diatoms" | Class == "Prymnesiophytes")])
             + geom_ribbon(data = algae.data.agg.combo[Class == "Chlorophytes" & Site == "Saguaro"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_ribbon(data = algae.data.agg.combo[Class == "Diatoms" & Site == "Saguaro"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_ribbon(data = algae.data.agg.combo[Class == "Prymnesiophytes" & Site == "Saguaro"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_line(aes(x = Date, y = Abundance, colour = Class), size = 1.2)
             + geom_point(aes(x = Date, y = Abundance, colour = Class), size = 1.5)
             + scale_y_continuous(name = "cells/ml", labels = comma)
             + scale_x_date(date_breaks = "1 month", expand = c(0.025,0.015) , date_labels = "%b %Y", limits = c(min(algae.data.agg.combo$Date[algae.data.agg.combo$Site == "Saguaro"]), max(algae.data.agg.combo$Date[algae.data.agg.combo$Site == "Saguaro"])))
             + scale_colour_manual(values = cols)
             #+ ggtitle("Saguaro Algal Abundance")
             #+ facet_grid(~ Class, scales = "free")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 9, colour = "black", angle = 90, vjust = 0.5),
                     axis.text.y = element_text(size = 10, colour = "black", margin = unit(c(0, 2, 0, 2), "mm")),
                     legend.position = "bottom",
                     plot.margin=unit(c(1, 6, 1, 6), units = "mm")  # add a little space to right side of plot
             )
  )
  
  Fig.11a <- arrangeGrob(Fig.11a, top = textGrob("(a)", x = unit(1, "mm"), y  = unit(1, "mm"), just = c("left","top"),
                                               gp = gpar(col = "black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
  
  Fig.11b <- arrangeGrob(Fig.11b, top = textGrob("(b)", x = unit(1, "mm") , y = unit(1, "mm"), just = c("left","top"),
                                               gp = gpar(col = "black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
  
  Fig.11 <- grid.arrange(Fig.11a, Fig.11b, nrow = 2,
                        bottom = textGrob("Date", gp = gpar(fontface = "plain", fontfamily = "Times New Roman", fontsize = 14))
  )
}

# FIG 12 - Pleasant Abundance New + Old
{
  Fig.12a <- (ggplot(data = algae.data.agg.combo[Site == "Pleasant" & (Class == "Filamentous Cyanbacteria" | Class == "Synechococcus")])
             + geom_ribbon(data = algae.data.agg.combo[Class == "Filamentous Cyanbacteria" & Site == "Pleasant"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_ribbon(data = algae.data.agg.combo[Class == "Synechococcus" & Site == "Pleasant"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_line(aes(x = Date, y = Abundance, colour = Class), size = 1.2)
             + geom_point(aes(x = Date, y = Abundance, colour = Class), size = 1.5)
             + scale_y_continuous(name = "cells/ml", labels = comma)
             + scale_x_date(date_breaks = "1 month", expand = c(0.025,0.015) , date_labels = "%b %Y", limits = c(min(algae.data.agg.combo$Date[algae.data.agg.combo$Site == "Pleasant"]), max(algae.data.agg.combo$Date[algae.data.agg.combo$Site == "Pleasant"])))
             + scale_colour_manual(values = cols)
             #+ ggtitle("Pleasant Algal Abundance")
             #+ facet_grid(~ Class, scales = "free")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 9, colour = "black", angle = 90, vjust = 0.5),
                     axis.text.y = element_text(size = 10, colour = "black", margin = unit(c(0, 2, 0, 2), "mm")),
                     legend.position = "bottom",
                     plot.margin=unit(c(1, 6, 1, 6), units = "mm")  # add a little space to right side of plot
             )
  )
  
  Fig.12b <- (ggplot(data = algae.data.agg.combo[Site == "Pleasant" & (Class == "Chlorophytes" | Class == "Diatoms" | Class == "Prymnesiophytes")])
             + geom_ribbon(data = algae.data.agg.combo[Class == "Chlorophytes" & Site == "Pleasant"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_ribbon(data = algae.data.agg.combo[Class == "Diatoms" & Site == "Pleasant"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_ribbon(data = algae.data.agg.combo[Class == "Prymnesiophytes" & Site == "Pleasant"], aes(x = Date, ymin = abun_l95, ymax = abun_u95), fill = "grey90", inherit.aes = FALSE)
             + geom_line(aes(x = Date, y = Abundance, colour = Class), size = 1.2)
             + geom_point(aes(x = Date, y = Abundance, colour = Class), size = 1.5)
             + scale_y_continuous(name = "cells/ml", labels = comma)
             + scale_x_date(date_breaks = "1 month", expand = c(0.025,0.015) , date_labels = "%b %Y", limits = c(min(algae.data.agg.combo$Date[algae.data.agg.combo$Site == "Pleasant"]), max(algae.data.agg.combo$Date[algae.data.agg.combo$Site == "Pleasant"])))
             + scale_colour_manual(values = cols)
             #+ ggtitle("Pleasant Algal Abundance")
             #+ facet_grid(~ Class, scales = "free")
             + theme_minimal()
             + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                     plot.title = element_text(hjust = 0.47, size = 11),
                     axis.title.y = element_text(size = 13, face = "plain"),
                     axis.title.x = element_blank(),
                     axis.text.x = element_text(size = 9, colour = "black", angle = 90, vjust = 0.5),
                     axis.text.y = element_text(size = 10, colour = "black", margin = unit(c(0, 2, 0, 2), "mm")),
                     legend.position = "bottom",
                     plot.margin=unit(c(1, 6, 1, 6), units = "mm")  # add a little space to right side of plot
             )
             
  )
  
  Fig.12a <- arrangeGrob(Fig.12a, top = textGrob("(a)", x = unit(1, "mm"), y  = unit(1, "mm"), just = c("left","top"),
                                               gp = gpar(col = "black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
  
  Fig.12b <- arrangeGrob(Fig.12b, top = textGrob("(b)", x = unit(1, "mm") , y = unit(1, "mm"), just = c("left","top"),
                                               gp = gpar(col = "black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))
  
  Fig.12 <- grid.arrange(Fig.12a, Fig.12b, nrow = 2,
                        bottom = textGrob("Date", gp = gpar(fontface = "plain", fontfamily = "Times New Roman", fontsize = 14))
  )
  
}


# Fig 13+

# read old data / all data
#meta.old <- read.csv("X:/Dropbox (ASU)/Chris-Jaz/Landsat_data_all/20180529_all_old_data.csv", stringsAsFactors = F)
meta.old <- read.csv(here("outputs/all_extracted_sat_data.csv"), stringsAsFactors = F)
new.chloro <- read.csv(here("data/in-situ/chlorophyll_data.csv"), stringsAsFactors = F)
new.tss <- read.csv(here("data/in-situ/TSS_data.csv"), stringsAsFactors = F)

# remove unused objects
#rm(list = ls()[!ls() %in% c("meta.old","today")])

# create factors to order and rename for plotting
meta.old <- meta.old[meta.old$Site == 'B1' | meta.old$Site == 'P1' | meta.old$Site == 'S2',]

# adjust bands on Landsat 8
#meta.old$SRB1 <- ifelse(meta.old$Sat == 8, meta.old$SRB2, meta.old$SRB1)
#meta.old$SRB2 <- ifelse(meta.old$Sat == 8, meta.old$SRB3, meta.old$SRB2)
#meta.old$SRB3 <- ifelse(meta.old$Sat == 8, meta.old$SRB4, meta.old$SRB3)
#meta.old$SRB4 <- ifelse(meta.old$Sat == 8, meta.old$SRB5, meta.old$SRB4)
#meta.old$SRB5 <- ifelse(meta.old$Sat == 8, meta.old$SRB6, meta.old$SRB5)

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

# old formulas
#meta.old$sag_tss_est <- (1/129) * ((4 * sqrt(26875 * (meta.old$SRB1 / meta.old$SRB2) - 31408)) + 63)
#meta.old$sag_chl_est <- ((meta.old$SRB1/meta.old$SRB2) - 1.0889) / (-0.0123)
#meta.old$sag_tss_est <- ((meta.old$SRB1/meta.old$SRB2) - 1.222) / (-0.0996)
#meta.old$bar_chl_est <- ((meta.old$SRB3/meta.old$SRB1) - 0.7418) / (-0.0034)
#meta.old$bar_tss_est <- ((meta.old$SRB3/meta.old$SRB1) - 0.6015) / (-0.0399)
#meta.old$ple_chl_est <- ((meta.old$SRB4 - meta.old$SRB3) + 86.114) / (10.801)
#meta.old$sag_chl_est <- ((1/3) * sqrt(10000 * (meta.old$SRB1 / meta.old$SRB2) - 7810)) + 21
#meta.old$sag_tss_est <- (1/258) * (sqrt(1720000 * (meta.old$SRB1 / meta.old$SRB2) - 1383207) + 1099)
#meta.old$bar_chl_est <-  (1/2) * (sqrt(40000 * (meta.old$SRB3 / meta.old$SRB1) - 25271) + 91)
#meta.old$bar_tss_est <- (1/106) * (1013 - (sqrt(1182837 - (1060000 * meta.old$SRB3 / meta.old$SRB1))))
#meta.old$ple_chl_est <- (-2/895) * ((sqrt(5) * sqrt(2983923 - (89500 * (meta.old$SRB4 - meta.old$SRB3)))) - 7530)

#setDT(meta.old) # make into data.table for easier refrencing
#quantile(meta.old$chl_est[meta.old$Site == "Saguaro"], probs = c(0.05,0.95), na.rm = T)  #calc percentils

# add dots for actual values
#actual.val <- read.csv("Chl_bartlett.csv")



# first merge satellite data to sample data by nearest day 

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


Fig.13a <- (ggplot(data = meta.old[meta.old$Site == "Saguaro" & !is.na(meta.old$sag_chl_est) & !is.nan(meta.old$sag_chl_est) & meta.old$Sat == 7,]) #  
           #+ geom_smooth(aes(x = Date, y = sag_chl_est))
           + geom_line(aes(x = Date, y = sag_chl_est), size = .6)
           + geom_point(aes(x = Date, y = sag_chl_est), size = .8)
           + geom_point(data = merged.nearest[merged.nearest$Site == "S2" & merged.nearest$DayDif < 2,], aes(x = Date, y = Chlorophyll), size = 1.2, color = "red")
           + scale_y_continuous(name = "Chlorophyll Estimate (B/G) \n (?g/L)", limits = c(0, ceiling(max(meta.old$sag_chl_est, na.rm = T))))
           + scale_x_date(limits = c(min(meta.old$Date), max(meta.old$Date)))
           #+ ggtitle("")
           + theme_minimal()
           + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                   plot.title = element_text(hjust = 0.47, size = 11),
                   axis.title.y = element_text(size = 13, face = "plain"),
                   axis.title.x = element_blank(),
                   axis.text.x = element_text(size = 10, colour = "black"),
                   axis.text.y = element_text(size = 10, colour = "black"),
                   axis.line = element_line(size = .5, color = "black"),
                   plot.margin=unit(c(1, 5, 1, 1), units = "mm")  # add a little space to right side of plot
           )
)

Fig.13b <- (ggplot(data = meta.old[meta.old$Site == "Saguaro"  & !is.na(meta.old$sag_tss_est) & !is.nan(meta.old$sag_tss_est) & meta.old$Sat == 7,]) # 
            #+ geom_smooth(aes(x = Date, y = sag_tss_est))
            + geom_line(aes(x = Date, y = sag_tss_est), size = .6)
            + geom_point(aes(x = Date, y = sag_tss_est), size = .8)
            + geom_point(data = merged.nearest[merged.nearest$Site == "S2" & merged.nearest$DayDif < 2,], aes(x = Date, y = TSS), size = 1.2, color = "red")
            + scale_y_continuous(name = "TSS Estimate (B/G) \n (mg/mL)", limits = c(0, 10))
            + scale_x_date(limits = c(min(meta.old$Date), max(meta.old$Date)))
            #+ ggtitle("")
            + theme_minimal()
            + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                    plot.title = element_text(hjust = 0.47, size = 11),
                    axis.title.y = element_text(size = 13, face = "plain"),
                    axis.title.x = element_blank(),
                    axis.text.x = element_text(size = 10, colour = "black"),
                    axis.text.y = element_text(size = 10, colour = "black"),
                    axis.line = element_line(size = .5, color = "black"),
                    plot.margin=unit(c(1, 5, 1, 1), units = "mm")  # add a little space to right side of plot
            )
)

Fig.13a <- arrangeGrob(Fig.13a, top = textGrob("(a)", x = unit(5, "mm"), y   = unit(3, "mm"), just = c("left","top"),
                                             gp = gpar(col="black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))

Fig.13b <- arrangeGrob(Fig.13b, top = textGrob("(b)", x = unit(5, "mm"), y = unit(3, "mm"), just = c("left","top"),
                                             gp = gpar(col = "black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))

Fig.13 <- grid.arrange(Fig.13a, Fig.13b, nrow = 2,
                      bottom = textGrob("Date", gp = gpar(fontface = "plain", fontfamily = "Times New Roman", fontsize = 14))
                      )




#### fig 14

Fig.14a <- (ggplot(data = meta.old[meta.old$Site == "Bartlett"  & !is.na(meta.old$bar_chl_est) & !is.nan(meta.old$bar_chl_est) & meta.old$Sat == 7,]) #  & Sat == 7
            #+ geom_smooth(aes(x = Date, y = bar_chl_est))
            + geom_line(aes(x = Date, y = bar_chl_est), size = .6)
            + geom_point(aes(x = Date, y = bar_chl_est), size = .8)
            + geom_point(data = merged.nearest[merged.nearest$Site == "B1" & merged.nearest$DayDif < 2,], aes(x = Date, y = Chlorophyll), size = 1.2, color = "red")
            + scale_y_continuous(name = "Chlorophyll Estimate (R/B) \n (?g/L)", limits = c(floor(min(meta.old$bar_chl_est, na.rm = T)), ceiling(max(meta.old$bar_chl_est, na.rm = T))))
            + scale_x_date(limits = c(min(meta.old$Date), max(meta.old$Date)))
            #+ ggtitle("")
            + theme_minimal()
            + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                    plot.title = element_text(hjust = 0.47, size = 11),
                    axis.title.y = element_text(size = 13, face = "plain"),
                    axis.title.x = element_blank(),
                    axis.text.x = element_text(size = 10, colour = "black"),
                    axis.text.y = element_text(size = 10, colour = "black"),
                    axis.line = element_line(size = .5, color = "black"),
                    plot.margin=unit(c(1, 5, 1, 1), units = "mm")  # add a little space to right side of plot
            )
)

Fig.14b <- (ggplot(data = meta.old[meta.old$Site == "Bartlett"  & !is.na(meta.old$bar_tss_est) & !is.nan(meta.old$bar_tss_est) & meta.old$Sat == 7,]) #  & Sat == 7
            #+ geom_smooth(aes(x = Date, y = bar_tss_est))
            + geom_line(aes(x = Date, y = bar_tss_est), size = .6)
            + geom_point(aes(x = Date, y = bar_tss_est), size = .8)
            + geom_point(data = merged.nearest[merged.nearest$Site == "B1" & merged.nearest$DayDif < 2,], aes(x = Date, y = TSS), size = 1.2, color = "red")
            + scale_y_continuous(name = "TSS Estimate (R/B) \n (mg/mL)", limits = c(0, ceiling(max(meta.old$bar_tss_est, na.rm = T))))
            + scale_x_date(limits = c(min(meta.old$Date), max(meta.old$Date)))
            #+ ggtitle("")
            + theme_minimal()
            + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                    plot.title = element_text(hjust = 0.47, size = 11),
                    axis.title.y = element_text(size = 13, face = "plain"),
                    axis.title.x = element_blank(),
                    axis.text.x = element_text(size = 10, colour = "black"),
                    axis.text.y = element_text(size = 10, colour = "black"),
                    axis.line = element_line(size = .5, color = "black"),
                    plot.margin=unit(c(1, 5, 1, 1), units = "mm")  # add a little space to right side of plot
            )
)

Fig.14a <- arrangeGrob(Fig.14a, top = textGrob("(a)", x = unit(5, "mm"), y   = unit(3, "mm"), just = c("left","top"),
                                               gp = gpar(col="black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))

Fig.14b <- arrangeGrob(Fig.14b, top = textGrob("(b)", x = unit(5, "mm"), y = unit(3, "mm"), just = c("left","top"),
                                               gp = gpar(col = "black", fontsize = 14, fontface = "plain", fontfamily = "Times New Roman")))

Fig.14 <- grid.arrange(Fig.14a, Fig.14b, nrow = 2,
                       bottom = textGrob("Date", gp = gpar(fontface = "plain", fontfamily = "Times New Roman", fontsize = 14))
)



#### fig 15

Fig.15 <- (ggplot(data = meta.old[meta.old$Site == "Pleasant"  & !is.na(meta.old$ple_tss_est) & !is.nan(meta.old$ple_tss_est) & meta.old$Sat == 7,]) #  & Sat == 7
            #+ geom_smooth(aes(x = Date, y = ple_tss_est))
            + geom_line(aes(x = Date, y = ple_tss_est), size = .6)
            + geom_point(aes(x = Date, y = ple_tss_est), size = .8)
            + scale_y_continuous(name = "TSS Estimate (NIR/R) \n (?g/L)", limits = c(-1, ceiling(max(meta.old$ple_tss_est, na.rm = T))))
            + scale_x_date(limits = c(min(meta.old$Date), max(meta.old$Date)))
            #+ ggtitle("")
            + theme_minimal()
            + theme(text = element_text(colour = "black", size = 12, family = "Times New Roman"),
                    plot.title = element_text(hjust = 0.47, size = 11),
                    axis.title.y = element_text(size = 13, face = "plain"),
                    axis.title.x = element_blank(),
                    axis.text.x = element_text(size = 10, colour = "black"),
                    axis.text.y = element_text(size = 10, colour = "black"),
                    axis.line = element_line(size = .5, color = "black"),
                    plot.margin=unit(c(1, 5, 1, 1), units = "mm")  # add a little space to right side of plot
            )
)


ggsave(here("outputs/figures/Figure_1_Bartlett.tiff"), Fig.1, device = "tiff", scale = 1, width = 6.5, height = 7, dpi = 300, units = "in")
ggsave(here("outputs/figures/Figure_2_Saguaro.tiff"), Fig.2, device = "tiff", scale = 1, width = 6.5, height = 7, dpi = 300, units = "in")
ggsave(here("outputs/figures/Figure_3_Pleasant.tiff"), Fig.3, device = "tiff", scale = 1, width = 6.5, height = 7, dpi = 300, units = "in")
ggsave(here("outputs/figures/Figure_4_Bartlett.tiff"), Fig.4, device = "tiff", scale = 1, width = 6.5, height = 7, dpi = 300, units = "in")
ggsave(here("outputs/figures/Figure_5_Saguaro.tiff"), Fig.5, device = "tiff", scale = 1, width = 6.5, height = 7, dpi = 300, units = "in")
ggsave(here("outputs/figures/Figure_6_Pleasant.tiff"), Fig.6, device = "tiff", scale = 1, width = 6.5, height = 7, dpi = 300, units = "in")
ggsave(here("outputs/figures/Figure_7_Bartlett.tiff"), Fig.7, device = "tiff", scale = 1, width = 6.5, height = 3.5, dpi = 300, units = "in")
ggsave(here("outputs/figures/Figure_8_Saguaro.tiff"), Fig.8, device = "tiff", scale = 1, width = 6.5, height = 3.5, dpi = 300, units = "in")
ggsave(here("outputs/figures/Figure_9_Pleasant.tiff"), Fig.9, device = "tiff", scale = 1, width = 6.5, height = 3.5, dpi = 300, units = "in")
ggsave(here("outputs/figures/Figure_10_Bartlett.tiff"), Fig.10, device = "tiff", scale = 1, width = 6.5, height = 7, dpi = 300, units = "in")
ggsave(here("outputs/figures/Figure_11_Sagaro.tiff"), Fig.11, device = "tiff", scale = 1, width = 6.5, height = 7, dpi = 300, units = "in")
ggsave(here("outputs/figures/Figure_12_Pleasant.tiff"), Fig.12, device = "tiff", scale = 1, width = 6.5, height = 7, dpi = 300, units = "in")
ggsave(here("outputs/figures/Figure_13_Saguaro_estimates_new.tiff"),Fig.13, device = "tiff", scale = 1, width = 6.5, height = 7, dpi = 300, units = "in")
ggsave(here("outputs/figures/Figure_14_Bartlett_estimates_new.tiff"), Fig.14, device = "tiff", scale = 1, width = 6.5, height = 7, dpi = 300, units = "in")
ggsave(here("outputs/figures/Figure_15_Pleasant_estimate.tiff"),  Fig.15, device = "tiff", scale = 1, width = 6.5, height = 4, dpi = 300, units = "in")


