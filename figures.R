
#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#
#$%   J. B. Russell Thesis Figures   $%#
#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#$%#

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

# FIG 10 - Bartlett Abundance New + Old
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


# Fig 13

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


