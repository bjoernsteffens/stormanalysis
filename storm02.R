

#
# Prepare the environment
rm(list=ls())
dev.off(dev.list())
setwd("~/mycloud/Private_DataScience/Coursera/10 Data Science Specialisation/40 Reproducible Research/Assignments/week4")

#
# Load the libraries we will be needing
library(RCurl)
library(data.table)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

#
# Global variables
stormDataURL    <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
stormDataFile   <- "resources_data/repdata_data_StormData.csv.bz2"
stormDwnLdDate  <- "resources_data/repdata_data_StormData.csv.bz2.DownLoadDate"

#
# Download the file if it is still available
# unless we have it on disk
if (url.exists(stormDataURL)) {

    #
    # Does the local file exist?
    if (!file.exists(stormDataFile)) {
    
        dir.create("resources_data")
        dir.create("resources_illustrations")
        
        download.file(stormDataURL,
                      method = "libcurl",
                      destfile = stormDataFile)
                    
        #
        # Stamp download date
        date_Downloaded = c("Date Downloaded", date())
        write.csv(date_Downloaded, file = stormDwnLdDate )
    }
}

#
# If for some reasons the data files should no longer be available at the original 
# location, copies can be found here.
# https://github.com/bjoernsteffens/stormanalysis

#
# Read the bz2 file into a data frame.
# fill = TRUE in case row lenght is not equal
stormData <- read.table(stormDataFile, 
                        header = TRUE, 
                        fill = TRUE, 
                        na.strings = "", sep = ",")

#
# Tranform the dates
stormData$BGN_DATE <- as.Date(stormData$BGN_DATE, "%m/%d/%Y %H:%M:%S")
stormData$BGN_YEAR <- as.POSIXlt(stormData$BGN_DATE)$year+1900
stormData$END_DATE <- as.Date(stormData$END_DATE, "%m/%d/%Y %H:%M:%S")
stormData$END_YEAR <- as.POSIXlt(stormData$END_DATE)$year+1900

#
# Fatality and Injury calculations
#
stormFatal <- stormData %>% filter(BGN_YEAR >= 1993) %>% group_by(EVTYPE) %>% summarize(Fatalities = sum(FATALITIES))
colnames(stormFatal) <- c("Storm Type", "Fatalities")
stormFatal <- as.data.frame(stormFatal)
head(arrange(stormFatal, desc(Fatalities)),10)

stormInjury <- stormData %>% filter(BGN_YEAR >= 1993) %>% group_by(EVTYPE) %>% summarize(Injuries = sum(INJURIES))
colnames(stormInjury) <- c("Storm Type", "Injuries")
stormInjury <- as.data.frame(stormInjury)
head(arrange(stormInjury, desc(Injuries)),10)

plotFatality <- head(arrange(stormFatal, desc(Fatalities)),10)
plotInjuries <- head(arrange(stormInjury, desc(Injuries)),10)

p1 <- ggplot(aes(x = `Storm Type`, y = Fatalities), data = plotFatality) + 
    geom_bar(fill = "dark blue", stat = "identity", alpha = 0.8) + 
    ggtitle("Fatalities") +
    ylab("Total Fatalities") +
    scale_x_discrete(limits=plotFatality$`Storm Type`) +
    geom_text(aes(label=round(Fatalities,0)), 
              position = position_dodge(width=0.9), 
              vjust=-.5, 
              color="black") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    theme(panel.background = element_rect(fill = "lightblue")) +
    theme(strip.background = element_rect(fill = "lightblue")) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_line(colour = "grey95")) +
    theme(axis.text.x = element_text(size=10,margin = margin(5,0,20,0))) +
    theme(axis.text.y = element_text(size=10,margin = margin(0,0,0,10))) +
    theme(plot.title = element_text(size = 14,margin = margin(15,15,30,15))) +
    #theme(axis.title.x = element_text(size = 16,margin = margin(15,15,30,15))) +
    theme(axis.title.x=element_blank()) +
    theme(axis.title.y = element_text(size = 12,margin = margin(15,15,30,15))) 
    #theme(plot.margin=unit(c(1,1,1,1),"cm"))


p2 <- ggplot(aes(x = `Storm Type`, y = Injuries), data = plotInjuries) + 
    geom_bar(fill = "dark blue", stat = "identity", alpha = 0.8) + 
    ggtitle("Injuries") +
    xlab("Type") + 
    ylab("Total Injuries") +
    scale_x_discrete(limits=plotInjuries$`Storm Type`) +
    geom_text(aes(label=round(Injuries,0)), 
              position = position_dodge(width=0.9), 
              vjust=-.5, 
              color="black") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    theme(panel.background = element_rect(fill = "lightblue")) +
    theme(strip.background = element_rect(fill = "lightblue")) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_line(colour = "grey95")) +
    theme(axis.text.x = element_text(size=10,margin = margin(5,0,20,0))) +
    theme(axis.text.y = element_text(size=10,margin = margin(0,0,0,10))) +
    theme(plot.title = element_text(size = 12,margin = margin(15,15,30,15))) +
    #theme(axis.title.x = element_text(size = 16,margin = margin(15,15,30,15))) +
    theme(axis.title.x=element_blank()) +
    theme(axis.title.y = element_text(size = 14,margin = margin(15,15,30,15))) 
    #theme(plot.margin=unit(c(1,1,1,1),"cm")) 

png(filename = "resources_illustrations/event_impact_fatalinjury.png", width = 960, height = 1440)
grid.arrange(p1,p2, nrow = 2, 
            top = textGrob("Top 10 Most Dangerous Weather Events for the Population",gp=gpar(fontsize=24,font=2)))
dev.off()

#
# CropPropCalculations
#

# I want to separete property damages from crop damages. A weather event can be
# damaging to crop but not necessarily to properties and vice versa depending
# on where it takes place.

#
# Filter and select
stormProp <- stormData[with(stormData, grepl("[012345678bBhHkKmM]", stormData$PROPDMGEXP)),]
stormCrop <- stormData[with(stormData, grepl("[bBkKmM]", stormData$CROPDMGEXP)),]

#
# Filter out data before 1993
stormProp <- stormProp[with(stormProp, BGN_YEAR >= 1993),]
stormCrop <- stormCrop[with(stormCrop, BGN_YEAR >= 1993),]

#
# Transform PROPDMGEXP->PROPDMGEXPVAL
stormProp$PROPDMGEXPVAL[stormProp$PROPDMGEXP %in% "0"] <- 1e0
stormProp$PROPDMGEXPVAL[stormProp$PROPDMGEXP %in% "1"] <- 1e1
stormProp$PROPDMGEXPVAL[stormProp$PROPDMGEXP %in% "2"] <- 1e2
stormProp$PROPDMGEXPVAL[stormProp$PROPDMGEXP %in% "3"] <- 1e3
stormProp$PROPDMGEXPVAL[stormProp$PROPDMGEXP %in% "4"] <- 1e4
stormProp$PROPDMGEXPVAL[stormProp$PROPDMGEXP %in% "5"] <- 1e5
stormProp$PROPDMGEXPVAL[stormProp$PROPDMGEXP %in% "6"] <- 1e6
stormProp$PROPDMGEXPVAL[stormProp$PROPDMGEXP %in% "7"] <- 1e7
stormProp$PROPDMGEXPVAL[stormProp$PROPDMGEXP %in% "8"] <- 1e8
stormProp$PROPDMGEXPVAL[stormProp$PROPDMGEXP %in% "b" | stormProp$PROPDMGEXP %in% "B"] <- 1e9
stormProp$PROPDMGEXPVAL[stormProp$PROPDMGEXP %in% "h" | stormProp$PROPDMGEXP %in% "H"] <- 1e2
stormProp$PROPDMGEXPVAL[stormProp$PROPDMGEXP %in% "k" | stormProp$PROPDMGEXP %in% "K"] <- 1e3
stormProp$PROPDMGEXPVAL[stormProp$PROPDMGEXP %in% "m" | stormProp$PROPDMGEXP %in% "M"] <- 1e6

#
# Transform CROPDMGEXP->CROPDMGEXPVAL
stormCrop$CROPDMGEXPVAL[stormCrop$CROPDMGEXP %in% "b" | stormCrop$CROPDMGEXP %in% "B"] <- 1e9
stormCrop$CROPDMGEXPVAL[stormCrop$CROPDMGEXP %in% "k" | stormCrop$CROPDMGEXP %in% "K"] <- 1e3
stormCrop$CROPDMGEXPVAL[stormCrop$CROPDMGEXP %in% "m" | stormCrop$CROPDMGEXP %in% "M"] <- 1e6


#
# Prepare the Property Damage plot
stormProgDmg <- stormProp %>% group_by(EVTYPE) %>% summarize(Damages = sum(PROPDMG*as.numeric(PROPDMGEXPVAL)))
colnames(stormProgDmg) <- c("Storm Type", "Damages")
stormProgDmg <- as.data.frame(stormProgDmg)
plotstormPropDmg <- head(arrange(stormProgDmg, desc(Damages)),10)

p1 <- ggplot(aes(x = `Storm Type`, y = Damages), data = plotstormPropDmg) + 
    geom_bar(fill = "dark blue", stat = "identity", alpha = 0.8) + 
    ggtitle("Estimated Property Damages in USD") +
    ylab("Total Estimated Damages") +
    scale_x_discrete(limits=plotstormPropDmg$`Storm Type`) +
    geom_text(aes(label=round(Damages,0)), 
              position = position_dodge(width=0.9), 
              vjust=-.5, 
              color="black") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    theme(panel.background = element_rect(fill = "lightblue")) +
    theme(strip.background = element_rect(fill = "lightblue")) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_line(colour = "grey95")) +
    theme(axis.text.x = element_text(size=10,margin = margin(5,0,20,0))) +
    theme(axis.text.y = element_text(size=10,margin = margin(0,0,0,10))) +
    theme(plot.title = element_text(size = 14,margin = margin(15,15,30,15))) +
    #theme(axis.title.x = element_text(size = 16,margin = margin(15,15,30,15))) +
    theme(axis.title.x=element_blank()) +
    theme(axis.title.y = element_text(size = 12,margin = margin(15,15,30,15))) 
    #theme(plot.margin=unit(c(1,1,1,1),"cm"))

#
# Prepare the Crop Damage plot
stormCropDmg <- stormCrop %>% group_by(EVTYPE) %>% summarize(Damages = sum(CROPDMG*as.numeric(CROPDMGEXPVAL)))
colnames(stormCropDmg) <- c("Storm Type", "Damages")
stormCropDmg <- as.data.frame(stormCropDmg)
plotstormCropDmg <- head(arrange(stormCropDmg, desc(Damages)),10)

p2 <- ggplot(aes(x = `Storm Type`, y = Damages), data = plotstormCropDmg) + 
    geom_bar(fill = "dark blue", stat = "identity", alpha = 0.8) + 
    ggtitle("Estimated Crop Damages in USD") +
    ylab("Total Estimated Damages") +
    scale_x_discrete(limits=plotstormCropDmg$`Storm Type`) +
    geom_text(aes(label=round(Damages,0)), 
              position = position_dodge(width=0.9), 
              vjust=-.5, 
              color="black") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    theme(panel.background = element_rect(fill = "lightblue")) +
    theme(strip.background = element_rect(fill = "lightblue")) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_line(colour = "grey95")) +
    theme(axis.text.x = element_text(size=10,margin = margin(5,0,20,0))) +
    theme(axis.text.y = element_text(size=10,margin = margin(0,0,0,10))) +
    theme(plot.title = element_text(size = 14,margin = margin(15,15,30,15))) +
    #theme(axis.title.x = element_text(size = 16,margin = margin(15,15,30,15))) +
    theme(axis.title.x=element_blank()) +
    theme(axis.title.y = element_text(size = 12,margin = margin(15,15,30,15))) 
    #theme(plot.margin=unit(c(1,1,1,1),"cm"))


png(filename = "resources_illustrations/event_impact_propcrop.png", width = 960, height = 1440)
grid.arrange(p1,p2, nrow = 2,
             top = textGrob("Top 10 'Inconvenient' Weather Events for Insurance & Farming Industries",gp=gpar(fontsize=24,font=2)))
dev.off()
