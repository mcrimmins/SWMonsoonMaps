# create monsoon climo maps for AZ and/or NM 
# adapted from monsoonExtentTS.R and plotMonsoonPRISM.R
# MAC 05/18/2020
# update data with downloadDailyPRISM_monsoon.R

library(raster)
# library(plyr)
# library(tidyr)
# library(cowplot)
# library(caTools)

# set rasteroptions
rasterOptions(progress = 'text')

us <- getData("GADM", country="USA", level=1)
# extract states (need to uppercase everything)
# state<-subset(us, NAME_1=="New Mexico")
state<-subset(us, NAME_1=="Arizona")
#state<-subset(us, NAME_1 %in% c("New Mexico","Arizona"))


# precip threshold
thresh<-0.01

gridStack<-list()
gridStackSum<-list()
i<-1

for(year in 1991:2020){
  currYear<-stack(paste0("/home/crimmins/RProjects/ClimPlot/monsoonDailyData/AZNM_PRISM_Monsoon_",year,"_dailyPrecip.grd"))
  
  gridStack[[i]] <- calc(currYear, fun=function(x){sum(x >= thresh, na.rm = TRUE)})
  gridStackSum[[i]] <- calc(currYear, fun=function(x){sum(x, na.rm = TRUE)})
  
  i<-i+1
  print(year)
}
# combine into stack
rainDays = stack(gridStack)
sumPrecip=stack(gridStackSum)

meanRainDays<-round(calc(rainDays, mean),0)  
meanRainDays[meanRainDays <= 0] <- NA

meanPrecip<-calc(sumPrecip,mean)
meanPrecip[meanPrecip<=0]<-NA

#percAvg<-(rainDays[[39]]/meanRainDays)*100

# PLOT MAP
library(ggplot2)
library(PBSmapping)
library(rgdal)
library(readr)
library(rasterVis)
library(scales)
library(magick)
# get shapefiles
all_states <- map_data("state")
all_counties<-map_data("county")
tribes <- readOGR("/home/crimmins/RProjects/ClimPlot/tribes", "indlanp020")
# load cities
SWCities <- read_csv("/home/crimmins/RProjects/ClimPlot/SWCities.csv")
# load map boundary data
latN<-37.1;latS<-31
lonE<--108.9; lonW<--115
xlim = c(lonW,lonE)
ylim = c(latS,latN)
# state boundaries
colnames(all_states)<-c("X","Y","PID","POS","region","subregion")
states= clipPolys(all_states, xlim=xlim,ylim=ylim, keepExtra=TRUE)
# county boundaries
colnames(all_counties)<-c("X","Y","PID","POS","region","subregion")
all_states = clipPolys(all_counties, xlim=xlim,ylim=ylim, keepExtra=TRUE)
# tribal boundaries
tribes_df <- fortify(tribes)
#colnames(tribes_df)<-c("X","Y","POS","hole","piece","id","PID")
#tribes_df = clipPolys(tribes_df, xlim=xlim,ylim=ylim, keepExtra=TRUE)


# RAIN DAYS Percent Map -----
# colorramp for total precip
precipCols<-colorRampPalette(c("yellow2", "green", "blue","red","orange"))(50)
precBreaks<-seq(0,50,5)
precLabs<-as.character(seq(0,50,5))
precLabs[11]<-"50"
precLabs[1]<-"0"
#precBreaksmin<-seq(1,19,2)

#theme_set(theme_bw())
p<-gplot(meanRainDays) + geom_tile(aes(fill = value)) +
  #scale_fill_gradient2(low = 'white', high = 'blue') +
  #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod", 
  #                     name="inches", limits=c(0,20),oob=squish)+
  
  scale_fill_gradientn(colours = precipCols, na.value="darkgoldenrod", 
                       name="Days", limits=c(0,50),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
  guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+
  
  coord_equal(xlim = c(lonW,lonE), ylim = c(latS,latN), expand = FALSE)+
  xlab("Longitude") + ylab("Latitude") 

p<-p +  geom_polygon( data=all_states, aes(x=X, y=Y, group = PID),colour="black", fill=NA, size=0.25 )+
  #scale_x_continuous(breaks = c(-120,-140))+
  #ggtitle("Daily Total Precip (inches) - PRISM")+
  ggtitle(paste0("Average number of days with rain (>0.01 in): June 15th - Sept. 30th (1991-2020 period) "))+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group\nRCC-ACIS"))+
  theme(plot.title=element_text(size=15, face = "bold"))

p<-p+geom_path(data = tribes_df, 
               aes(x = long, y = lat, group = group),
               color = 'azure4', size = .2)

p<-p+geom_point(data = SWCities, aes(x = lon, y = lat), size = 1, 
                shape = 20)

p<-p+geom_text(data = SWCities, aes(x = lon, y = lat, label = City), 
               size = 5, col = "black", fontface = "bold", nudge_y = 0.1)

# write out file
png("/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/AZMonsoon_MeanRainDays_1991-2020.png", width = 10, height = 10, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

# add logos
# Call back the plot
plot <- image_read("/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/AZMonsoon_MeanRainDays_1991-2020.png")
# And bring in a logo
#logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
logo <- image_resize(logo_raw, geometry_size_percent(width=120,height = 120))
# Stack them on top of each other
#final_plot <- image_append((c(plot, logo)), stack = TRUE)
#final_plot <- image_mosaic((c(plot, logo)))
final_plot <- image_composite(plot, logo, offset = "+310+2760")
# And overwrite the plot without a logo
image_write(final_plot, "/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/AZMonsoon_MeanRainDays_1991-2020.png")
#####

# RAIN DAYS Percent Map -----
# colorramp for total precip
precipCols<-colorRampPalette(c("yellow2", "green", "blue","red","orange"))(50)
precBreaks<-seq(0,50,5)
precLabs<-as.character(seq(0,50,5))
precLabs[11]<-"50"
precLabs[1]<-"0"
#precBreaksmin<-seq(1,19,2)

#theme_set(theme_bw())
p<-gplot(rainDays[[39]]) + geom_tile(aes(fill = value)) +
  #scale_fill_gradient2(low = 'white', high = 'blue') +
  #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod", 
  #                     name="inches", limits=c(0,20),oob=squish)+
  
  scale_fill_gradientn(colours = precipCols, na.value="darkgoldenrod", 
                       name="Days", limits=c(0,50),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
  guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+
  
  coord_equal(xlim = c(lonW,lonE), ylim = c(latS,latN), expand = FALSE)+
  xlab("Longitude") + ylab("Latitude") 

p<-p +  geom_polygon( data=all_states, aes(x=X, y=Y, group = PID),colour="black", fill=NA, size=0.25 )+
  #scale_x_continuous(breaks = c(-120,-140))+
  #ggtitle("Daily Total Precip (inches) - PRISM")+
  ggtitle(paste0("Number of days with rain (>0.01 in) in 2019: June 15th - Sept. 30th"))+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group\nRCC-ACIS"))+
  theme(plot.title=element_text(size=15, face = "bold"))

p<-p+geom_path(data = tribes_df, 
               aes(x = long, y = lat, group = group),
               color = 'azure4', size = .2)

p<-p+geom_point(data = SWCities, aes(x = lon, y = lat), size = 1, 
                shape = 20)

p<-p+geom_text(data = SWCities, aes(x = lon, y = lat, label = City), 
               size = 5, col = "black", fontface = "bold", nudge_y = 0.1)

# write out file
png("/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/AZMonsoon_2019TotalRainDays.png", width = 10, height = 10, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

# add logos
# Call back the plot
plot <- image_read("/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/AZMonsoon_2019TotalRainDays.png")
# And bring in a logo
#logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
logo <- image_resize(logo_raw, geometry_size_percent(width=120,height = 120))
# Stack them on top of each other
#final_plot <- image_append((c(plot, logo)), stack = TRUE)
#final_plot <- image_mosaic((c(plot, logo)))
final_plot <- image_composite(plot, logo, offset = "+310+2760")
# And overwrite the plot without a logo
image_write(final_plot, "/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/AZMonsoon_2019TotalRainDays.png")
#####

# SINGLE YEAR % of avg number of rain days
# RAIN DAYS Percent Map -----
# colorramp for total precip
precipCols<-colorRampPalette(c("red","orange","white","green","blue"))(50)
precBreaks<-seq(0,200,25)
precLabs<-as.character(seq(0,200,25))
precLabs[9]<-"200"
precLabs[1]<-"0"
#precBreaksmin<-seq(1,19,2)

#theme_set(theme_bw())
p<-gplot(percAvg) + geom_tile(aes(fill = value)) +
  #scale_fill_gradient2(low = 'white', high = 'blue') +
  #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod", 
  #                     name="inches", limits=c(0,20),oob=squish)+
  
  scale_fill_gradientn(colours = precipCols, na.value="darkgoldenrod", 
                       name="% of avg", limits=c(0,200),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
  guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+
  
  coord_equal(xlim = c(lonW,lonE), ylim = c(latS,latN), expand = FALSE)+
  xlab("Longitude") + ylab("Latitude") 

p<-p +  geom_polygon( data=all_states, aes(x=X, y=Y, group = PID),colour="black", fill=NA, size=0.25 )+
  #scale_x_continuous(breaks = c(-120,-140))+
  #ggtitle("Daily Total Precip (inches) - PRISM")+
  ggtitle(paste0("Percent of average days with rain (>0.01 in) in 2019 (6/15-9/30, 1981-2019 base period) "))+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group\nRCC-ACIS"))+
  theme(plot.title=element_text(size=15, face = "bold"))

p<-p+geom_path(data = tribes_df, 
               aes(x = long, y = lat, group = group),
               color = 'azure4', size = .2)

p<-p+geom_point(data = SWCities, aes(x = lon, y = lat), size = 1, 
                shape = 20)

p<-p+geom_text(data = SWCities, aes(x = lon, y = lat, label = City), 
               size = 5, col = "black", fontface = "bold", nudge_y = 0.1)

# write out file
png("/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/AZMonsoon_2019RainDays.png", width = 10, height = 10, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

# add logos
# Call back the plot
plot <- image_read("/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/AZMonsoon_2019RainDays.png")
# And bring in a logo
#logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
logo <- image_resize(logo_raw, geometry_size_percent(width=120,height = 120))
# Stack them on top of each other
#final_plot <- image_append((c(plot, logo)), stack = TRUE)
#final_plot <- image_mosaic((c(plot, logo)))
final_plot <- image_composite(plot, logo, offset = "+310+2760")
# And overwrite the plot without a logo
image_write(final_plot, "/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/AZMonsoon_2019RainDays.png")
#####



# monthly long-term averages, monthly PRISM
# use downloadPRISM.R 

# CREATE CLIMATOLOGIES ----
# monthly values
JJASppt<-stack("/home/crimmins/RProjects/ClimPlot/PRISM/JJASppt.grd")
JJASppt<-JJASppt/25.4


# colorramp for total precip
precipCols<-colorRampPalette(c("lightblue", "dodgerblue3", "palegreen","green4","salmon","orangered3",
                               "lightgoldenrod1","orange2","plum2","purple"))(50)
precBreaks<-seq(0,9,1)
precLabs<-as.character(seq(0,9,1))
precLabs[10]<-">9"
precLabs[1]<-"0.01"
#precBreaksmin<-seq(1,19,2)

#theme_set(theme_bw())
names(JJASppt)<-c("June","July","August","September")
p<-gplot(JJASppt) + geom_tile(aes(fill = value)) +
  #scale_fill_gradient2(low = 'white', high = 'blue') +
  #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod",
  #                     name="inches", limits=c(0,20),oob=squish)+
  facet_wrap(~ variable)+
  scale_fill_gradientn(colours = precipCols, na.value="darkgoldenrod",
                       name="inches", limits=c(0,9),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
  guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+

  coord_equal(xlim = c(lonW,lonE), ylim = c(latS,latN), expand = FALSE)+
  xlab("Longitude") + ylab("Latitude")

p<-p +  geom_polygon( data=all_states, aes(x=X, y=Y, group = PID),colour="black", fill=NA, size=0.25 )+
  #scale_x_continuous(breaks = c(-120,-140))+
  #ggtitle("Daily Total Precip (inches) - PRISM")+
  ggtitle("Monthly Average Precipitation (1981-2010)")+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group"))+
  theme(plot.title=element_text(size=14, face = "bold"))+
  theme(strip.text.x = element_text(size = 12))

p<-p+geom_path(data = tribes_df,
               aes(x = long, y = lat, group = group),
               color = 'azure4', size = .2)

p<-p+geom_point(data = SWCities, aes(x = lon, y = lat), size = 1,
                shape = 20)

p<-p+geom_text(data = SWCities, aes(x = lon, y = lat, label = City),
               size = 3, col = "black", fontface = "bold", nudge_y = 0.15)

# write out file
png("/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/AZ_Monsoon_JJAS_Precip_Climatology.png", width = 10, height = 10, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

# add logos
# Call back the plot
plot <- image_read("/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/AZ_Monsoon_JJAS_Precip_Climatology.png")
# And bring in a logo
#logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png")
logo <- image_resize(logo_raw, geometry_size_percent(width=100,height = 100))
# Stack them on top of each other
#final_plot <- image_append((c(plot, logo)), stack = TRUE)
#final_plot <- image_mosaic((c(plot, logo)))
final_plot <- image_composite(plot, logo, offset = "+210+2780")
# And overwrite the plot without a logo
image_write(final_plot, "/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/AZ_Monsoon_JJAS_Precip_Climatology.png")

# Seasonal TOTAL
# colorramp for total precip
totalJJAS<-calc(JJASppt, sum, na.rm=TRUE)

# or from Dailies 6/15-9/30
totalJJAS<-meanPrecip

precipCols<-colorRampPalette(c("lightblue", "dodgerblue3", "palegreen","green4","salmon","orangered3",
                               "lightgoldenrod1","orange2","plum2","purple"))(50)
# precBreaks<-seq(0,20,2)
# precLabs<-as.character(seq(0,20,2))
# precLabs[11]<-">20"
# precLabs[1]<-"0.01"

precBreaks<-seq(0,18,2)
precLabs<-as.character(seq(0,18,2))
precLabs[10]<-">18"
precLabs[1]<-"0.01"

#precBreaksmin<-seq(1,19,2)

#theme_set(theme_bw())
p<-gplot(totalJJAS) + geom_tile(aes(fill = value)) +
  #scale_fill_gradient2(low = 'white', high = 'blue') +
  #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod",
  #                     name="inches", limits=c(0,20),oob=squish)+
  scale_fill_gradientn(colours = precipCols, na.value="darkgoldenrod",
                       name="inches", limits=c(0,18),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
  guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+

  coord_equal(xlim = c(lonW,lonE), ylim = c(latS,latN), expand = FALSE)+
  xlab("Longitude") + ylab("Latitude")

p<-p +  geom_polygon( data=all_states, aes(x=X, y=Y, group = PID),colour="black", fill=NA, size=0.25 )+
  #scale_x_continuous(breaks = c(-120,-140))+
  #ggtitle("Daily Total Precip (inches) - PRISM")+
  ggtitle("Average Seasonal (Jun 15 - Sept 30th) Total Precipitation (1991-2020)")+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group"))+
  theme(plot.title=element_text(size=14, face = "bold"))

p<-p+geom_path(data = tribes_df,
               aes(x = long, y = lat, group = group),
               color = 'azure4', size = .2)

p<-p+geom_point(data = SWCities, aes(x = lon, y = lat), size = 1,
                shape = 20)

p<-p+geom_text(data = SWCities, aes(x = lon, y = lat, label = City),
               size = 5, col = "black", fontface = "bold", nudge_y = 0.1)

# write out file
png("/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/AZ_Monsoon_Seasonal_Precip_Climatology_1991-2020.png", width = 10, height = 10, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

# add logos
# Call back the plot
plot <- image_read("/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/AZ_Monsoon_Seasonal_Precip_Climatology_1991-2020.png")
# And bring in a logo
#logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png")
logo <- image_resize(logo_raw, geometry_size_percent(width=100,height = 100))
# Stack them on top of each other
#final_plot <- image_append((c(plot, logo)), stack = TRUE)
#final_plot <- image_mosaic((c(plot, logo)))
final_plot <- image_composite(plot, logo, offset = "+210+2780")
# And overwrite the plot without a logo
image_write(final_plot, "/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/AZ_Monsoon_Seasonal_Precip_Climatology_1991-2020.png")

# monsoon season percent of annual total
ANNppt<-stack("/home/crimmins/RProjects/ClimPlot/PRISM/ANNppt.grd")
ANNppt<-ANNppt/25.4
totalANN<-calc(ANNppt, sum, na.rm=TRUE)
percMonsoon<-round((totalJJAS/totalANN)*100,0)

precipCols<-colorRampPalette(c("saddlebrown", "orange4","orange1", "navajowhite1","royalblue1","royalblue4","navy"))(50)
precBreaks<-seq(10,70,5)
precLabs<-as.character(seq(10,70,5))
precLabs[13]<-"70"
precLabs[1]<-"10"
#precBreaksmin<-seq(1,19,2)

#theme_set(theme_bw())
p<-gplot(percMonsoon) + geom_tile(aes(fill = value)) +
  #scale_fill_gradient2(low = 'white', high = 'blue') +
  #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod",
  #                     name="inches", limits=c(0,20),oob=squish)+
  scale_fill_gradientn(colours = precipCols, na.value="darkgoldenrod",
                       name="%", limits=c(10,70),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
  guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+
  
  coord_equal(xlim = c(lonW,lonE), ylim = c(latS,latN), expand = FALSE)+
  xlab("Longitude") + ylab("Latitude")

p<-p +  geom_polygon( data=all_states, aes(x=X, y=Y, group = PID),colour="black", fill=NA, size=0.25 )+
  #scale_x_continuous(breaks = c(-120,-140))+
  #ggtitle("Daily Total Precip (inches) - PRISM")+
  ggtitle("Percent of Average Annual Precipitation in Jun-Jul-Aug-Sep (1981-2010)")+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group"))+
  theme(plot.title=element_text(size=14, face = "bold"))

p<-p+geom_path(data = tribes_df,
               aes(x = long, y = lat, group = group),
               color = 'azure4', size = .2)

p<-p+geom_point(data = SWCities, aes(x = lon, y = lat), size = 1,
                shape = 20)

p<-p+geom_text(data = SWCities, aes(x = lon, y = lat, label = City),
               size = 5, col = "black", fontface = "bold", nudge_y = 0.1)

# write out file
png("/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/AZ_Percent_Monsoon_Seasonal_Precip_Climatology.png", width = 10, height = 10, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

# add logos
# Call back the plot
plot <- image_read("/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/AZ_Percent_Monsoon_Seasonal_Precip_Climatology.png")
# And bring in a logo
#logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png")
logo <- image_resize(logo_raw, geometry_size_percent(width=100,height = 100))
# Stack them on top of each other
#final_plot <- image_append((c(plot, logo)), stack = TRUE)
#final_plot <- image_mosaic((c(plot, logo)))
final_plot <- image_composite(plot, logo, offset = "+210+2780")
# And overwrite the plot without a logo
image_write(final_plot, "/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/AZ_Percent_Monsoon_Seasonal_Precip_Climatology.png")


# lightning data for Paul
latlon<-raster("./monsoonClimo_maps/latlon.nc")
#plot(latlon)
#plot(totalJJAS, add=TRUE)

totalJJAS<-crop(totalJJAS, extent(latlon))
totalJJAS<-resample(totalJJAS, latlon)

writeRaster(totalJJAS, filename="/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/JJASPrecip_PRISM_81_10.nc", format="CDF",overwrite=TRUE)

# 2000-19 period
# get PRISM data
library(prism)
options(prism.path = "/scratch/crimmins/PRISM/monthly/precip")
files<-ls_prism_data(absPath=T)[,2]

dates<-as.data.frame(seq(as.Date("2000-01-01"),as.Date("2019-10-01"), by="months"))
  colnames(dates)<-"date"
#subdates<-as.data.frame(dates[1261:1498])
# subdates$month<-as.numeric(format(subdates$`dates[1261:1498]`,"%m"))
dates$month<-as.numeric(format(dates$date,"%m"))
  
moPrecip<-stack(files[1263:1500])
moPrecip<-crop(moPrecip, extent(latlon))

moPrecipAvg <- stackApply(moPrecip, indices=subdates$month, fun=sum, na.rm=TRUE) 

moPrecipAvg<-moPrecipAvg/((2019-2000)+1)

totalJJAS<-sum(moPrecipAvg[[6:9]])
totalJJAS<-resample(totalJJAS, latlon)

writeRaster(totalJJAS, filename="/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/JJASPrecip_PRISM_00_19.nc", format="CDF",overwrite=TRUE)
