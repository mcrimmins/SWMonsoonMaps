# create monsoon climo maps for AZ AND NM 
# adapted from monsoonExtentTS.R and plotMonsoonPRISM.R, monsoonClimo_maps.R
# MAC 05/18/2020
# update data with downloadDailyPRISM_monsoon.R

library(raster)
library(ggplot2)
library(PBSmapping)
library(rgdal)
library(readr)
library(rasterVis)
library(scales)
library(magick)

# set rasteroptions
rasterOptions(progress = 'text')

# LOAD NORMALS
# monthly values
JJASppt<-stack("/home/crimmins/RProjects/ClimPlot/PRISM/JJASppt.grd")
JJASppt<-JJASppt/25.4

# PLOT MAP
# get shapefiles
all_states <- map_data("state")
all_counties<-map_data("county")
tribes <- readOGR("/home/crimmins/RProjects/ClimPlot/tribes", "indlanp020")
# load cities
SWCities <- read_csv("/home/crimmins/RProjects/ClimPlot/SWCities.csv")
# load map boundary data
latN<-37.1;latS<-31
lonE<--102; lonW<--115
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
  ggtitle("Monthly Average Precipitation (1991-2020)")+
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
png("/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/SW_Monsoon_JJAS_Precip_Climatology.png", width = 16, height = 8, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

# add logos
# Call back the plot
plot <- image_read("/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/SW_Monsoon_JJAS_Precip_Climatology.png")
# And bring in a logo
#logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png")
logo <- image_resize(logo_raw, geometry_size_percent(width=100,height = 100))
# Stack them on top of each other
#final_plot <- image_append((c(plot, logo)), stack = TRUE)
#final_plot <- image_mosaic((c(plot, logo)))
final_plot <- image_composite(plot, logo, offset = "+410+2170")
# And overwrite the plot without a logo
image_write(final_plot, "/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/SW_Monsoon_JJAS_Precip_Climatology.png")

# Seasonal TOTAL
# colorramp for total precip
totalJJAS<-calc(JJASppt, sum, na.rm=TRUE)

# or from Dailies 6/15-9/30
#totalJJAS<-meanPrecip

precipCols<-colorRampPalette(c("lightblue", "dodgerblue3", "palegreen","green4","salmon","orangered3",
                               "lightgoldenrod1","orange2","plum2","purple"))(50)
 precBreaks<-seq(0,20,2)
 precLabs<-as.character(seq(0,20,2))
 precLabs[11]<-">20"
 precLabs[1]<-"0.01"

# precBreaks<-seq(0,18,2)
# precLabs<-as.character(seq(0,18,2))
# precLabs[10]<-">18"
# precLabs[1]<-"0.01"

#precBreaksmin<-seq(1,19,2)

#theme_set(theme_bw())
p<-gplot(totalJJAS) + geom_tile(aes(fill = value)) +
  #scale_fill_gradient2(low = 'white', high = 'blue') +
  #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod",
  #                     name="inches", limits=c(0,20),oob=squish)+
   scale_fill_gradientn(colours = precipCols, na.value="darkgoldenrod",
                        name="inches", limits=c(0,20),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
  guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+
  
  coord_equal(xlim = c(lonW,lonE), ylim = c(latS,latN), expand = FALSE)+
  xlab("Longitude") + ylab("Latitude")

p<-p +  geom_polygon( data=all_states, aes(x=X, y=Y, group = PID),colour="black", fill=NA, size=0.25 )+
  #scale_x_continuous(breaks = c(-120,-140))+
  #ggtitle("Daily Total Precip (inches) - PRISM")+
  ggtitle("Average Seasonal (Jun-Jul-Aug-Sep) Total Precipitation (1991-2020)")+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group"))+
  theme(plot.title=element_text(size=14, face = "bold"))

p<-p+geom_path(data = tribes_df,
               aes(x = long, y = lat, group = group),
               color = 'azure4', size = .2)

p<-p+geom_point(data = SWCities, aes(x = lon, y = lat), size = 1,
                shape = 20)

p<-p+geom_text(data = SWCities, aes(x = lon, y = lat, label = City),
               size = 4, col = "black", fontface = "bold", nudge_y = 0.1)

# write out file
png("/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/SW_Monsoon_Seasonal_Precip_Climatology.png", width = 16, height = 8, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

# add logos
# Call back the plot
plot <- image_read("/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/SW_Monsoon_Seasonal_Precip_Climatology.png")
# And bring in a logo
#logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png")
logo <- image_resize(logo_raw, geometry_size_percent(width=100,height = 100))
# Stack them on top of each other
#final_plot <- image_append((c(plot, logo)), stack = TRUE)
#final_plot <- image_mosaic((c(plot, logo)))
final_plot <- image_composite(plot, logo, offset = "+410+2170")
# And overwrite the plot without a logo
image_write(final_plot, "/home/crimmins/RProjects/ClimPlot/monsoonClimo_maps/SW_Monsoon_Seasonal_Precip_Climatology.png")


