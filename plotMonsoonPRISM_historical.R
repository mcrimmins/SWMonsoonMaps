# Summer Monsoon Precip mapping
# adapted from plotPRISM.R
# Get Gridded data from ACIS
# MAC 05/31/2019

# HISTORICAL VERSION 06/10/2019

library(RCurl)
library(jsonlite)
library(ggplot2)
#library(ggmap)
library(scales)
library(reshape2)
library(raster)
library(rasterVis)
library(PBSmapping)
library(Hmisc)
library(rgdal)
library(readr)
library(magick)
library(leaflet)
library(mapview)
library(leafem)
library(htmlwidgets)
library(rmarkdown)
library(knitr)
library(rmdformats)


# Loop through years
yr1=2023
yr2=2023

for (yearDir in yr1:yr2)
{
  # create dirs for year
  dir.create(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir),showWarnings = FALSE)
  dir.create(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/daily"),showWarnings = FALSE)
  dir.create(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/leafletMaps"),showWarnings = FALSE)
  # copy markdown file into dir
  file.copy("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/SWmonsoonTemplateHistorical.Rmd",
            paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir), overwrite = TRUE)
  
  
## ---- download PRISM data ----
# Manually set universal date range - ACIS PRISM current day-1, correct for LINUX UTC time
dateRangeStart=paste0(yearDir,"-06-15")
dateRangeEnd=paste0(yearDir,"-09-30")

# auto date range...start with 6-15 and run on 6-17 to get two days of data, end on 10/1
#dateRangeStart="2019-06-15"
#dateRangeEnd=Sys.Date()
#  if(dateRangeEnd<"2019-06-17" | dateRangeEnd>="2019-10-01"){
#    stop()
#  }

# generate dates -- keep with PRISM date
allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)
  #allDates<-as.Date(format(allDates, format="%m-%d-%Y"),format="%m-%d-%Y")

# Automated Universal date range
# days<-35
# dateRangeEnd<-as.Date(format(Sys.time(), "%Y-%m-%d"))-1
# dateRangeStart<-as.Date(format(Sys.time(), "%Y-%m-%d"))-days
# allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)

# get shapefiles
all_states <- map_data("state")
all_counties<-map_data("county")
tribes <- readOGR("/home/crimmins/RProjects/ClimPlot/tribes", "indlanp020")
# load cities
SWCities <- read_csv("/home/crimmins/RProjects/ClimPlot/SWCities.csv")

# AZ/NM bbox -115.004883,31.184609,-102.524414,37.387617
 ACISbbox<-"-115,31,-102,38"

# ACIS query
jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"21","elems":"pcpn","meta":"ll,elev","output":"json"}') # or uid
#jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"2","elems":"pcpn","meta":"ll","output":"json"}') # or uid

out<-postForm("http://data.rcc-acis.org/GridData", 
              .opts = list(postfields = jsonQuery, 
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
out<-fromJSON(out)

# convert to list of matrices, flipud with PRISM
matrixList <- vector("list",length(out$data))
for(i in 1:length(out$data)){
  matrixList[[i]]<-apply(t(out$data[[i]][[2]]),1,rev) 
}

# get elev grid
elevGrid<-apply(t(out[["meta"]][["elev"]]),1,rev) 
elevGrid<-raster(elevGrid)
gridExtent<-extent(min(out$meta$lon), max(out$meta$lon), min(out$meta$lat), max(out$meta$lat))
elevGrid<-setExtent(elevGrid, gridExtent, keepres=FALSE, snap=FALSE)
  elevGrid[elevGrid < 0] <- NA

# read into raster stack
rasterList<-lapply(matrixList, raster)
gridStack<-stack(rasterList)
gridExtent<-extent(min(out$meta$lon), max(out$meta$lon), min(out$meta$lat), max(out$meta$lat))
gridStack<-setExtent(gridStack, gridExtent, keepres=FALSE, snap=FALSE)
names(gridStack)<-allDates
# grab grid for ts extent
gridStackTS<-gridStack
# set 0 and neg to NA
gridStack[gridStack <= 0] <- NA
## ----

# create summary grids ----
# total precip
totalPrecipAll<-calc(gridStack, sum, na.rm=TRUE)
  totalPrecipAll_w0<-totalPrecipAll
totalPrecipAll[totalPrecipAll <= 0] <- NA
  totalPrecipAll_w0[totalPrecipAll_w0 < 0]<- NA
# percent of average
JJASppt<-stack("/home/crimmins/RProjects/ClimPlot/PRISM/JJASppt.grd")
  JJASppt<-JJASppt/25.4
moStack<-as.numeric(format(allDates[length(allDates)], format='%m'))-5

if (moStack==1) {
  normal<-JJASppt[[moStack]]*(as.numeric(format(allDates[length(allDates)], format='%d'))/monthDays(allDates[length(allDates)]))
} else if ( moStack==2) {
  norm1<-JJASppt[[moStack]]*(as.numeric(format(allDates[length(allDates)], format='%d'))/monthDays(allDates[length(allDates)]))
  normal<-norm1+JJASppt[[(moStack-1)]]
} else {
  norm1<-JJASppt[[moStack]]*(as.numeric(format(allDates[length(allDates)], format='%d'))/monthDays(allDates[length(allDates)]))
  norm2<-calc(JJASppt[[1:(moStack-1)]], sum, na.rm=TRUE)
  normal<-norm1+norm2
}
percPrecip<-totalPrecipAll/normal

# perc days with rain >0.01"
rainDays <- calc(gridStack, fun=function(x){sum(x > 0.01, na.rm = TRUE)})
percRainDays<-(rainDays/length(allDates))*100
  percRainDays[percRainDays <= 0] <- NA

# daily intensity index
sdii<-totalPrecipAll/rainDays
  sdii[is.infinite(sdii)] <- NA  

# max 1-day precip
maxRain <- calc(gridStack, fun=function(x){max(x, na.rm = TRUE)})
  maxRain[maxRain <= 0] <- NA

# days since 0.05" rainfall
daysSince <-length(allDates)-(calc(gridStack, fun=function(x){max(which(x > 0.05), na.rm = TRUE)}))
  daysSince[daysSince <= 0] <- NA
  daysSince[daysSince==Inf] <- NA
## ----

  # percentile rank of precip
  # load allCumSum
  allCumSum<-stack("/home/crimmins/RProjects/ClimPlot/AZNM_PRISM_Monsoon_cumPrecip_1981_2022.grd")
  # get subsets for the current day
  doyCumSum<-subset(allCumSum, seq(i,nlayers(allCumSum)-(108-i),by=108))
  # add current year
  doyCumSum<-stack(doyCumSum,totalPrecipAll_w0)
  
  perc.rank<-function(x) trunc(rank(x,ties.method = "average"))/length(x)
  percRankPrecip <- calc(doyCumSum, fun=perc.rank)
  percRankPrecip <-(percRankPrecip[[nlayers(percRankPrecip)]])*100
  ## ----
  
# process daily anomalies ----
dailyAnoms= list()
  i<-1
for(i in 2:nlayers(gridStack)){
  totalPrecip<-calc(gridStack[[1:i]], sum, na.rm=TRUE)
  # percent of average
  JJASppt<-stack("/home/crimmins/RProjects/ClimPlot/PRISM/JJASppt.grd")
  JJASppt<-JJASppt/25.4
  moStack<-as.numeric(format(allDates[i], format='%m'))-5
  
  if (moStack==1) {
    normal<-JJASppt[[moStack]]*(as.numeric(format(allDates[i], format='%d'))/monthDays(allDates[i]))
  } else if ( moStack==2) {
    norm1<-JJASppt[[moStack]]*(as.numeric(format(allDates[i], format='%d'))/monthDays(allDates[i]))
    normal<-norm1+JJASppt[[(moStack-1)]]
  } else {
    norm1<-JJASppt[[moStack]]*(as.numeric(format(allDates[i], format='%d'))/monthDays(allDates[i]))
    norm2<-calc(JJASppt[[1:(moStack-1)]], sum, na.rm=TRUE)
    normal<-norm1+norm2
  }
  percPrecip<-(totalPrecip/normal)*100

freq=hist(getValues(percPrecip),c(0,75,125,200,Inf), plot=FALSE)
dailyAnoms[[i-1]] <- t(freq$counts/sum(freq$counts))
}
# create data table
dailyAnoms = as.data.frame(do.call(rbind, dailyAnoms))
dailyAnoms$Date<-allDates[2:i]
colnames(dailyAnoms)<-c("Below Average","Near Average","Above Average","Much Above Average","Date")

dailyAnomsMelt<-melt(dailyAnoms, id.vars = "Date")
  colnames(dailyAnomsMelt)<-c("Date","Anomaly","Coverage")
  dailyAnomsMelt$Coverage<-dailyAnomsMelt$Coverage*100

# plot anomaly time series 
p<-ggplot(dailyAnomsMelt, aes(x=Date,y=Coverage,fill=Anomaly))+
  geom_area(color="black")+
  scale_fill_manual(values=c("tan4", "white","green4","blue"),
                    labels=c("Below Avg (<75%)","Near Avg (75-125%)","Above Avg (125-200%)","Much Above Avg (>200%)"))+
  scale_x_date(expand = c(0, 0), date_breaks = "1 weeks", date_labels = "%b-%d") +
  scale_y_continuous(expand = c(0, 0))+
  ylab("Percent of Southwest in Anomaly Category")+
  guides(fill=guide_legend(title="Anom Category"))+
  ggtitle(paste0("Precipitation Anomaly (% of Ave) Coverage: ",allDates[1]," to ",allDates[length(allDates)]))+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group\nRCC-ACIS"))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title=element_text(size=14, face = "bold"),
        legend.position = c(0.1, 0.88),
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="black"),
        axis.text.x = element_text(angle = -45, hjust = 0))

      # write out file
      png(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_Anomaly_TS.png"), width = 11, height = 8, units = "in", res = 300L)
      #grid.newpage()
      print(p, newpage = FALSE)
      dev.off()
      
      # add logos
      # Call back the plot
      plot <- image_read(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_Anomaly_TS.png"))
      # And bring in a logo
      logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
      logo <- image_resize(logo_raw, geometry_size_percent(width=120,height = 120))
      # Stack them on top of each other
      #final_plot <- image_append((c(plot, logo)), stack = TRUE)
      #final_plot <- image_mosaic((c(plot, logo)))
      final_plot <- image_composite(plot, logo, offset = "+180+2130")
      # And overwrite the plot without a logo
      image_write(final_plot, paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_Anomaly_TS.png"))
# ----

      # ----
      # PROCESS and PLOT daily monsoon extent in AZ and NM
      # precip threshold
      thresh<-0.01
      # load datasets
      load("/home/crimmins/RProjects/ClimPlot/AZ_doyStats.RData")
      load("/home/crimmins/RProjects/ClimPlot/NM_doyStats.RData")
      USPoly <- getData("GADM", country="USA", level=1)
      NMPoly<-subset(USPoly, NAME_1=="New Mexico")
      AZPoly<-subset(USPoly, NAME_1=="Arizona")
      # extract extents
      currYearClip <- raster::mask(gridStackTS, AZPoly)
      # get counts
      AZts<-as.data.frame(cellStats(currYearClip, function(i, ...) sum(i>thresh, na.rm = TRUE)))
      currYearClip <- raster::mask(gridStackTS, NMPoly)
      # get counts
      NMts<-as.data.frame(cellStats(currYearClip, function(i, ...) sum(i>thresh, na.rm = TRUE)))
      # get base count
      tempGrid <- raster::mask(gridStackTS[[1]], AZPoly)
      baseCountAZ<-cellStats(tempGrid, function(i, ...) sum(i>=0, na.rm = TRUE)) 
      tempGrid <- raster::mask(gridStackTS[[1]], NMPoly)
      baseCountNM<-cellStats(tempGrid, function(i, ...) sum(i>=0, na.rm = TRUE))  
      
      # AZ data table
      # get dates from col names
      colnames(AZts)<-"counts"
      AZts$percExt<-(AZts$counts/baseCountAZ)*100
      AZts$dates<-rownames(AZts)
      AZts<-tidyr::separate(AZts,dates, c(NA,"year",NA,"month",NA,"day"),sep=c(1,5,6,8,9,11), convert = TRUE)
      # build date and doy
      AZts$date<-as.Date(paste0(AZts$year,"-",AZts$month,"-",AZts$day), format="%Y-%m-%d")
      # dummy date var
      AZts$dummyDate<-as.Date(paste0(2000,"-",AZts$month,"-",AZts$day), format="%Y-%m-%d")
      AZts$doy<-as.numeric(format(AZts$dummyDate, "%j"))
      # join stats to data table
      AZ_doyStats$doy<-as.numeric(format(AZ_doyStats$dummyDate, "%j"))
      AZts<-merge(AZts, AZ_doyStats, by="doy", all.y=TRUE)
      AZts$abvQ50<-ifelse(AZts$percExt>=AZts$q50smooth,1,0) # monsoon days
      
      # NM data table
      # get dates from col names
      colnames(NMts)<-"counts"
      NMts$percExt<-(NMts$counts/baseCountNM)*100
      NMts$dates<-rownames(NMts)
      NMts<-tidyr::separate(NMts,dates, c(NA,"year",NA,"month",NA,"day"),sep=c(1,5,6,8,9,11), convert = TRUE)
      # build date and doy
      NMts$date<-as.Date(paste0(NMts$year,"-",NMts$month,"-",NMts$day), format="%Y-%m-%d")
      # dummy date var
      NMts$dummyDate<-as.Date(paste0(2000,"-",NMts$month,"-",NMts$day), format="%Y-%m-%d")
      NMts$doy<-as.numeric(format(NMts$dummyDate, "%j"))
      # join stats to data table
      NM_doyStats$doy<-as.numeric(format(NM_doyStats$dummyDate, "%j"))
      NMts<-merge(NMts, NM_doyStats, by="doy", all.y=TRUE)
      NMts$abvQ50<-ifelse(NMts$percExt>=NMts$q50smooth,1,0) # monsoon days    
      
      
      #  AZ plot of daily monsoon extent by year
      p<-ggplot(AZts, aes(dummyDate.y,percExt))+
        geom_bar(stat = "identity", fill="darkgreen")+
        xlab("Day of Year")+
        ylab("Percent Coverage - AZ")+
        ylim(0,100)
      p<-p+geom_line(data=AZts, aes(dummyDate.y,q50smooth))+
        geom_label(aes(label=sum(AZts$abvQ50, na.rm=TRUE)), 
                   x = -Inf, y = Inf, hjust=0, vjust=1,
                   inherit.aes = FALSE)+
        ggtitle(paste0("Percent of Arizona observing >=0.01 inches daily total precip: ",allDates[1]," to ",allDates[length(allDates)]))+
        labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                            "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group\nRCC-ACIS"))+
        theme_bw()+
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14),
              plot.title=element_text(size=14, face = "bold"))
      
      # write out file
      png(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/AZMonsoonDays.png"), width = 11, height = 8, units = "in", res = 300L)
      #grid.newpage()
      print(p, newpage = FALSE)
      dev.off()
      
      # add logos
      # Call back the plot
      plot <- image_read(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/AZMonsoonDays.png"))
      # And bring in a logo
      #logo_raw <- image_read("./logos/UA_CLIMAS_logos.png") 
      logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
      logo <- image_resize(logo_raw, geometry_size_percent(width=120,height = 120))
      # Stack them on top of each other
      #final_plot <- image_append((c(plot, logo)), stack = TRUE)
      #final_plot <- image_mosaic((c(plot, logo)))
      final_plot <- image_composite(plot, logo, offset = "+150+2150")
      # And overwrite the plot without a logo
      image_write(final_plot, paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/AZMonsoonDays.png"))   
      
      #  NM plot of daily monsoon extent by year
      p<-ggplot(NMts, aes(dummyDate.y,percExt))+
        geom_bar(stat = "identity", fill="darkgreen")+
        xlab("Day of Year")+
        ylab("Percent Coverage - NM")+
        ylim(0,100)
      p<-p+geom_line(data=NMts, aes(dummyDate.y,q50smooth))+
        geom_label(aes(label=sum(NMts$abvQ50, na.rm=TRUE)), 
                   x = -Inf, y = Inf, hjust=0, vjust=1,
                   inherit.aes = FALSE)+
        ggtitle(paste0("Percent of New Mexico observing >=0.01 inches daily total precip: ",allDates[1]," to ",allDates[length(allDates)]))+
        labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                            "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group\nRCC-ACIS"))+
        theme_bw()+
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14),
              plot.title=element_text(size=14, face = "bold"))
      
      # write out file
      png(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/NMMonsoonDays.png"), width = 11, height = 8, units = "in", res = 300L)
      #grid.newpage()
      print(p, newpage = FALSE)
      dev.off()
      
      # add logos
      # Call back the plot
      plot <- image_read(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/NMMonsoonDays.png"))
      # And bring in a logo
      #logo_raw <- image_read("./logos/UA_CLIMAS_logos.png") 
      logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
      logo <- image_resize(logo_raw, geometry_size_percent(width=120,height = 120))
      # Stack them on top of each other
      #final_plot <- image_append((c(plot, logo)), stack = TRUE)
      #final_plot <- image_mosaic((c(plot, logo)))
      final_plot <- image_composite(plot, logo, offset = "+150+2150")
      # And overwrite the plot without a logo
      image_write(final_plot, paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/NMMonsoonDays.png"))   
      
      # ----    
      
      
# PLOT total precip vs elevation categories ----
      elevPrecip<-as.data.frame(getValues(stack(totalPrecipAll,elevGrid)))
      colnames(elevPrecip)<-c("precip","elevation")
        elevPrecip<-elevPrecip[complete.cases(elevPrecip),]
      elevPrecip$elevation_group<-cut(elevPrecip$elevation, breaks=seq(0,15000,by=2000), dig.lab=5) #, labels=as.character(seq(3000,15000,by=3000)
      
      p<- ggplot(elevPrecip, aes(x=elevation_group, y=precip)) + 
        geom_boxplot(varwidth = TRUE, fill='seagreen4')+
         ylab("Total Precipitation (in)")+
         xlab("Elevation Range (min,max - ft)")+
         ggtitle(paste0("Range of Total Precipitation Values across SW US by Elevation Groups: ",allDates[1]," to ",allDates[length(allDates)]))+
         labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                             "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group\nRCC-ACIS"))+
        theme_bw()+
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14),
              plot.title=element_text(size=14, face = "bold"))
        
      # write out file
      png(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_PrecipByElevation.png"), width = 11, height = 8, units = "in", res = 300L)
      #grid.newpage()
      print(p, newpage = FALSE)
      dev.off()
      
      # add logos
      # Call back the plot
      plot <- image_read(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_PrecipByElevation.png"))
      # And bring in a logo
      #logo_raw <- image_read("./logos/UA_CLIMAS_logos.png") 
      logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
      logo <- image_resize(logo_raw, geometry_size_percent(width=120,height = 120))
      # Stack them on top of each other
      #final_plot <- image_append((c(plot, logo)), stack = TRUE)
      #final_plot <- image_mosaic((c(plot, logo)))
      final_plot <- image_composite(plot, logo, offset = "+150+2150")
      # And overwrite the plot without a logo
      image_write(final_plot, paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_PrecipByElevation.png"))   
# END precip/elevation plots ----             
  
# PLOT ANOM vs elevation categories ----
      elevPrecip<-as.data.frame(getValues(stack(percPrecip,elevGrid)))
      colnames(elevPrecip)<-c("precip","elevation")
      elevPrecip<-elevPrecip[complete.cases(elevPrecip),]
      elevPrecip$elevation_group<-cut(elevPrecip$elevation, breaks=seq(0,15000,by=2000), dig.lab=5) #, labels=as.character(seq(3000,15000,by=3000)
      
      p<- ggplot(elevPrecip, aes(x=elevation_group, y=precip)) + 
        geom_boxplot(varwidth = TRUE, fill='indianred')+
        ylab("Percent of Average Precipitation")+
        xlab("Elevation Range (min,max - ft)")+
        ggtitle(paste0("Range of Perc of Average Values across SW US by Elevation Groups: ",allDates[1]," to ",allDates[length(allDates)]))+
        labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                            "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group\nRCC-ACIS"))+
        theme_bw()+
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14),
              plot.title=element_text(size=14, face = "bold"))
      
      
      # write out file
      png(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_AnomByElevation.png"), width = 11, height = 8, units = "in", res = 300L)
      #grid.newpage()
      print(p, newpage = FALSE)
      dev.off()
      
      # add logos
      # Call back the plot
      plot <- image_read(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_AnomByElevation.png"))
      # And bring in a logo
      #logo_raw <- image_read("./logos/UA_CLIMAS_logos.png") 
      logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
      logo <- image_resize(logo_raw, geometry_size_percent(width=120,height = 120))
      # Stack them on top of each other
      #final_plot <- image_append((c(plot, logo)), stack = TRUE)
      #final_plot <- image_mosaic((c(plot, logo)))
      final_plot <- image_composite(plot, logo, offset = "+150+2150")
      # And overwrite the plot without a logo
      image_write(final_plot, paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_AnomByElevation.png"))   
      
# END anom elevation plots ----             
      
          
# PLOT AND SAVE MAPS ----
# load map boundary data
xlim = c(-115,-102)
ylim = c(31,38)
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

    
# total precip Map -----
# colorramp for total precip
precipCols<-colorRampPalette(c("lightblue", "dodgerblue3", "palegreen","green4","salmon","orangered3",
                   "lightgoldenrod1","orange2","plum2","purple"))(50)
precBreaks<-seq(0,20,2)
precLabs<-as.character(seq(0,20,2))
  precLabs[11]<-">20"
  precLabs[1]<-"0.01"
#precBreaksmin<-seq(1,19,2)

#theme_set(theme_bw())

p<-gplot(totalPrecipAll) + geom_tile(aes(fill = value)) +
  #scale_fill_gradient2(low = 'white', high = 'blue') +
  #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod", 
  #                     name="inches", limits=c(0,20),oob=squish)+
  
  scale_fill_gradientn(colours = precipCols, na.value="darkgoldenrod", 
                                          name="inches", limits=c(0,20),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
  guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+
  
  coord_equal(xlim = c(-115,-102), ylim = c(31,38), expand = FALSE)+
  xlab("Longitude") + ylab("Latitude") 

p<-p +  geom_polygon( data=all_states, aes(x=X, y=Y, group = PID),colour="black", fill=NA, size=0.25 )+
  #scale_x_continuous(breaks = c(-120,-140))+
  #ggtitle("Daily Total Precip (inches) - PRISM")+
  ggtitle(paste0("Total Precipitation (in.): ",allDates[1]," to ",allDates[length(allDates)]))+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group\nRCC-ACIS"))+
  theme(plot.title=element_text(size=14, face = "bold"))

p<-p+geom_path(data = tribes_df, 
               aes(x = long, y = lat, group = group),
               color = 'azure4', size = .2)

p<-p+geom_point(data = SWCities, aes(x = lon, y = lat), size = 1, 
           shape = 20)

p<-p+geom_text(data = SWCities, aes(x = lon, y = lat, label = City), 
            size = 3, col = "black", fontface = "bold", nudge_y = 0.1)

# write out file
png(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_TotalPrecip.png"), width = 16, height = 8, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

# add logos
# Call back the plot
plot <- image_read(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_TotalPrecip.png"))
# And bring in a logo
#logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
logo <- image_resize(logo_raw, geometry_size_percent(width=120,height = 120))
# Stack them on top of each other
#final_plot <- image_append((c(plot, logo)), stack = TRUE)
#final_plot <- image_mosaic((c(plot, logo)))
final_plot <- image_composite(plot, logo, offset = "+510+2150")
# And overwrite the plot without a logo
image_write(final_plot, paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_TotalPrecip.png"))

# leaflet interactive
pal <- colorNumeric(c("lightblue", "dodgerblue3", "palegreen","green4","salmon","orangered3",
                      "lightgoldenrod1","orange2","plum2","purple"), values(totalPrecipAll),
                    na.color = "transparent")
crs(totalPrecipAll) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

leafMap<-leaflet() %>% addTiles() %>%
          addRasterImage(totalPrecipAll, colors = pal, opacity = 0.8, layerId = "Inches") %>%
          addLegend(pal = pal, values = values(totalPrecipAll),
                    title=paste0("Total Precipitation (in.):<br> ",allDates[1]," to ",allDates[length(allDates)]))%>%
          addMouseCoordinates() %>%
          addImageQuery(totalPrecipAll, type="mousemove", layerId = "Inches", prefix = "")

saveWidget(leafMap, file=paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/leafletMaps/SW_Monsoon_TotalPrecip.html"))

# ----- end Total Precip ----

# PERCENT of AVG Map -----
# colorramp for total precip
precipCols<-colorRampPalette(c("darkgoldenrod4", "white", "darkgreen","blue1","deepskyblue"))(50)
precBreaks<-seq(0,400,50)
precLabs<-as.character(seq(0,400,50))
precLabs[9]<-">400"
precLabs[1]<-"0"
#precBreaksmin<-seq(1,19,2)

#theme_set(theme_bw())
p<-gplot(percPrecip) + geom_tile(aes(fill = value)) +
  #scale_fill_gradient2(low = 'white', high = 'blue') +
  #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod", 
  #                     name="inches", limits=c(0,20),oob=squish)+
  
  scale_fill_gradientn(colours = precipCols, na.value="darkgoldenrod", 
                       name="% of avg", limits=c(0,400),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
  guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+
  
  coord_equal(xlim = c(-115,-102), ylim = c(31,38), expand = FALSE)+
  xlab("Longitude") + ylab("Latitude") 

p<-p +  geom_polygon( data=all_states, aes(x=X, y=Y, group = PID),colour="black", fill=NA, size=0.25 )+
  #scale_x_continuous(breaks = c(-120,-140))+
  #ggtitle("Daily Total Precip (inches) - PRISM")+
  ggtitle(paste0("Percent of Average Precipitation (%): ",allDates[1]," to ",allDates[length(allDates)]))+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group\nRCC-ACIS"))+
  theme(plot.title=element_text(size=14, face = "bold"))

p<-p+geom_path(data = tribes_df, 
               aes(x = long, y = lat, group = group),
               color = 'azure4', size = .2)

p<-p+geom_point(data = SWCities, aes(x = lon, y = lat), size = 1, 
                shape = 20)

p<-p+geom_text(data = SWCities, aes(x = lon, y = lat, label = City), 
               size = 3, col = "black", fontface = "bold", nudge_y = 0.1)

# write out file
png(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_PercentPrecip.png"), width = 16, height = 8, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

# add logos
# Call back the plot
plot <- image_read(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_PercentPrecip.png"))
# And bring in a logo
#logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
logo <- image_resize(logo_raw, geometry_size_percent(width=120,height = 120))
# Stack them on top of each other
#final_plot <- image_append((c(plot, logo)), stack = TRUE)
#final_plot <- image_mosaic((c(plot, logo)))
final_plot <- image_composite(plot, logo, offset = "+510+2150")
# And overwrite the plot without a logo
image_write(final_plot, paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_PercentPrecip.png"))

# leaflet interactive
pal <- colorNumeric(c("darkgoldenrod4", "white", "darkgreen","blue1","deepskyblue"), c(0,400),
                    na.color = "transparent")
crs(percPrecip) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

leafMap<-leaflet() %>% addTiles() %>%
  addRasterImage(percPrecip, colors = pal, opacity = 0.8, layerId = "% of Avg") %>%
  addLegend(pal = pal, values = values(percPrecip),
            title=paste0("% of Avg Precip:<br> ",allDates[1]," to ",allDates[length(allDates)]))%>%
  addMouseCoordinates() %>%
  addImageQuery(percPrecip, type="mousemove", layerId = "% of Avg", digits = 0, prefix = "")

saveWidget(leafMap, file=paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/leafletMaps/SW_Monsoon_PercentPrecip.html"))
# ----- end PERCENT AVG ----


# RRECIP PERCENTILES Map -----
# colorramp for total precip
precipCols<-colorRampPalette(c("darkgoldenrod4", "white", "darkgreen"))(50)
precBreaks<-seq(0,100,10)
precLabs<-as.character(seq(0,100,10))
precLabs[11]<-"100"
precLabs[1]<-"0"
#precBreaksmin<-seq(1,19,2)

#theme_set(theme_bw())
p<-gplot(percRankPrecip) + geom_tile(aes(fill = value)) +
  #scale_fill_gradient2(low = 'white', high = 'blue') +
  #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod", 
  #                     name="inches", limits=c(0,20),oob=squish)+
  
  scale_fill_gradientn(colours = precipCols, na.value="darkgoldenrod", 
                       name="%tile", limits=c(0,100),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
  guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+
  
  coord_equal(xlim = c(-115,-102), ylim = c(31,38), expand = FALSE)+
  xlab("Longitude") + ylab("Latitude") 

p<-p +  geom_polygon( data=all_states, aes(x=X, y=Y, group = PID),colour="black", fill=NA, size=0.25 )+
  #scale_x_continuous(breaks = c(-120,-140))+
  #ggtitle("Daily Total Precip (inches) - PRISM")+
  ggtitle(paste0("Total Precipitation Percentile Rank (1981-2018 ranking period): ",allDates[1]," to ",allDates[length(allDates)]))+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group\nRCC-ACIS"))+
  theme(plot.title=element_text(size=14, face = "bold"))

p<-p+geom_path(data = tribes_df, 
               aes(x = long, y = lat, group = group),
               color = 'azure4', size = .2)

p<-p+geom_point(data = SWCities, aes(x = lon, y = lat), size = 1, 
                shape = 20)

p<-p+geom_text(data = SWCities, aes(x = lon, y = lat, label = City), 
               size = 3, col = "black", fontface = "bold", nudge_y = 0.1)

# write out file
png(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_RankPrecip.png"), width = 16, height = 8, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

# add logos
# Call back the plot
plot <- image_read(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_RankPrecip.png"))
# And bring in a logo
#logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
logo <- image_resize(logo_raw, geometry_size_percent(width=120,height = 120))
# Stack them on top of each other
#final_plot <- image_append((c(plot, logo)), stack = TRUE)
#final_plot <- image_mosaic((c(plot, logo)))
final_plot <- image_composite(plot, logo, offset = "+510+2150")
# And overwrite the plot without a logo
image_write(final_plot, paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_RankPrecip.png"))

# leaflet interactive
pal <- colorNumeric(c("darkgoldenrod4", "white", "darkgreen"), c(0,100),
                    na.color = "transparent")
crs(percRankPrecip) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

leafMap<-leaflet() %>% addTiles() %>%
  addRasterImage(percRankPrecip, colors = pal, opacity = 0.8, layerId = "%tile") %>%
  addLegend(pal = pal, values = values(percRankPrecip),
            title=paste0("%-tile rank of total precip:<br> ",allDates[1]," to ",allDates[length(allDates)]))%>%
  addMouseCoordinates() %>%
  addImageQuery(percRankPrecip, type="mousemove", layerId = "%tile", digits = 0, prefix = "")

saveWidget(leafMap, file=paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/leafletMaps/SW_Monsoon_RankPrecip.html"), selfcontained = FALSE)
# ----- end PERCENTILE ----


# RAIN DAYS Percent Map -----
# colorramp for total precip
precipCols<-colorRampPalette(c("yellow2", "green", "blue","red","orange"))(50)
precBreaks<-seq(0,75,5)
precLabs<-as.character(seq(0,75,5))
precLabs[16]<-">75"
precLabs[1]<-"0"
#precBreaksmin<-seq(1,19,2)

#theme_set(theme_bw())
p<-gplot(percRainDays) + geom_tile(aes(fill = value)) +
  #scale_fill_gradient2(low = 'white', high = 'blue') +
  #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod", 
  #                     name="inches", limits=c(0,20),oob=squish)+
  
  scale_fill_gradientn(colours = precipCols, na.value="darkgoldenrod", 
                       name="%", limits=c(0,75),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
  guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+
  
  coord_equal(xlim = c(-115,-102), ylim = c(31,38), expand = FALSE)+
  xlab("Longitude") + ylab("Latitude") 

p<-p +  geom_polygon( data=all_states, aes(x=X, y=Y, group = PID),colour="black", fill=NA, size=0.25 )+
  #scale_x_continuous(breaks = c(-120,-140))+
  #ggtitle("Daily Total Precip (inches) - PRISM")+
  ggtitle(paste0("Percent of days with rain (>0.01 in):",allDates[1]," to ",allDates[length(allDates)]))+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group\nRCC-ACIS"))+
  theme(plot.title=element_text(size=14, face = "bold"))

p<-p+geom_path(data = tribes_df, 
               aes(x = long, y = lat, group = group),
               color = 'azure4', size = .2)

p<-p+geom_point(data = SWCities, aes(x = lon, y = lat), size = 1, 
                shape = 20)

p<-p+geom_text(data = SWCities, aes(x = lon, y = lat, label = City), 
               size = 3, col = "black", fontface = "bold", nudge_y = 0.1)

# write out file
png(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_PercentDays.png"), width = 16, height = 8, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

# add logos
# Call back the plot
plot <- image_read(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_PercentDays.png"))
# And bring in a logo
#logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
logo <- image_resize(logo_raw, geometry_size_percent(width=120,height = 120))
# Stack them on top of each other
#final_plot <- image_append((c(plot, logo)), stack = TRUE)
#final_plot <- image_mosaic((c(plot, logo)))
final_plot <- image_composite(plot, logo, offset = "+510+2150")
# And overwrite the plot without a logo
image_write(final_plot, paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_PercentDays.png"))

# leaflet interactive
pal <- colorNumeric(c("yellow2", "green", "blue","red","orange"), c(0,75),
                    na.color = "transparent")
crs(percRainDays) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

leafMap<-leaflet() %>% addTiles() %>%
  addRasterImage(percRainDays, colors = pal, opacity = 0.8, layerId = "perc_days") %>%
  addLegend(pal = pal, values = values(percRainDays),
            title=paste0("% Rain Days:<br> ",allDates[1]," to ",allDates[length(allDates)]))%>%
  addMouseCoordinates() %>%
  leafem::addImageQuery(percRainDays, type="mousemove", layerId = "perc_days", digits = 0, prefix = "")

saveWidget(leafMap, file=paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/leafletMaps/SW_Monsoon_PercentDays.html"))
# ----- end PERCENT AVG ----

# SDII Map -----
# colorramp for total precip
precipCols<-colorRampPalette(c("yellow", "blue", "red","orange"))(50)
precBreaks<-seq(0,1.5,0.1)
precLabs<-as.character(seq(0,1.5,0.1))
precLabs[16]<-">1.5"
precLabs[1]<-"0"
#precBreaksmin<-seq(1,19,2)

#theme_set(theme_bw())
p<-gplot(sdii) + geom_tile(aes(fill = value)) +
  #scale_fill_gradient2(low = 'white', high = 'blue') +
  #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod", 
  #                     name="inches", limits=c(0,20),oob=squish)+
  
  scale_fill_gradientn(colours = precipCols, na.value="darkgoldenrod", 
                       name="in/day", limits=c(0,1.5),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
  guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+
  
  coord_equal(xlim = c(-115,-102), ylim = c(31,38), expand = FALSE)+
  xlab("Longitude") + ylab("Latitude") 

p<-p +  geom_polygon( data=all_states, aes(x=X, y=Y, group = PID),colour="black", fill=NA, size=0.25 )+
  #scale_x_continuous(breaks = c(-120,-140))+
  #ggtitle("Daily Total Precip (inches) - PRISM")+
  ggtitle(paste0("Intensity Index (total precip/rain days):",allDates[1]," to ",allDates[length(allDates)]))+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group\nRCC-ACIS"))+
  theme(plot.title=element_text(size=14, face = "bold"))

p<-p+geom_path(data = tribes_df, 
               aes(x = long, y = lat, group = group),
               color = 'azure4', size = .2)

p<-p+geom_point(data = SWCities, aes(x = lon, y = lat), size = 1, 
                shape = 20)

p<-p+geom_text(data = SWCities, aes(x = lon, y = lat, label = City), 
               size = 3, col = "black", fontface = "bold", nudge_y = 0.1)

# write out file
png(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_IntensityIndex.png"), width = 16, height = 8, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

# add logos
# Call back the plot
plot <- image_read(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_IntensityIndex.png"))
# And bring in a logo
#logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
logo <- image_resize(logo_raw, geometry_size_percent(width=120,height = 120))
# Stack them on top of each other
#final_plot <- image_append((c(plot, logo)), stack = TRUE)
#final_plot <- image_mosaic((c(plot, logo)))
final_plot <- image_composite(plot, logo, offset = "+510+2150")
# And overwrite the plot without a logo
image_write(final_plot, paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_IntensityIndex.png"))

# leaflet interactive
pal <- colorNumeric(c("yellow", "blue", "red","orange"), c(0,1.5),
                    na.color = "transparent")
crs(sdii) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

leafMap<-leaflet() %>% addTiles() %>%
  addRasterImage(sdii, colors = pal, opacity = 0.8, layerId = "in per day") %>%
  addLegend(pal = pal, values = values(sdii),
            title=paste0("Intensity Index:<br> ",allDates[1]," to ",allDates[length(allDates)]))%>%
  #addMouseCoordinates() %>%
  addImageQuery(sdii, type="mousemove", layerId = "in per day", digits = 0, prefix = "")

saveWidget(leafMap, file=paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/leafletMaps/SW_Monsoon_IntensityIndex.html"))
# ----- end SDII ----

# MAX DAILY precip Map -----
# colorramp for total precip
precipCols<-colorRampPalette(c("lightblue", "dodgerblue3", "palegreen","green4","salmon","orangered3",
                               "lightgoldenrod1","orange2","plum2","purple"))(50)
precBreaks<-seq(0,6,0.5)
precLabs<-as.character(seq(0,6,0.5))
precLabs[13]<-">6"
precLabs[1]<-"0.01"
#precBreaksmin<-seq(1,19,2)

#theme_set(theme_bw())

p<-gplot(maxRain) + geom_tile(aes(fill = value)) +
  #scale_fill_gradient2(low = 'white', high = 'blue') +
  #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod", 
  #                     name="inches", limits=c(0,20),oob=squish)+
  
  scale_fill_gradientn(colours = precipCols, na.value="darkgoldenrod", 
                       name="inches", limits=c(0,6),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
  guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+
  
  coord_equal(xlim = c(-115,-102), ylim = c(31,38), expand = FALSE)+
  xlab("Longitude") + ylab("Latitude") 

p<-p +  geom_polygon( data=all_states, aes(x=X, y=Y, group = PID),colour="black", fill=NA, size=0.25 )+
  #scale_x_continuous(breaks = c(-120,-140))+
  #ggtitle("Daily Total Precip (inches) - PRISM")+
  ggtitle(paste0("Max 1-day Precipitation (in.): ",allDates[1]," to ",allDates[length(allDates)]))+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group\nRCC-ACIS"))+
  theme(plot.title=element_text(size=14, face = "bold"))

p<-p+geom_path(data = tribes_df, 
               aes(x = long, y = lat, group = group),
               color = 'azure4', size = .2)

p<-p+geom_point(data = SWCities, aes(x = lon, y = lat), size = 1, 
                shape = 20)

p<-p+geom_text(data = SWCities, aes(x = lon, y = lat, label = City), 
               size = 3, col = "black", fontface = "bold", nudge_y = 0.1)

# write out file
png(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_MaxPrecip.png"), width = 16, height = 8, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

# add logos
# Call back the plot
plot <- image_read(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_MaxPrecip.png"))
# And bring in a logo
#logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
logo <- image_resize(logo_raw, geometry_size_percent(width=120,height = 120))
# Stack them on top of each other
#final_plot <- image_append((c(plot, logo)), stack = TRUE)
#final_plot <- image_mosaic((c(plot, logo)))
final_plot <- image_composite(plot, logo, offset = "+510+2150")
# And overwrite the plot without a logo
image_write(final_plot, paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_MaxPrecip.png"))

# leaflet interactive
pal <- colorNumeric(c("lightblue", "dodgerblue3", "palegreen","green4","salmon","orangered3",
                      "lightgoldenrod1","orange2","plum2","purple"), values(maxRain),
                    na.color = "transparent")
crs(maxRain) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

leafMap<-leaflet() %>% addTiles() %>%
  addRasterImage(maxRain, colors = pal, opacity = 0.8, layerId = "Inches") %>%
  addLegend(pal = pal, values = values(maxRain),
            title=paste0("Max 1-day precip:<br> ",allDates[1]," to ",allDates[length(allDates)]))%>%
  addMouseCoordinates() %>%
  addImageQuery(maxRain, type="mousemove", layerId = "Inches", prefix = "")

saveWidget(leafMap, file=paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/leafletMaps/SW_Monsoon_MaxPrecip.html"))

# ----- end MAX DAILY ----

# DAYS SINCE MAP ----
# colorramp for total precip
precipCols<-colorRampPalette(c("deepskyblue3","cyan","palegreen1","seagreen2","khaki","yellow","orange","orangered","red"))(30)
precBreaks<-seq(0,30,2)
precLabs<-as.character(seq(0,30,2))
precLabs[16]<-">30"
precLabs[1]<-"0"
#precBreaksmin<-seq(1,19,2)

#theme_set(theme_bw())

p<-gplot(daysSince) + geom_tile(aes(fill = value)) +
  #scale_fill_gradient2(low = 'white', high = 'blue') +
  #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod", 
  #                     name="inches", limits=c(0,20),oob=squish)+
  
  scale_fill_gradientn(colours = precipCols, na.value="tan4", 
                       name="days", limits=c(0,30),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
  guides(fill= guide_colorbar(barheight=15,nbin = 30, raster = FALSE))+
  
  coord_equal(xlim = c(-115,-102), ylim = c(31,38), expand = FALSE)+
  xlab("Longitude") + ylab("Latitude") 

p<-p +  geom_polygon( data=all_states, aes(x=X, y=Y, group = PID),colour="black", fill=NA, size=0.25 )+
  #scale_x_continuous(breaks = c(-120,-140))+
  #ggtitle("Daily Total Precip (inches) - PRISM")+
  ggtitle(paste0("Days since >0.05 inch rain event: ",allDates[length(allDates)]))+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group\nRCC-ACIS"))+
  theme(plot.title=element_text(size=14, face = "bold"))

p<-p+geom_path(data = tribes_df, 
               aes(x = long, y = lat, group = group),
               color = 'azure4', size = .2)

p<-p+geom_point(data = SWCities, aes(x = lon, y = lat), size = 1, 
                shape = 20)

p<-p+geom_text(data = SWCities, aes(x = lon, y = lat, label = City), 
               size = 3, col = "black", fontface = "bold", nudge_y = 0.1)

# write out file
png(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_DaysSince.png"), width = 16, height = 8, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

# add logos
# Call back the plot
plot <- image_read(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_DaysSince.png"))
# And bring in a logo
#logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
logo <- image_resize(logo_raw, geometry_size_percent(width=120,height = 120))
# Stack them on top of each other
#final_plot <- image_append((c(plot, logo)), stack = TRUE)
#final_plot <- image_mosaic((c(plot, logo)))
final_plot <- image_composite(plot, logo, offset = "+510+2150")
# And overwrite the plot without a logo
image_write(final_plot, paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_DaysSince.png"))

# leaflet interactive
pal <- colorNumeric(c("deepskyblue3","cyan","palegreen1","seagreen2","khaki","yellow","orange","orangered","red"), c(0,max(values(daysSince),na.rm = TRUE)+1),
                    na.color = "transparent")

crs(daysSince) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

leafMap<-leaflet() %>%
  setView(lng = -109.046436, lat = 34.341280, zoom = 07) %>%
  addTiles() %>%
  addRasterImage(daysSince, colors = pal, opacity = 0.8, layerId = "Days") %>%
  addLegend(pal = pal, values = values(daysSince),
            title=paste0("Days since >0.05 in:<br> ",allDates[length(allDates)]))%>%
  addMouseCoordinates() %>%
  addImageQuery(daysSince, type="mousemove", layerId = "Days", prefix = "")
  #addControl(title, position = "topleft", className="map-title")
  #addLogo("https://cals.arizona.edu/climate/misc/UA_CSAP_CLIMAS_logos.png", src = "remote",
  #        position="bottomleft",width=210, height=129)

saveWidget(leafMap, file=paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/leafletMaps/SW_Monsoon_DaysSince.html"))
# END DAYS SINCE ----

# LATEST DAILY precip Map -----
# colorramp for total precip
precipCols<-colorRampPalette(c("lightblue", "dodgerblue3", "palegreen","green4","salmon","orangered3",
                               "lightgoldenrod1","orange2","plum2","purple"))(50)
precBreaks<-seq(0,6,0.5)
precLabs<-as.character(seq(0,6,0.5))
precLabs[13]<-">6"
precLabs[1]<-"0.01"
#precBreaksmin<-seq(1,19,2)

#theme_set(theme_bw())

p<-gplot(gridStack[[max(nlayers(gridStack))]]) + geom_tile(aes(fill = value)) +
  #scale_fill_gradient2(low = 'white', high = 'blue') +
  #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod", 
  #                     name="inches", limits=c(0,20),oob=squish)+
  
  scale_fill_gradientn(colours = precipCols, na.value="darkgoldenrod", 
                       name="inches", limits=c(0,6),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
  guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+
  
  coord_equal(xlim = c(-115,-102), ylim = c(31,38), expand = FALSE)+
  xlab("Longitude") + ylab("Latitude") 

p<-p +  geom_polygon( data=all_states, aes(x=X, y=Y, group = PID),colour="black", fill=NA, size=0.25 )+
  #scale_x_continuous(breaks = c(-120,-140))+
  #ggtitle("Daily Total Precip (inches) - PRISM")+
  ggtitle(paste0("Total Precipitation (in.): ",allDates[length(allDates)]))+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group\nRCC-ACIS"))+
  theme(plot.title=element_text(size=14, face = "bold"))

p<-p+geom_path(data = tribes_df, 
               aes(x = long, y = lat, group = group),
               color = 'azure4', size = .2)

p<-p+geom_point(data = SWCities, aes(x = lon, y = lat), size = 1, 
                shape = 20)

p<-p+geom_text(data = SWCities, aes(x = lon, y = lat, label = City), 
               size = 3, col = "black", fontface = "bold", nudge_y = 0.1)

# write out file
png(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_LatestDay.png"), width = 16, height = 8, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

# add logos
# Call back the plot
plot <- image_read(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_LatestDay.png"))
# And bring in a logo
#logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
logo <- image_resize(logo_raw, geometry_size_percent(width=120,height = 120))
# Stack them on top of each other
#final_plot <- image_append((c(plot, logo)), stack = TRUE)
#final_plot <- image_mosaic((c(plot, logo)))
final_plot <- image_composite(plot, logo, offset = "+510+2150")
# And overwrite the plot without a logo
image_write(final_plot, paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_LatestDay.png"))

# leaflet interactive
pal <- colorNumeric(c("lightblue", "dodgerblue3", "palegreen","green4","salmon","orangered3",
                      "lightgoldenrod1","orange2","plum2","purple"), values(gridStack[[max(nlayers(gridStack))]]),
                    na.color = "transparent")
crs(gridStack) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

leafMap<-leaflet() %>% addTiles() %>%
  addRasterImage(gridStack[[max(nlayers(gridStack))]], colors = pal, opacity = 0.8, layerId = "Inches") %>%
  addLegend(pal = pal, values = values(gridStack[[max(nlayers(gridStack))]]),
            title=paste0("Total precip:<br> ",allDates[length(allDates)]))%>%
  addMouseCoordinates() %>%
  addImageQuery(gridStack[[max(nlayers(gridStack))]], type="mousemove", layerId = "Inches", prefix = "")

saveWidget(leafMap, file=paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/leafletMaps/SW_Monsoon_LatestDay.html"))

# ----- end LATEST DAILY ----

# PLOT ALL DAYS THUMBNAILS ----
# colorramp for total precip
precipCols<-colorRampPalette(c("lightblue", "dodgerblue3", "palegreen","green4","salmon","orangered3",
                               "lightgoldenrod1","orange2","plum2","purple"))(50)
precBreaks<-seq(0,6,0.5)
precLabs<-as.character(seq(0,6,0.5))
precLabs[13]<-">6"
precLabs[1]<-"0.01"
#precBreaksmin<-seq(1,19,2)

#theme_set(theme_bw())
names(gridStack)<-format(allDates, format="%b %d %Y")
p<-gplot(gridStack) + geom_tile(aes(fill = value)) +
  #scale_fill_gradient2(low = 'white', high = 'blue') +
  #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod", 
  #                     name="inches", limits=c(0,20),oob=squish)+
  facet_wrap(~ variable) +
  scale_fill_gradientn(colours = precipCols, na.value="darkgoldenrod", 
                       name="inches", limits=c(0,6),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
  guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+
  
  coord_equal(xlim = c(-115,-102), ylim = c(31,38), expand = FALSE)+
  xlab("Longitude") + ylab("Latitude") 

p<-p +  geom_polygon( data=states, aes(x=X, y=Y, group = PID),colour="grey", fill=NA, size=0.25 )+
  #scale_x_continuous(breaks = c(-120,-140))+
  #ggtitle("Daily Total Precip (inches) - PRISM")+
  ggtitle(paste0("Daily Precipitation (in.): ",allDates[1]," to ",allDates[length(allDates)]))+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group\nRCC-ACIS"))+
  theme(plot.title=element_text(size=14, face = "bold"))+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# write out file
png(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_AllDaysGrid.png"),width = 16, height = 10, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

# add logos
# Call back the plot
plot <- image_read(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_AllDaysGrid.png"))
# And bring in a logo
#logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
logo <- image_resize(logo_raw, geometry_size_percent(width=120,height = 120))
# Stack them on top of each other
#final_plot <- image_append((c(plot, logo)), stack = TRUE)
#final_plot <- image_mosaic((c(plot, logo)))
final_plot <- image_composite(plot, logo, offset = "+410+2760")
# And overwrite the plot without a logo
image_write(final_plot, paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/SW_Monsoon_AllDaysGrid.png"))
# END PLOT ALL DAYS THUMBNAILS ----

# PLOT DAILIES ----
# inDaily<-list.files("/home/crimmins/RProjects/ClimPlot/monsoonMaps/daily")
# if(length(inDaily)==0){
#   j=1
# }else{
#   j=length(inDaily)
# }
# 
# if(j>length(allDates)){
#   j=length(allDates)
# }

# colorramp for total precip
precipCols<-colorRampPalette(c("lightblue", "dodgerblue3", "palegreen","green4","salmon","orangered3",
                               "lightgoldenrod1","orange2","plum2","purple"))(50)
precBreaks<-seq(0,6,0.5)
precLabs<-as.character(seq(0,6,0.5))
precLabs[13]<-">6"
precLabs[1]<-"0.01"
#precBreaksmin<-seq(1,19,2)

# loop through days, change j to 1
for(i in 1:length(allDates)){
    p<-gplot(gridStack[[i]]) + geom_tile(aes(fill = value)) +
      #scale_fill_gradient2(low = 'white', high = 'blue') +
      #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod", 
      #                     name="inches", limits=c(0,20),oob=squish)+
      
      scale_fill_gradientn(colours = precipCols, na.value="darkgoldenrod", 
                           name="inches", limits=c(0,6),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
      guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+
      
      coord_equal(xlim = c(-115,-102), ylim = c(31,38), expand = FALSE)+
      xlab("Longitude") + ylab("Latitude") 
    
    p<-p +  geom_polygon( data=all_states, aes(x=X, y=Y, group = PID),colour="black", fill=NA, size=0.25 )+
      #scale_x_continuous(breaks = c(-120,-140))+
      #ggtitle("Daily Total Precip (inches) - PRISM")+
      ggtitle(paste0("Total Precipitation (in.): ",allDates[i]))+
      labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                          "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group\nRCC-ACIS"))+
      theme(plot.title=element_text(size=14, face = "bold"))
    
    p<-p+geom_path(data = tribes_df, 
                   aes(x = long, y = lat, group = group),
                   color = 'azure4', size = .2)
    
    p<-p+geom_point(data = SWCities, aes(x = lon, y = lat), size = 1, 
                    shape = 20)
    
    p<-p+geom_text(data = SWCities, aes(x = lon, y = lat, label = City), 
                   size = 3, col = "black", fontface = "bold", nudge_y = 0.1)
    
    # write out file
    dayFileName<-paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/daily/SW_Monsoon_Precip_",allDates[i],".png")
      png(dayFileName, width = 16, height = 8, units = "in", res = 300L)
    #grid.newpage()
    print(p, newpage = FALSE)
    dev.off()
    
    # add logos
    # Call back the plot
    plot <- image_read(dayFileName)
    # And bring in a logo
    #logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
    logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
    logo <- image_resize(logo_raw, geometry_size_percent(width=120,height = 120))
    # Stack them on top of each other
    #final_plot <- image_append((c(plot, logo)), stack = TRUE)
    #final_plot <- image_mosaic((c(plot, logo)))
    final_plot <- image_composite(plot, logo, offset = "+510+2150")
    # And overwrite the plot without a logo
    image_write(final_plot, dayFileName)
}

# CLEAN OUT DAILY DIR - DANGER
#plots <- list.files("./monsoonMaps/daily/")
#file.remove(paste0("./monsoonMaps/daily/",plots))

# END PLOT DAILIES ----

# RMARKDOWN of DAILIES ----
# create Rmarkdown html of daily maps
#file.remove("/home/crimmins/RProjects/ClimPlot/monsoonMaps/dailyPrecip.html")
render('/home/crimmins/RProjects/ClimPlot/createThumbsHTML_historical.Rmd', output_file='dailyPrecip.html',
       output_dir=paste0('/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/',yearDir), clean=TRUE)
  plots <- list.files(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/daily/"))
  file.remove(paste0("/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/",yearDir,"/daily/",plots))

# ----

# create Website with markdown ----
  render(paste0('/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/',yearDir,'/SWmonsoonTemplateHistorical.Rmd'), output_file='index.html',
         output_dir=paste0('/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/',yearDir), clean=TRUE)
# ----  
  
}  
  
# OUTSIDE OF LOOP -----  
# trying mapview
#names(gridStack)<-allDates
#test<-mapView(gridStack, col.regions = precipCols, at = precBreaks, legend = TRUE, na.color="transparent")
# mapshot(test, url="/home/crimmins/RProjects/ClimPlot/monsoonprecip.html")



# RENDER NAV PAGE
render('/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical/createHistoricalMonsoonNav.Rmd', output_file='past_SWUS_monsoon.html',
       output_dir=paste0('/home/crimmins/RProjects/ClimPlot/monsoonMapshistorical'), clean=TRUE)

# CREATE CLIMATOLOGIES ----
# # monthly values
# # colorramp for total precip
# precipCols<-colorRampPalette(c("lightblue", "dodgerblue3", "palegreen","green4","salmon","orangered3",
#                                "lightgoldenrod1","orange2","plum2","purple"))(50)
# precBreaks<-seq(0,9,1)
# precLabs<-as.character(seq(0,9,1))
# precLabs[10]<-">9"
# precLabs[1]<-"0.01"
# #precBreaksmin<-seq(1,19,2)
# 
# #theme_set(theme_bw())
# names(JJASppt)<-c("June","July","August","September")
# p<-gplot(JJASppt) + geom_tile(aes(fill = value)) +
#   #scale_fill_gradient2(low = 'white', high = 'blue') +
#   #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod", 
#   #                     name="inches", limits=c(0,20),oob=squish)+
#   facet_wrap(~ variable)+
#   scale_fill_gradientn(colours = precipCols, na.value="darkgoldenrod", 
#                        name="inches", limits=c(0,9),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
#   guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+
#   
#   coord_equal(xlim = c(-115,-102), ylim = c(31,38), expand = FALSE)+
#   xlab("Longitude") + ylab("Latitude") 
# 
# p<-p +  geom_polygon( data=all_states, aes(x=X, y=Y, group = PID),colour="black", fill=NA, size=0.25 )+
#   #scale_x_continuous(breaks = c(-120,-140))+
#   #ggtitle("Daily Total Precip (inches) - PRISM")+
#   ggtitle("Monthly Average Precipitation (1981-2010)")+
#   labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
#                       "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group"))+
#   theme(plot.title=element_text(size=14, face = "bold"))
# 
# p<-p+geom_path(data = tribes_df, 
#                aes(x = long, y = lat, group = group),
#                color = 'azure4', size = .2)
# 
# p<-p+geom_point(data = SWCities, aes(x = lon, y = lat), size = 1, 
#                 shape = 20)
# 
# p<-p+geom_text(data = SWCities, aes(x = lon, y = lat, label = City), 
#                size = 2, col = "black", fontface = "bold", nudge_y = 0.15)
# 
# # write out file
# png("/home/crimmins/RProjects/ClimPlot/monsoonMaps/climatology/SW_Monsoon_JJAS_Precip_Climatology.png", width = 16, height = 8, units = "in", res = 300L)
# #grid.newpage()
# print(p, newpage = FALSE)
# dev.off()
# 
# # add logos
# # Call back the plot
# plot <- image_read("/home/crimmins/RProjects/ClimPlot/monsoonMaps/climatology/SW_Monsoon_JJAS_Precip_Climatology.png")
# # And bring in a logo
# #logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
# logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
# logo <- image_resize(logo_raw, geometry_size_percent(width=120,height = 120))
# # Stack them on top of each other
# #final_plot <- image_append((c(plot, logo)), stack = TRUE)
# #final_plot <- image_mosaic((c(plot, logo)))
# final_plot <- image_composite(plot, logo, offset = "+620+2150")
# # And overwrite the plot without a logo
# image_write(final_plot, "/home/crimmins/RProjects/ClimPlot/monsoonMaps/climatology/SW_Monsoon_JJAS_Precip_Climatology.png")
# 
# # Seasonal TOTAL
# # colorramp for total precip
# totalJJAS<-calc(JJASppt, sum, na.rm=TRUE)
# precipCols<-colorRampPalette(c("lightblue", "dodgerblue3", "palegreen","green4","salmon","orangered3",
#                                "lightgoldenrod1","orange2","plum2","purple"))(50)
# precBreaks<-seq(0,20,2)
# precLabs<-as.character(seq(0,20,2))
# precLabs[11]<-">20"
# precLabs[1]<-"0.01"
# #precBreaksmin<-seq(1,19,2)
# 
# #theme_set(theme_bw())
# p<-gplot(totalJJAS) + geom_tile(aes(fill = value)) +
#   #scale_fill_gradient2(low = 'white', high = 'blue') +
#   #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod", 
#   #                     name="inches", limits=c(0,20),oob=squish)+
#   scale_fill_gradientn(colours = precipCols, na.value="darkgoldenrod", 
#                        name="inches", limits=c(0,20),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
#   guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+
#   
#   coord_equal(xlim = c(-115,-102), ylim = c(31,38), expand = FALSE)+
#   xlab("Longitude") + ylab("Latitude") 
# 
# p<-p +  geom_polygon( data=all_states, aes(x=X, y=Y, group = PID),colour="black", fill=NA, size=0.25 )+
#   #scale_x_continuous(breaks = c(-120,-140))+
#   #ggtitle("Daily Total Precip (inches) - PRISM")+
#   ggtitle("Average Seasonal (Jun-Jul-Aug-Sep) Total Precipitation (1981-2010)")+
#   labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
#                       "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group"))+
#   theme(plot.title=element_text(size=14, face = "bold"))
# 
# p<-p+geom_path(data = tribes_df, 
#                aes(x = long, y = lat, group = group),
#                color = 'azure4', size = .2)
# 
# p<-p+geom_point(data = SWCities, aes(x = lon, y = lat), size = 1, 
#                 shape = 20)
# 
# p<-p+geom_text(data = SWCities, aes(x = lon, y = lat, label = City), 
#                size = 3, col = "black", fontface = "bold", nudge_y = 0.1)
# 
# # write out file
# png("/home/crimmins/RProjects/ClimPlot/monsoonMaps/climatology/SW_Monsoon_Seasonal_Precip_Climatology.png", width = 16, height = 8, units = "in", res = 300L)
# #grid.newpage()
# print(p, newpage = FALSE)
# dev.off()
# 
# # add logos
# # Call back the plot
# plot <- image_read("/home/crimmins/RProjects/ClimPlot/monsoonMaps/climatology/SW_Monsoon_Seasonal_Precip_Climatology.png")
# # And bring in a logo
# #logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
# logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
# logo <- image_resize(logo_raw, geometry_size_percent(width=120,height = 120))
# # Stack them on top of each other
# #final_plot <- image_append((c(plot, logo)), stack = TRUE)
# #final_plot <- image_mosaic((c(plot, logo)))
# final_plot <- image_composite(plot, logo, offset = "+490+2150")
# # And overwrite the plot without a logo
# image_write(final_plot, "/home/crimmins/RProjects/ClimPlot/monsoonMaps/climatology/SW_Monsoon_Seasonal_Precip_Climatology.png")

  
  
  
