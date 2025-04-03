# calculate daily extent of precip 
# MAC 08/27/19

# run ./ClimPlot/downloadDailyPRISM_monsoon.R to update files

#library(raster)
library(plyr)
library(tidyr)
library(cowplot)
library(caTools)
library(sf)
library(terra)

# replacement for getData
us <- geodata::gadm(country = "USA", level = 1, path = tempdir(), resolution = 2)
state<-us[us$NAME_1=="New Mexico",]
#state<-us[us$NAME_1=="Arizona",]
state<-makeValid(state)

#us <- getData("GADM", country="USA", level=1)
# extract states (need to uppercase everything)
#state<-subset(us, NAME_1=="New Mexico")
#state<-subset(us, NAME_1=="Arizona")
#state<-subset(us, NAME_1 %in% c("New Mexico","Arizona"))

dailyGT0<-NULL

# precip threshold
thresh<-0.01

#### OLD RASTER CODE ----
# for(year in 1981:2024){
#   currYear<-stack(paste0("/home/crimmins/RProjects/SWMonsoonMaps/monsoonDailyData/AZNM_PRISM_Monsoon_",year,"_dailyPrecip.grd"))
#   # mask to AZ
#   currYearClip <- mask(currYear, state)
#   # get counts
#   temp1<-as.data.frame(cellStats(currYearClip, function(i, ...) sum(i>=thresh, na.rm = TRUE)))
#   # build time series  
#   dailyGT0 = rbind(dailyGT0, temp1)
# print(year) 
# }
#####

##### TERRA BASED code
for (year in 1981:2024) {
  # Load SpatRaster stack
  file_path <- paste0("/home/crimmins/RProjects/SWMonsoonMaps/monsoonDailyData/AZNM_PRISM_Monsoon_", year, "_dailyPrecip.grd")
  currYear <- rast(file_path)
  # (Optional) Manually assign CRS if warning is thrown
  crs(currYear) <- "EPSG:4326"  # Adjust as needed
  # Mask to AZ
  currYearClip <- mask(currYear, state)
  # For each layer (day), count how many cells exceed the threshold
  temp1 <- global(currYearClip >= thresh, fun = "sum", na.rm = TRUE, wide = TRUE)
  # Add year and bind to time series
  dailyGT0 <- rbind(dailyGT0, temp1)
  print(year)
}
#####

# get base count
tempGrid <- mask(currYear[[1]], state)
#baseCount<-cellStats(tempGrid, function(i, ...) sum(i>=0, na.rm = TRUE))
baseCount<-global(tempGrid>=0, fun = "sum", na.rm = TRUE, wide = TRUE)

# get dates from col names
colnames(dailyGT0)<-"counts"
dailyGT0$percExt<-(dailyGT0$counts/baseCount$sum[1])*100
dailyGT0$dates<-rownames(dailyGT0)
dailyGT0<-separate(dailyGT0,dates, c(NA,"year",NA,"month",NA,"day"),sep=c(1,5,6,8,9,11), convert = TRUE)

# build date and doy
dailyGT0$date<-as.Date(paste0(dailyGT0$year,"-",dailyGT0$month,"-",dailyGT0$day), format="%Y-%m-%d")
dailyGT0$doy<-as.numeric(format(dailyGT0$date, "%j"))
# dummy date var
dailyGT0$dummyDate<-as.Date(paste0(2000,"-",dailyGT0$month,"-",dailyGT0$day), format="%Y-%m-%d")

# doy stats
# calculate stats by day
doyStats<- ddply(dailyGT0,.(dummyDate),summarise,
                 q25 = quantile(percExt,0.25,na.rm='TRUE'),
                 q50 = quantile(percExt,0.50,na.rm='TRUE'),
                 q75 = quantile(percExt,0.75,na.rm='TRUE'),
                 avg = mean(percExt,na.rm='TRUE'))
doyStats$q50smooth<-runmean(doyStats$q50, 15, align = "center", endrule = "mean")

# save doyStats for plots in historical/realtime
#AZ_doyStats<-doyStats
#save(AZ_doyStats, file="/home/crimmins/RProjects/SWMonsoonMaps/data/AZ_doyStats.RData")
# NM
NM_doyStats<-doyStats
save(NM_doyStats, file="/home/crimmins/RProjects/SWMonsoonMaps/data/NM_doyStats.RData")
#

# join stats to data table
doyStats$doy<-as.numeric(format(doyStats$dummyDate, "%j"))
tempGT0<-merge(dailyGT0, doyStats, by="doy")
tempGT0$abvQ50<-ifelse(tempGT0$percExt>=tempGT0$q50smooth,1,0) # monsoon days
# summarize by year
library(dplyr)
yearlyStats<-tempGT0 %>%
  group_by(year) %>%
  summarise(totalMonsoonDays = sum(abvQ50),
            avgExtent= mean(percExt),
            medExtent=median(percExt))

write.csv(yearlyStats, file = "AZ_Extent.csv")

# plot monsoon days per year
library(ggplot2)
ggplot(yearlyStats, aes(year,totalMonsoonDays))+
  geom_bar(stat = "identity")+
  ggtitle("Total Monsoon Days - New Mexico (June 15th-Sept 30th, 1981-2022)")+
  ylab("days")+
  theme_bw()

ggplot(yearlyStats, aes(year,medExtent))+
  geom_bar(stat = "identity")+
  ggtitle("Median Daily Precipitation Coverage - New Mexico (June 15th-Sept 30th, 1981-2022)")+
  ylab("% coverage")+
  theme_bw()




# plot of daily monsoon extent by year
p<-ggplot(dailyGT0, aes(dummyDate,percExt))+
  geom_bar(stat = "identity", fill="darkgreen")+
  facet_wrap(~year, ncol = 5, nrow = 8)+
  xlab("Day of Year")+
  ylab("Percent Coverage - NM")+
  ggtitle("Percent of New Mexico observing >=0.01 inches daily total precipitation, June 15th-Sep 30th (PRISM 1981-2019) ")
p<-p+geom_line(data=doyStats, aes(dummyDate,q50smooth))+
  geom_label(data = yearlyStats, aes(label=totalMonsoonDays), 
             x = -Inf, y = Inf, hjust=0, vjust=1,
             inherit.aes = FALSE)+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group\nRCC-ACIS"))
  

png("/home/crimmins/RProjects/ClimPlot/NMMonsoonDays_allYears.png", width = 11, height = 8.5, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
## now add the text 
dev.off()

# plot single year
temp<-subset(dailyGT0, year==2019)
p<-ggplot(temp, aes(dummyDate,percExt))+
  geom_bar(stat = "identity", fill="darkgreen")+
  #facet_wrap(~year, ncol = 5, nrow = 8)+
  xlab("Day of Year")+
  ylab("Percent Coverage - AZ")+
  ylim(0,100)+
  ggtitle("Percent of Arizona observing >=0.01 inches during Monsoon Season (PRISM 1981-2019) ")
p<-p+geom_line(data=doyStats, aes(dummyDate,q50smooth))+
  geom_label(data = yearlyStats, aes(label=totalMonsoonDays), 
             x = -Inf, y = Inf, hjust=0, vjust=1,
             inherit.aes = FALSE)

png("/home/crimmins/RProjects/ClimPlot/AZMonsoonDays_2019.png", width = 11, height = 8.5, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
## now add the text 
dev.off()


# add climo plot
library(reshape)
doyStats <- doyStats[-c(7)]
doyStatsMelt<-melt(doyStats, id.vars = "dummyDate")
ggplot(doyStatsMelt, aes(dummyDate,value,color=variable))+
  geom_line()+
  xlab("Day of Year")+
  ylab("Percent Coverage - AZ")+
  ggtitle("Climatology of Percent of New Mexico observing >=0.05 (PRISM 1981-2019) ")+
  labs(color = "quantiles")

