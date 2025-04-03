# develop daily precip Extent/Tenharkel Index for monsoon season 
# code adapted from PRISMPrecipPerc.R
# MAC 08/27/2019

library(RCurl)
library(jsonlite)
library(raster)

#loop through each and create growing stack of cumulative precip - does not work with webservice ----
#write to file
for(year in 2023:2024){
    # create current date
    dateRangeStart=paste0(year,"-06-15")
    dateRangeEnd= paste0(year,"-09-30")

    # generate dates -- keep with PRISM date
    allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)

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

    # read into raster stack
    rasterList<-lapply(matrixList, raster)
    gridStack<-stack(rasterList)
    gridExtent<-extent(min(out$meta$lon), max(out$meta$lon), min(out$meta$lat), max(out$meta$lat))
    gridStack<-setExtent(gridStack, gridExtent, keepres=FALSE, snap=FALSE)
    names(gridStack)<-allDates
    # set 0 and neg to NA
    gridStack[gridStack < 0] <- NA
    ##

# write to file
    writeRaster(gridStack,filename=paste0("/home/crimmins/RProjects/SWMonsoonMaps/monsoonDailyData/AZNM_PRISM_Monsoon_",year,"_dailyPrecip.grd"), overwrite=TRUE)

# cumPrecipAll <- stack(cumPrecipAll , tempGrid)
print(year)
}
#----




