# develop precip percentiles from daily PRISM
# MAC 08/17/2019

library(RCurl)
library(jsonlite)
library(raster)

# loop through each and create growing stack of cumulative precip - does not work with webservice ----
# write to file
cumPrecipAll <- stack()
for(year in 2022:2023){
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

    # get cumulative total precip
    # parallell calc
    ptm <- proc.time()
    beginCluster(7)
      tempGrid <- clusterR(gridStack, calc, args=list(fun=cumsum))
      endCluster()
    proc.time() - ptm

# write to file
    writeRaster(tempGrid,filename=paste0("/home/crimmins/RProjects/ClimPlot/monsoonPercs/AZNM_PRISM_Monsoon_",year,"_cumPrecip.grd"), overwrite=TRUE)


# cumPrecipAll <- stack(cumPrecipAll , tempGrid)
print(year)
}
# ----

# load years into stack and reorder into days
setwd("~/RProjects/ClimPlot/monsoonPercs")
allCumSum <- do.call(stack, lapply(list.files(path = "~/RProjects/ClimPlot/monsoonPercs", pattern = "*.grd"), stack))
writeRaster(allCumSum,filename=paste0("/home/crimmins/RProjects/ClimPlot/AZNM_PRISM_Monsoon_cumPrecip_1981_2023.grd"), overwrite=TRUE)

# load allCumSum
allCumSum<-stack("/home/crimmins/RProjects/ClimPlot/AZNM_PRISM_Monsoon_cumPrecip_1981_2018.grd")

doy<-73
doyCumSum<-subset(allCumSum, seq(doy,nlayers(allCumSum)-(108-doy),by=108))

perc.rank<-function(x) trunc(rank(x, ties.method = "average"))/length(x)
percRankPrecip <- calc(doyCumSum, fun=perc.rank)

# explore point time series
y<-32.124101
x<--110.948236

cumSumTS<-t(raster::extract(doyCumSum, cellFromXY(doyCumSum, c(x,y))))
percTS<-t(raster::extract(percRankPrecip, cellFromXY(percRankPrecip, c(x,y))))

percTS<-cbind(seq(1981,2018,1),cumSumTS,percTS)