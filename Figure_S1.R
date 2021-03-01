######################
# Charge libraries
######################
library("raster")

#################################################################################
#          Read environemnatl data                                          ##
##############################################################################
setwd("~/Downloads/Trabajo_grifoni/Trabajo_grifoni/IBIS/Envi")
alt <-raster::getData('alt', country='ITA', mask=TRUE)
ext<-extent(7, 11, 38, 41.5)
alt=crop(alt,ext)

Temp=raster("Temp .asc")
Temp=resample(Temp,alt)
Prec=raster("Prec .asc")
Prec=resample(Prec,alt)
Iso=raster("Iso .asc")
Iso=resample(Iso,alt)
Wind=raster("Wind.asc")
Wind=resample(Wind,alt)
ndvi=raster("ndvi.asc")
ndvi=resample(ndvi,alt)
NPP=raster("Primary.asc")
NPP=resample(NPP,alt)
slope<- terrain(alt,opt='slope', unit='degrees',neighbors = 8)
aspect<- terrain(alt,opt='aspect', unit='degrees',neighbors = 8)

predictors=stack(Temp, Prec, Iso, alt, ndvi,aspect,slope,Wind,NPP)
ext<-extent(7, 11, 38.5, 41.5)
predictors=crop(predictors, ext)
names(predictors)=c("Temperature","Precipitation","Isothermality","Elevation","NDVI","Aspect","Slope","Wind_speed","NPP")

italy <- getData('GADM',country="ITA",level=0)

setwd("~/Downloads/Trabajo_grifoni/Trabajo_grifoni/IBIS/Appendix")
jpeg("Figure_S1.jpeg", width = 1862, height = 1294, res = 300)
plot(predictors)
dev.off()
