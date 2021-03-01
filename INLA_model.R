

######################
# Charge libraries
######################
library(sp)
library(car)
library(INLA)
library(gstat)
library(curl)
library(spdep)
library(Hmisc)
library(raster)
library(leaflet)
library(GGally)
library(ggplot2)
library(maptools)
library(corrplot)
library(devtools)
library(cowplot)


#########################
#Read data 
#########################
setwd("~/Downloads/Trabajo_grifoni/IBIS/Model_and_Figure_3")
data <- read.csv2("Data_matrix.csv")
str(data)


#################################################################################
#          Read environemnatl data                                          ##
##############################################################################
setwd("~/Downloads/Trabajo_grifoni/IBIS/Envi")
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


######################################
### --- Standardize predictors --- ###
######################################
predictors2<-scale(predictors)
round(apply(values(predictors2), 2, summary), 4)

#######################################################
### --- Create the datasets with the predictors --- ###
#######################################################
data<-cbind(data, extract(predictors2, as.matrix(data[,5:4])))

################################################################################
#Step 1: Check correlation among explicative variables. 
#Variables highly correlated can NOT be used together in the model.
################################################################################
data=na.omit(data)
matrix<-rcorr(as.matrix(data[,c(9:16)]), type = "pearson")

# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

jpeg("Corrplot.jpeg", width = 1862, height = 2194, res = 300)
corrplot(matrix$r, type="lower", tl.col = "black",method="number",
         p.mat = matrix$P, sig.level = 0.05)
dev.off()


################################################################################
#Step 2: Check multicollinearity among variables
################################################################################
setwd("~/Downloads/Trabajo_grifoni/Trabajo_grifoni/IBIS/Model_and_Figure_3")
source("HighstatLib.r")
d=corvif(data[,c(9:17)])
names=c("Temperature","Precipitation","Isothermality","Elevation","NDVI","Aspect",
        "Slope","Wind_speed","NPP")
Variables=cbind(names,d)
# Nuage de points
sp<-ggplot(Variables, aes(x=Variables$GVIF, y=Variables$names, 
          color=Variables$names)) + geom_point()+  xlab("VIF")+ylab("Variables")+theme_bw()
sp

ggsave("VIF_1.jpeg")

d2=corvif(data[,c(10,16,13)])
names2=c("Precipitation","Wind_speed","NDVI")
VariablesModel=cbind(names2,d2)
# Nuage de points
sp<-ggplot(VariablesModel, aes(x=VariablesModel$GVIF, y=VariablesModel$names, 
                          color=VariablesModel$names)) + geom_point()+  xlab("VIF")+ylab("Variables")+theme_bw()
sp

ggsave("VIF_2.jpeg")

################################################################################
#Step 3: Select the Model
################################################################################
### --- Response variable --- ###
source("Bdiclcpomodel_stack.R")
resp=(data$Juv)
data$beta0=rep(1,6400)

#Run all the possible models
variables <- c("beta0","Aspect", "Wind_speed", "NPP","NDVI" ,"Slope", "Precipitation" , 
               "as.factor(Type)" ,"as.factor(Antropic_effect)" ) #add RW2 


### --- Call the function --- ###
models_bin<-Bdiclcpomodel_stack(resp=resp, variables=variables, 
                                datos=data, n=10, #stk.est is the matrix we've justc created, 20 es el umero de los mejores que te guarda y te presenta`
                                family="binomial", #change family according to density distribution of data
                                control.compute = list(config=TRUE, dic=T, cpo=TRUE, waic=TRUE),  #waic is similar to the dic, the lower the number, the better/ for each model it compares each criteria
                                num.threads=3,  #!!especifiy the number or it will use up ll the internal memory of the laptop (3-4 is the normal)
                                control.inla=list(strategy="gaussian"), #siempre se quedaria gaussian! 
                                verbose=T)  #normally it should be TRUE  and it wil give you a list of everything is doing the model, but it can take hours

#See the best 10 model with respect to WAIc and LCPO 
models_bin$`Modelos waic`[1:10,]
models_bin$`Modelos lcpo`[1:10,]


################################################################################
#Step 4: SRun the selected model
################################################################################

grouped_wind=inla.group(data$Wind_speed, n=15)
grouped_ndvi=inla.group(data$NDVI, n=15)
grouped_prec=inla.group(data$Precipitation, n=15)
grouped_lon=inla.group(data$Longitude, n=15)
grouped_lat=inla.group(data$Latitude, n=15)

#hyper.prec <- list(theta = list(prior="pc.prec", 
                               # param = c(1, 0.05)))#allow smaller values prec iid

m2=inla(Juv ~ f(grouped_wind,model="rw2")+
        f(grouped_ndvi,model="rw2")+
        f(Longitude, Latitude,model="iid")+
        f(ID, model="iid")+
        f(grouped_prec,model="rw2")+
        as.factor(Type)+as.factor(Antropic_effect)+
        f(Year, model="ar1"),
        family="binomial", data=data,
        #control.inla      = list(h = 1e-12),
        control.compute=list(cpo =T, waic =T))

summary(m2)
-mean(log(m2$cpo$cpo))

##The CPO is the probability density of an observed response based on the model fit to the rest of the data. Like a eave-one-out cross-validation
## We can ask to the model how many failure have. The perfect model has 0 falire
#check faiures crossvalidation
sum(m2$cpo$failure)

#### info tabela ###
sqrt(mean((m2$summary.fitted.values$mean - data$Juv)^2)) # RMSE Root Mean sqaured error, from 0 to 1 here 0.63


################################################################################
#Step 5: Plot
################################################################################
###Wind
wind_mean=m2$summary.random$grouped_wind$mean
windsup=m2$summary.random$grouped_wind$`0.975quant`
windinf=m2$summary.random$grouped_wind$`0.025quant`
windid=m2$summary.random$grouped_wind$ID
wind=data.frame(wind_mean,windsup,windinf,windid)


g1=ggplot(data=wind,aes(x=windid, y=wind_mean))+
  geom_line(aes(x=windid,y=wind_mean))+
  geom_ribbon(aes(x=windid,ymin=windinf,ymax=windsup),alpha=0.2,fill="darkgreen")+
  ggtitle("A")+
  xlab("Wind Speed (m s-1)")+
  ylab("Effect")+
theme_bw();g1

###Precipitation
prec_mean=m2$summary.random$grouped_prec$mean
precsup=m2$summary.random$grouped_prec$`0.975quant`
precinf=m2$summary.random$grouped_prec$`0.025quant`
precid=m2$summary.random$grouped_prec$ID
prec=data.frame(prec_mean,precsup,precinf,precid)

g2=ggplot(data=prec,aes(x=precid, y=prec_mean))+
  geom_line(aes(x=precid,y=prec_mean))+
  geom_ribbon(aes(x=precid,ymin=precinf,ymax=precsup),alpha=0.2,fill="darkgreen")+
  ggtitle("B")+
  xlab("Precipitation (mm)")+
  ylab("Effect")+
  theme_bw();g2

###NDVI
NDVI_mean=m2$summary.random$grouped_ndvi$mean
NDVIsup=m2$summary.random$grouped_ndvi$`0.975quant`
NDVIinf=m2$summary.random$grouped_ndvi$`0.025quant`
NDVIid=m2$summary.random$grouped_ndvi$ID
NDVI=data.frame(NDVI_mean,NDVIsup,NDVIinf,NDVIid)

g3=ggplot(data=NDVI,aes(x=NDVIid, y=NDVI_mean))+
  geom_line(aes(x=NDVIid,y=NDVI_mean))+
  geom_ribbon(aes(x=NDVIid,ymin=NDVIinf,ymax=NDVIsup),alpha=0.2,fill="darkgreen")+
  ggtitle("C")+
  xlab("NDVI(mm)")+
  ylab("Effect")+
  theme_bw();g3


###Year
Year_mean=m2$summary.random$Year$mean
Yearsup=m2$summary.random$Year$`0.975quant`
Yearinf=m2$summary.random$Year$`0.025quant`
Yearid=m2$summary.random$Year$ID
Year=data.frame(Year_mean,Yearsup,Yearinf,Yearid)

g4=ggplot(data=Year,aes(x=Yearid, y=Year_mean))+
  geom_line(aes(x=Yearid,y=Year_mean))+
  geom_ribbon(aes(x=Yearid,ymin=Yearinf,ymax=Yearsup),alpha=0.2,fill="darkgreen")+
  ggtitle("D")+
  xlab("Year")+
  ylab("Effect")+
  theme_bw();g4

###Latitude
Latitude_mean=m2$summary.random$grouped_lat$mean
Latitudesup=m2$summary.random$grouped_lat$`0.975quant`
Latitudeinf=m2$summary.random$grouped_lat$`0.025quant`
Latitudeid=m2$summary.random$grouped_lat$ID
Latitude=data.frame(Latitude_mean,Latitudesup,Latitudeinf,Latitudeid)

g5=ggplot(data=Latitude,aes(x=Latitudeid, y=Latitude_mean))+
  geom_line(aes(x=Latitudeid,y=Latitude_mean))+
  geom_ribbon(aes(x=Latitudeid,ymin=Latitudeinf,ymax=Latitudesup),alpha=0.2,fill="darkgreen")+
  ggtitle("D")+
  xlab("Latitude")+
  ylab("Effect")+
  theme_bw();g5

###Longitude
Longitude_mean=m2$summary.random$grouped_long$mean
Longitudesup=m2$summary.random$grouped_long$`0.975quant`
Longitudeinf=m2$summary.random$grouped_long$`0.025quant`
Longitudeid=m2$summary.random$grouped_long$ID
Longitude=data.frame(Longitude_mean,Longitudesup,Longitudeinf,Longitudeid)

g6=ggplot(data=Longitude,aes(x=Longitudeid, y=Longitude_mean))+
  geom_line(aes(x=Longitudeid,y=Longitude_mean))+
  geom_ribbon(aes(x=Longitudeid,ymin=Longitudeinf,ymax=Longitudesup),alpha=0.2,fill="darkgreen")+
  ggtitle("D")+
  xlab("Longitude")+
  ylab("Effect")+
  theme_bw();g6

##Fixed factors
fixed=as.data.frame(m2$summary.fixed)
names_fix=c("Intercept","Type_Terc","Type_Ters ", "Anthropic_1",
             "Anthropic_2", "Anthropic_3")
fix=cbind(names_fix,fixed)
#fix$size=c("None", "High","High", "Low")
sp_fix<-ggplot(fix[2:6,], aes(y=fix$`0.025quant`[2:6],  yend = fix$`0.5quant`[2:6],
                              x=fix$names_fix[2:6], 
                              xend=fix$names_fix[2:6])) +  
  geom_errorbar(ymin=fix$`0.025quant`[2:6],  
  ymax = fix$`0.5quant`[2:6])


g7=sp_fix+theme_classic()+xlab("Fixed effects")+
  ylab("CI 95% Estimated coefficients")+ggtitle("E");g7

#Plot all together
jpeg("Figure_3.jpeg", width = 4400, height = 2290, res = 300)
plot_grid(g1,g2,g3,g4,g7)#labels = "AUTO"
dev.off()

