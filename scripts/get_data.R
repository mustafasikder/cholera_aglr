setwd("X:/Spatial Stat/climate_cholera/cholera_aglr/")
#Countries to consider: 
# 
rm(list= ls())
library(rnoaa)
library(data.table)
library(ggplot2)
library(sf)
library(raster)
library(rgdal)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(precintcon)
library(pryr)
library(fuzzyjoin)
library(exactextractr)
library(ggthemes)

theme_set(theme_minimal())
theme_update(legend.position = "bottom")
options(ggplot2.continuous.colour="viridis")
options(ggplot2.continuous.fill = "viridis")

par(mar=c(1,1,1,1))

out_dir<- 'X:/Spatial Stat/climate_cholera/cholera_aglr/results/'


## Countries to consider: 9 as suggested by Dr. Anyamba
country_vector<- c('Kenya', 'Uganda', 'Tanzania', 'South Sudan', 'Somalia', 
                   'Ethiopia', 'Zambia', 'Malawi', 
                   'DRC')

# great_lakes<- c( 'Democratic Republic of the Congo', 'Ethiopia', 'Kenya', 'Malawi', 
#                  'Mozambique', 'Rwanda', 'Zambia', 'Tanzania', 'Uganda')

# read cholera data
chl_dt_raw<- fread("X:/Spatial Stat/climate_cholera/data/Africa_El_Nino_Cases_April 2015-present.csv")
# view cholera data by country
chl_dt_raw[Disease=="CHL", .N , by= Country]
chl_dt_raw[Country=="Democratic Republic of the Congo", Country:="DRC"]
# getting cholera data
chl_dt<- chl_dt_raw[Disease=="CHL", ]
# subset target country data
chl_dt<- chl_dt[Country %in% country_vector,]


# format date and confirmed cases 
chl_dt$Start_Date<- as.Date(chl_dt$Start_Date, "%m/%d/%Y")
chl_dt$End_Date<- as.Date(chl_dt$End_Date, "%m/%d/%Y")
chl_dt$End_Date<- format(chl_dt$End_Date, "%Y-%m-%d")
chl_dt$End_Date<- as.Date(chl_dt$End_Date)

chl_dt$Confirmed_Cases<- as.numeric(chl_dt$Confirmed_Cases)

### correct long-lat data
# manually remove NA and any other unrealistic coordinate
# for unrealistic coordinate, use the Country and Admin_Level info from the data
# to get the correct coordinate. Use the map plotting section below to check the
# consistency of the long-lat info in the data. 
is.numeric(chl_dt$Long)
is.numeric(chl_dt$Lat)
chl_dt[is.na(as.numeric(Lat)), Lat]
chl_dt[is.na(as.numeric(Lat)), Lat:="-3.5667"]
chl_dt$Lat<- chl_dt[,as.numeric(Lat)]
# Four rows from DRC, Tanganyika are reported as Lat:Long= 37.6722:-97.5585;
# this is likely inaccurate as the coordinate for the province is 
# Lat:Long= -5.916667, 29.2 (Ref: Wikipedia)<- replacing with correct info
chl_dt[Lat==37.6722, Lat:= as.numeric('-5.916667')]
chl_dt[Long== -97.5585, Long:= as.numeric('29.2')]
# One row from South Sudan, Eastern Nile state has Lat= 15.6260 which is 
# outside the country's boundary. Updating that with Upper Nile's Lat= 9.766667
chl_dt[Lat==15.6260, Lat:= as.numeric('9.766667')]

saveRDS(chl_dt, 'results/chl_dt.RDS')



# plotting 
ggplot(data= chl_dt)+ 
  geom_point(aes(x= End_Date, y=log(Confirmed_Cases)))+ 
  facet_wrap(~Country)+ 
  labs(x= "Reporting date", y= 'Log of confirmed cholera cases')+
  theme_light()


# Point size by country plot --- maybe not useful here 
ggplot(data= chl_dt)+ 
  geom_point(aes(x= End_Date, y= Country, size= log(Confirmed_Cases)), pch= 2)


###---- map of cholera data
source('scripts/helper_functions.R')
iso3_vec<- c('KEN', 'UGA', 'SSD', 'SOM', 'ETH', 'ZMB', 'MWI', 'COD', 'TZA')
combined_shp<- get_gadm_shp(iso3_vector = iso3_vec)
combined_shp<- st_as_sf(combined_shp)
saveRDS(object = combined_shp, file = 'results/combined_shp.RDS')
combined_shp<- readRDS("results/combined_shp.RDS")


# change cholera data to sf
chl_sf<- st_as_sf(chl_dt, coords = c("Long", "Lat"), crs= crs(combined_shp))
# chl_sf$year<- as.numeric(format(chl_sf$End_Date, '%Y'))
# chl_sf$season<- ifelse(as.numeric(format(chl_sf$End_Date, '%m')) %in% c(10:12,1:5), "Oct-May", "Jun-Sep")
# chl_sf$season_year<- paste0(chl_sf$season, chl_sf$year)

chl_sf<- mutate(chl_sf, 
                season_year= ifelse(
                  month(End_Date) %in% 1:5, 
                  paste0('oct', year(End_Date)-1, '_', 'may', year(End_Date)), 
                  ifelse( month(End_Date) %in% 6:9,
                          paste0('jun', year(End_Date), '_', 'sep', year(End_Date)), 
                          paste0('oct', year(End_Date), '_', 'may', year(End_Date)+1)))
                
                )
chl_sf$season_year<- factor(chl_sf$season_year, levels= c("oct2014_may2015",
                                                             "jun2015_sep2015", 
                                                             "oct2015_may2016", 
                                                             "jun2016_sep2016", 
                                                             "oct2016_may2017", 
                                                             "jun2017_sep2017", 
                                                             "oct2017_may2018", 
                                                             "jun2018_sep2018", 
                                                             "oct2018_may2019", 
                                                             "jun2019_sep2019"))

ggplot(data= combined_shp)+
  geom_sf(fill= "white")+ 
  geom_tile(data= cumulative_oct_2014_may_2015_df, aes(x= x, y= y, fill=value), alpha= .7)+ 
  geom_sf(data= chl_sf, aes(size= Confirmed_Cases), color= "tomato", alpha= .5)+ 
  facet_wrap(vars(season_year))




# read rainfall data as precipitation (mm)
# https://cran.r-project.org/web/packages/rnoaa/rnoaa.pdf
# read me of Africa Rainfall Climatology version2 (ARC2): https://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/arc2/ARC2_readme.txt
# tz.box <- c(xmin = -7, ymin = 33.4, xmax = -6, ymax = 37.2)
# ea.box<- c(xmin = 10, ymin = -30, xmax = 55, ymax = 20)
# date.frame<- seq(as.Date("2016-01-01"), as.Date("2016-06-30"), "days")

# tz.rf<- arc2(date= "2017-06-14", box = tz.box, )
# ea.rf<- arc2(date= "2017-06-14", box = ea.box)
# ggplot(data= data.frame(ea.rf$`2017-06-14`), aes(x= lon, y= lat, color= precip))+geom_tile()

# ea.rf.2017.06.14<- rasterFromXYZ(ea.rf$`2017-06-14`[,2:4], crs = "+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83")
# plot(ea.rf.2017.06.14)
# To check if the rainfall data includes all target countries

# taz.shp<- getData('GADM', country='TZA', level=0)
# plot(taz.shp, add= T)
# plot(getData('GADM', country='KEN', level=0), add= T)
# plot(getData('GADM', country='UGA', level=0), add= T)
# plot(getData('GADM', country='SSD', level=0), add= T)
# plot(getData('GADM', country='SOM', level=0), add= T)
# plot(getData('GADM', country='ETH', level=0), add= T)
# plot(getData('GADM', country='ZMB', level=0), add= T)
# plot(getData('GADM', country='MWI', level=0), add= T)
# plot(getData('GADM', country='COD', level=0), add= T)




# tz.rf.16.1<- arc2(date= date.frame, box = tz.box, )

# tz.rf.16.1.df<- data.frame(lon= tz.rf.16.1[[1]]$lon, lat= tz.rf.16.1[[1]]$lat, sapply(tz.rf.16.1, function(x) x$precip))

# temp.df<- data.frame(lon= tz.rf[[1]]$lon, lat= tz.rf[[1]]$lat)
# date.frame<- seq(as.Date("2016-01-01"), as.Date("2016-01-31"), "days")
# for (i in date.frame){
#   i<- as.Date(i, origin = "1970-01-01")
#   print(i)
#   tz.temp<- arc2(date= i, box = tz.box)
#   temp.df<- data.frame(temp.df, sapply(tz.temp, function(x) x$precip))
#   colnames(temp.df)[ncol(temp.df)]<- paste0("d",substr(i, 9, 10))
#   file.remove(paste0("C:/Users/sikde/AppData/Local/cache/R/noaa_arc2/",list.files("C:/Users/sikde/AppData/Local/cache/R/noaa_arc2/")))
# }

# for now only write if for each year
source('scripts/helper_functions.R')

arc2015<-get_arc2(2015, area_box = c(xmin = 10, ymin = -30, xmax = 55, ymax = 20))
#ea.rf.2015.01.01<- rasterFromXYZ(arc2015[1:225951,], crs = "+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83")
# 
# arc2016<-get_arc2(2016)
# arc2017<-get_arc2(2017)
# arc2018<-get_arc2(2018)
# arc2019<-get_arc2(2019)
# 
# arc2015.2019<- bind_rows(arc2015, arc2016, arc2017, arc2018, arc2019)


### ------------ get rainfall data as raster stack ----------------###
### Note: saving as raster is more memory efficient; 
### 2015 as dataframe= 859.3mb and as raster is 773.4mb
source('scripts/helper_functions.R')

arc2014_rast<- get_arc2_raster(2014, area_box = c(xmin = 10, ymin = -20, xmax = 53, ymax = 17))
arc2015_rast<- get_arc2_raster(2015, area_box = c(xmin = 10, ymin = -20, xmax = 53, ymax = 17))
arc2016_rast<- get_arc2_raster(2016, area_box = c(xmin = 10, ymin = -20, xmax = 53, ymax = 17))
arc2017_rast<- get_arc2_raster(2017, area_box = c(xmin = 10, ymin = -20, xmax = 53, ymax = 17))
arc2018_rast<- get_arc2_raster(2018, area_box = c(xmin = 10, ymin = -20, xmax = 53, ymax = 17))
arc2019_rast<- get_arc2_raster(2019, area_box = c(xmin = 10, ymin = -20, xmax = 53, ymax = 17))

# stack all objects and save in disk
arc2014_2019_stack<- writeRaster(raster::stack(arc2014_rast, arc2015_rast, arc2016_rast, 
                                  arc2017_rast, arc2018_rast, arc2019_rast), 
                                filename="results/arc2014_2019_stack.tif", 
                                overwrite=TRUE)
arc2015_2019_stack<- writeRaster(raster::stack(arc2015_rast, arc2016_rast, 
                                               arc2017_rast, arc2018_rast, arc2019_rast), 
                                 filename="results/arc2015_2019_stack.tif", 
                                 overwrite=TRUE)



writeRaster(arc2014_rast, 'results/arc2014_rast.tif')
writeRaster(arc2015_rast, 'results/arc2015_rast.tif')
writeRaster(arc2016_rast, 'results/arc2016_rast.tif')
writeRaster(arc2017_rast, 'results/arc2017_rast.tif')
writeRaster(arc2018_rast, 'results/arc2018_rast.tif')
writeRaster(arc2019_rast, 'results/arc2019_rast.tif')

arc2014_rast<- raster::stack('results/arc2014_rast.tif')
arc2015_rast<- raster::stack('results/arc2015_rast.tif')
arc2016_rast<- raster::stack( 'results/arc2016_rast.tif')
arc2017_rast<- raster::stack( 'results/arc2017_rast.tif')
arc2018_rast<- raster::stack( 'results/arc2018_rast.tif')
arc2019_rast<- raster::stack( 'results/arc2019_rast.tif')

# after writing to the disk, remove the files and read if needed





### -------------- get cumulative rainfall ---------------------------###

cumulative_2014_oct_dec<- raster::calc(arc2014_rast[[275:365]], sum)
cumulative_2015_jan_may<- raster::calc(arc2015_rast[[1:115]], sum)
cumulative_2015_oct_dec<- raster::calc(arc2015_rast[[275:365]], sum)
cumulative_2016_jan_may<- raster::calc(arc2016_rast[[1:115]], sum)
cumulative_2016_oct_dec<- raster::calc(arc2016_rast[[275:365]], sum)
cumulative_2017_jan_may<- raster::calc(arc2017_rast[[1:115]], sum)
cumulative_2017_oct_dec<- raster::calc(arc2017_rast[[275:365]], sum)
cumulative_2018_jan_may<- raster::calc(arc2018_rast[[1:115]], sum)
cumulative_2018_oct_dec<- raster::calc(arc2018_rast[[275:365]], sum)
cumulative_2019_jan_may<- raster::calc(arc2019_rast[[1:115]], sum)
cumulative_2019_oct_dec<- raster::calc(arc2019_rast[[275:365]], sum)

# make seasonal cumulative rainfall

cumulative_oct2014_may2015<- cumulative_2014_oct_dec + cumulative_2015_jan_may
cumulative_jun2015_sep2015<- raster::calc(arc2015_rast[[116:274]], sum)
cumulative_oct2015_may2016<- cumulative_2015_oct_dec + cumulative_2016_jan_may
cumulative_jun2016_sep2016<- raster::calc(arc2016_rast[[116:274]], sum)
cumulative_oct2016_may2017<- cumulative_2016_oct_dec + cumulative_2017_jan_may
cumulative_jun2017_sep2017<- raster::calc(arc2017_rast[[116:274]], sum)
cumulative_oct2017_may2018<- cumulative_2017_oct_dec + cumulative_2018_jan_may
cumulative_jun2018_sep2018<- raster::calc(arc2018_rast[[116:274]], sum)
cumulative_oct2018_may2019<- cumulative_2018_oct_dec + cumulative_2019_jan_may
cumulative_jun2019_sep2019<- raster::calc(arc2019_rast[[116:274]], sum)


## making raster as data frame
# cumulative_oct_2014_may_2015_df <- as(cumulative_oct_2014_may_2015, "SpatialPixelsDataFrame")
# cumulative_oct_2014_may_2015_df<- as.data.frame(cumulative_oct_2014_may_2015_df)
# colnames(cumulative_oct_2014_may_2015_df) <- c("value", "x", "y")

# combining all rasters in a data frame
cumulative_list<- lapply(levels(chl_sf$season_year), 
                         function(g) cbind(base::as.data.frame(as(get(paste0('cumulative_', g)), 'SpatialPixelsDataFrame')), 'season_year'=paste0(g)))

cumulative_rainfall_df<- do.call(rbind.data.frame, cumulative_list)




### plot the cumilative rasters with confirmed cholera cases
# ggplot(data= combined_shp)+
#   geom_sf(fill= "white")+ 
#   geom_tile(data= as.data.frame(as(cumulative_oct2014_may2015, 'SpatialPixelsDataFrame')), aes(x= x, y= y, fill=layer), alpha= .7)+ 
#   geom_sf(data= chl_sf%>%filter(season_year=='oct2014_may2015'), aes(size= Confirmed_Cases), color= "tomato", alpha= .5) 
#   
# 
# sapply(levels(chl_sf$season_year), function(g)
#   ggsave(plot = ggplot(data= combined_shp)+
#       geom_sf(fill= "white")+ 
#       geom_tile(data= as.data.frame(as(eval(parse(text = paste0('cumulative_', g))), 'SpatialPixelsDataFrame')), aes(x= x, y= y, fill=layer), alpha= .7)+ 
#       geom_sf(data= chl_sf%>%filter(season_year==g), aes(size= Confirmed_Cases), color= "tomato", alpha= .5), 
#     filename = (paste0('results/',g,'.jpeg')), 
#     width = 5, height = 5, units = "in", dpi= 350)
#   )

## combining sf with rainfall cumulative data 

chl_sf_rainfall<- bind_rows(chl_sf, cumulative_rainfall_df)
chl_sf_rainfall$season_year<- factor(chl_sf_rainfall$season_year, levels= c("oct2014_may2015",
                                                             "jun2015_sep2015", 
                                                             "oct2015_may2016", 
                                                             "jun2016_sep2016", 
                                                             "oct2016_may2017", 
                                                             "jun2017_sep2017", 
                                                             "oct2017_may2018", 
                                                             "jun2018_sep2018", 
                                                             "oct2018_may2019", 
                                                             "jun2019_sep2019"))
saveRDS(chl_sf_rainfall, 'results/chls_sf_rainfall.RDS')
chl_sf_rainfall<-readRDS('results/chls_sf_rainfall.RDS')
# read the shapefile 
combined_shp<- readRDS("results/combined_shp.RDS")
### ploting
facet_plot<-ggplot(data= chl_sf_rainfall)+
  geom_tile(aes(x= x, y= y, fill=layer))+
  geom_sf(data= combined_shp, alpha= 0)+ 
  geom_sf(aes(size= Confirmed_Cases), color= "#2ec4b6", alpha= .5)+ 
  facet_wrap(vars(season_year))+ 
  theme(text = element_text(size=rel(3)),
              strip.text.x = element_text(size=rel(2.5)),
              strip.text.y = element_text(size=rel(2.5)), 
              legend.text = element_text(size = 8))+ 
  scale_fill_viridis_c(option = "inferno", name= "Rainfall(mm)")

ggsave(plot = facet_plot, 
       filename = (paste0('results/facet_plot23.jpeg')), 
       width = 8, height = 5, units = "in", dpi= 800)


.# monthly mean rainfall
mon.mean.2015.2019<- data.frame(arc2015.2019[,1:2] , mon.mean=rowMeans(arc2015.2019[,3:33], na.rm = T))
mon.mean.2015.2019$date<- rep(seq(as.Date("2015-01-01"), as.Date("2019-12-31"), "months"), times= 1, each= 429)
mon.mean.2015.2019.dt<- data.table(mon.mean.2015.2019)

# daily mean rainfall of the full area
arc2015.2019_dt<- data.table(arc2015.2019)
area.mean.2015.2019<- arc2015.2019_dt[,apply(.SD, 2, mean), .SDcols= patterns("d"), by= .(year, month) ]
colnames(area.mean.2015.2019)[3]<- "area.mean"
# remove NA caused by Feb 29, 30, 31 etc.
area.mean.2015.2019<- area.mean.2015.2019[complete.cases(area.mean.2015.2019),]
area.mean.2015.2019$date<- seq(as.Date("2015-01-01"), as.Date("2019-12-31"), "days")[c(1:424, 426:1826)]


# rainfall anomaly index
daily.obj2015<- as.daily(arc2015[,c(35, 34, 3:33)], na.value = NA)
rai.2015<-rai(daily.obj2015)


daily.obj<- as.daily(arc2015.2019, na.value = NA)
rai.2015.2019<-rai(daily.obj)
rai.2015.2019.dt<- data.table(rai.2015.2019)
rai.2015.2019.dt[,mean(rai), by= .(year, month)]
rai.2015.2019.dt$date<- rep(seq(as.Date("2015-01-01"), as.Date("2019-12-31"), "months"), times= 1, each= 429)

ggplot(data = rai.2015.2019.dt[,mean(rai), by= .(year, month)])+geom_line(aes(x= month, y= V1, color= year))

# can't take mean rai to match with cholera data because rai is monthly 
#mean.rai<- rai.2015.2019.dt[,rai.m:=mean(rai, na.rm= T), by= date]
#colnames(mean.rai)[2]<- "rai.m"

chl_tz<- chl_dt[Country=="Tanzania",]
merged.dt<- merge.data.table(area.mean.2015.2019, chl_tz, by.x = "date", by.y = "End_Date", all= TRUE )
# note: more rows then area.mean.2015.2019 becaue in the chl_tz multiple locations were included for the same date e.g.  2018-12-31 had five location so 5 rows for the same date


ggplot()+
  geom_line(data = rai.2015.2019.dt[,mean(rai), by= date], aes(x= date, y= V1, color= year))+ 
  geom_point(data= chl_dt[Country=="Tanzania",], aes(x= End_Date, y=log(Confirmed_Cases)))+
  geom_line(data = mon.mean.2015.2019.dt[, mean(mon.mean), by= date], aes(x=date, y= V1, color= year),size= 2 )+ 
  scale_x_date(date_minor_breaks = "1 month", date_breaks = "6 month", date_labels = "%Y-%b")+ 
  theme(axis.text= element_text(angle = 90), 
        legend.key.size = unit(3, "pt") )

# plot with rainfall moving average and lag 
rain_confirmed<- ggplot()+ 
  geom_point(data= merged.dt, aes(x= date, y= log(Confirmed_Cases)), size= 1.2, color= "#c09369") +
  geom_line(data= merged.dt, aes(x= date, y= lag(rollmean(area.mean, 7, na.pad = T), k= 5)), color= "turquoise")+ 
  scale_x_date(date_minor_breaks = "1 month", date_breaks = "6 month", date_labels = "%Y-%b")+ 
  scale_y_continuous(name= "log of confirmed cases", sec.axis = sec_axis(~ . * 1, name = "Rainfall (mm)"), minor_breaks = NULL)+ 
  theme(axis.text= element_text(angle = 90))
ggsave("X:/Spatial Stat/climate_cholera/rainfallVsconfirmed.jpeg", plot= rain_confirmed, width = 6, height = 2.5, units = "in", dpi= 500)



# plot with rainfall anomaly 
  ggplot()+ 
    geom_point(data= merged.dt, aes(x= date, y= log(Confirmed_Cases),size= 1.1, color= "#c09369")) +
    #geom_line(data= merged.dt, aes(x= date, y= lag(rollmean(area.mean, 7, na.pad = T), k= 5), color= year))+ 
    geom_line(data = rai.2015.2019.dt[,mean(rai), by= date], aes(x= date, y= V1, color= year), size= 1.2, color= "turquoise")+ 
    scale_x_date(date_minor_breaks = "1 month", date_breaks = "6 month", date_labels = "%Y-%b")+ 
    scale_y_continuous(name= "log of confirmed cases", sec.axis = sec_axis(~ . * 1, name = "Rainfall anomalies (mm)")) +
  theme(axis.text= element_text(angle = 90))
  


ggplot(tz.rf.16.1.df)+geom_point(aes(x= lon, y= lat, color= rowMeans(tz.rf.16.1.df[,names(tz.rf.16.1.df)[grep("X", names(tz.rf.16.1.df))]])))


tz.raster<- rasterFromXYZ(tz.rf$`2017-06-14`[, c("lon", "lat", "precip")])


###------------------- investigate the cholera data ----------------------###

chl_dropna_dt<- chl_dt[!is.na(chl_dt$Confirmed_Cases),]
chl_dropna_dt<- chl_dropna_dt%>%select(!c("Imported_From", "Imported_From_Lat",  "Imported_From_Long", "Disease", "Species", "Measuring_Units" ))
chl_dropna_dt%>%group_by(Country)%>%select(Country, Admin_Level_I)%>%summarise(N= n())
chl_dropna_dt%>%group_by(Country, Admin_Level_I, Admin_Level_II, Admin_Level_III)%>%
  select(Country, Admin_Level_I, Admin_Level_II, Admin_Level_III)%>%summarise(N= n())%>%View()

chl_dropna_dt%>%filter(Country=='Somalia')%>%group_by(Admin_Level_I)%>%summarise(N= n())%>%View()

chl_dropna_dt%>%group_by(Country, Admin_Level_I, Admin_Level_II, Admin_Level_III, `Village/Town`, End_Date)%>%
  select(Country, Admin_Level_I, Admin_Level_II, Admin_Level_III, `Village/Town`, End_Date)%>%
  summarise(N= n())%>%View()

chl_dropna_dt%>%filter(Country=='Somalia')%>%ggplot()+ 
  geom_point(aes(x= End_Date, y=log(Confirmed_Cases)))+ 
  labs(x= "Reporting date", y= 'Log of confirmed cholera cases')

chl_dropna_dt%>%ggplot()+ 
  geom_point(aes(x= End_Date, y=log(Confirmed_Cases)))+ 
  labs(x= "Reporting date", y= 'Log of confirmed cholera cases')+ 
  facet_wrap(~Country)+ theme_light()

### make data granularity variable
chl_dropna_dt2<- copy(chl_dropna_dt)

chl_dropna_dt3 <- chl_dropna_dt2 %>%
  mutate(adm_level =  case_when(
    nchar(Admin_Level_I) == 0 &
      nchar(Admin_Level_II) == 0 &
      nchar(Admin_Level_III) == 0 &
      nchar(`Village/Town`) == 0
   ~ 'adm_0', 
   nchar(Admin_Level_I)> 0 &
     nchar(Admin_Level_II) == 0 &
     nchar(Admin_Level_III) == 0 &
     nchar(`Village/Town`) == 0
   ~ 'adm_1', 
   nchar(Admin_Level_I) >= 0 &
     nchar(Admin_Level_II) > 0 &
     nchar(Admin_Level_III) == 0 &
     nchar(`Village/Town`) == 0
   ~ 'adm_2', 
   nchar(Admin_Level_I) >= 0 &
     nchar(Admin_Level_II) >= 0 &
     nchar(Admin_Level_III) > 0 &
     nchar(`Village/Town`) == 0
   ~ 'adm_3', 
   nchar(Admin_Level_I) >= 0 &
     nchar(Admin_Level_II) >= 0 &
     nchar(Admin_Level_III) >= 0 &
     nchar(`Village/Town`) > 0
   ~ 'adm_4'))


View(chl_dropna_dt3%>%select(Country, Admin_Level_I, Admin_Level_II, Admin_Level_III, `Village/Town`,adm_level))

saveRDS(chl_dropna_dt3, 'results/chl_dt_clean.RDS')

cases_country<- ggplot(data= chl_dropna_dt3)+ 
  geom_point(aes(x= End_Date, y= log10(Confirmed_Cases), color= adm_level))+ 
  facet_wrap(vars(Country), scales = 'free_y')+ theme_clean()
  #scale_color_viridis_d(option = "inferno")
ggsave(paste0( out_dir,"cases_b_country_clean.jpeg"), plot= cases_country, width = 6, height = 6, units = "in", dpi= 500)

View(chl_dropna_dt3%>%group_by(Country, adm_level)%>%count())

# Just testing
# drc_adm1<-st_as_sf(getData('GADM', country='COD', level=1))
# 
# unique(drc_adm1$NAME_1)
# unique(chl_dropna_dt3$Admin_Level_I[chl_dropna_dt3$Country=="DRC"])
# 
# chl_drc<- chl_dropna_dt3%>%filter(Country=="DRC")
# chl_drc$NAME_1= chl_drc$Admin_Level_I
# 
# chl_drc%>%filter(adm_level=='adm_1')

## take country means for now

arc2014_rast<- raster('results/arc2014_rast.tif')
spTransform(arc2014_rast, crs(drc_adm0))

drc_adm0<- getData('GADM', country='COD', level=0)


exactextractr::exact_extract(x= arc2014_rast, 
                             y= drc_adm0, 
                             fun= 'mean')



