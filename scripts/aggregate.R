library(exactextractr)
library(sf)
library(sp)
library(raster)
library(ggplot2)


theme_set(theme_minimal())
theme_update(legend.position = "bottom")
options(ggplot2.continuous.colour="viridis")
options(ggplot2.continuous.fill = "viridis")


chl_all_admin_sf<- st_read('results/combined_shape.gpkg')

drops<- c("VARNAME_1", "NL_NAME_1", "TYPE_1", "ENGTYPE_1", "CC_1", "HASC_1", "GID_2", "VARNAME_2", "NL_NAME_2", "TYPE_2", "ENGTYPE_2", "CC_2", "HASC_2", "GID_3", "VARNAME_3", "NL_NAME_3", "HASC_3")

chl_all_admin_sf<- chl_all_admin_sf[ , !(names(chl_all_admin_sf) %in% drops)]

ggplot(data = chl_all_admin_sf)+ geom_sf()

# Get rainfall data
arc2014_rast<- raster::stack('results/arc2014_rast.tif')
crs(arc2014_rast)<- CRS('+init=EPSG:4326')

#chl_all_admin_sf$
m1_rain<- exact_extract(x= arc2014_rast[[1]], y= chl_all_admin_sf, fun= 'mean')
