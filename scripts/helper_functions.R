### Contains all helper functions run the EA Cholera codes 

### ------------ get rainfall data as raster stack ----------------###
#' @description 
#' get_arc2_raster gets rainfall data from ARC2 site and returns as raster stack
#' for a full year
#' @param target_year year as yyyy
#' @param area_box max and min value of longitude and latitude e.g. area_box<- c(xmin = 10, ymin = -30, xmax = 55, ymax = 20)
#' @example get_arc2_raster(2015, area_box= c(xmin = 10, ymin = -30, xmax = 55, ymax = 20))
### Note: saving as raster is more memory efficient; 
### 2015 as dataframe= 859.3mb and as raster is 773.4mb

get_arc2_raster<- function(target_year, area_box){
  print(paste0('Getting rainfall between longitude: ', area_box['xmin'], '-', area_box['xmax'], ' | latitude: ', area_box['ymax'], '-',area_box['ymax']))
  ea_stack_full<- raster::stack()
  for (m in 1:12){
    days.month<- days_in_month(m)
    date.frame<- seq(as.Date(paste0(target_year, "-",m, "-", "01" )), as.Date(paste0(target_year, "-",m, "-",days.month)), "days")
    ea.stack<- raster::stack()
    for (i in date.frame){
      i<- as.Date(i, origin = "1970-01-01")
      print(i)
      ea.temp<- arc2(date= i, box = area_box)
      colnames(ea.temp[[1]])[4]<- paste0('d_',as.Date(i))
      ea.rast<- rasterFromXYZ(ea.temp[[1]][,2:4], crs = "+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83")
      ea.stack<- raster::stack(ea.stack, ea.rast)
      file.remove(paste0("C:/Users/sikde/AppData/Local/cache/R/noaa_arc2/",list.files("C:/Users/sikde/AppData/Local/cache/R/noaa_arc2/")))
    }
    ea_stack_full<- raster::stack(ea_stack_full,ea.stack)
  }
  return(ea_stack_full)
}


### ------------ get rainfall data as dataframe ----------------###
#' @description 
#' get_arc2_df gets rainfall data from ARC2 site and returns as dataframe with 
#' days as column and months  
#' for a full year
#' @param target_year year as yyyy
#' @param area_box max and min value of longitude and latitude e.g. area_box<- c(xmin = 10, ymin = -30, xmax = 55, ymax = 20)
#' @example get_arc2(2015, area_box= c(xmin = 10, ymin = -30, xmax = 55, ymax = 20))
### Note: saving as raster is more memory efficient; 
### 2015 as dataframe= 859.3mb and as raster is 773.4mb

get_arc2<- function(target_year, area_box){
  for (m in 1:12){
    
    temp.df<- arc2(date = as.Date(paste0(target_year, '-01-01')), box = area_box)
    file.remove(paste0("C:/Users/sikde/AppData/Local/cache/R/noaa_arc2/",list.files("C:/Users/sikde/AppData/Local/cache/R/noaa_arc2/")))
    temp.df<- data.frame(lon= temp.df[[1]]$lon, lat= temp.df[[1]]$lat)
    days.month<- days_in_month(m)
    date.frame<- seq(as.Date(paste0(target_year, "-",m, "-", "01" )), as.Date(paste0(target_year, "-",m, "-",days.month)), "days")
    
    for (i in date.frame){
      i<- as.Date(i, origin = "1970-01-01")
      print(i)
      tz.temp<- arc2(date= i, box = area_box)
      temp.df<- data.frame(temp.df, sapply(tz.temp, function(x) x$precip))
      colnames(temp.df)[ncol(temp.df)]<- paste0("d",substr(i, 9, 10))
      file.remove(paste0("C:/Users/sikde/AppData/Local/cache/R/noaa_arc2/",list.files("C:/Users/sikde/AppData/Local/cache/R/noaa_arc2/")))
    }
    temp.df$month<- as.integer(m)
    temp.df$year<- as.integer(target_year)
    assign(paste0("temp.df",m), temp.df)
    print(paste("month:", m))
  }
  temp.df.full<- bind_rows(temp.df1, temp.df2, temp.df3, temp.df4, temp.df5, temp.df6, temp.df7, temp.df8, temp.df9, temp.df10, temp.df11, temp.df12)
  # as.daily.obj<- temp.df.full[,3:ncol(temp.df.full)]
  # as.daily.obj<- as.daily.obj[, c(33, 32, 1:31)]
  return(temp.df.full)#as.daily.obj)
  
}




### ------------------ Get shapefile from GADM --------------------###
#' @title 
#' @description given a vector of ISO3 country names and admin level, this 
#' function will get corresponding shapefiles 
#' @param iso3_vector a vector of ISO3 country names
#' @param admin_level expected admin level, default 0
#' @example 

get_gadm_shp<- function(iso3_vector, admin_level= 0){
  shp_list<- lapply(iso3_vector, function(x) getData('GADM', country=x, level=admin_level))
  shp_combined<- do.call(rbind, shp_list)
  return(shp_combined)
}

