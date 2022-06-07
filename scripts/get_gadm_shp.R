
### ------------------ Get shapefile from GADM --------------------###
#' @title 
#' @description given a vector of ISO3 country names and admin level, this 
#' function will get corresponding shapefiles 
#' @param iso3_vector a vector of ISO3 country names
#' @param admin_level expected admin level, default 0
#' @example get_gadm_shp(c('KEN', 'UGA', 'SSD', 'SOM', 'ETH', 'ZMB', 'MWI', 'COD', 'TZA'), 1)
#' @note underlying getData function is from Raster package >>> library("raster")


get_gadm_shp<- function(iso3_vector, admin_level= 0){
  shp_list<- lapply(iso3_vector, function(x) getData('GADM', country=x, level=admin_level))
  shp_combined<- do.call(rbind, shp_list)
  return(shp_combined)
}
