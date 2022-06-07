
from math import comb
import pandas as pd
import geopandas as gpd
import fiona; help(fiona.open)
import os
import rasterio
from rasterio.plot import show
from rasterstats import zonal_stats

import datetime as dt
#import gdal
from osgeo import gdal
from osgeo import ogr
from osgeo import osr
from osgeo import gdal_array
from osgeo import gdalconst
gdal.UseExceptions()

import matplotlib.pyplot as plt


## chaning option to display all columns
pd.set_option('display.max_columns',None)


os.chdir('X:/Spatial Stat/climate_cholera')
os.listdir('data/')
ch_df= pd.read_csv('data/Africa_El_Nino_Cases_April 2015-present.csv', parse_dates=["End_Date"])

ch_df.head()
ch_df.info()
ch_df.shape


chl_df= ch_df[ch_df['Disease']== "CHL"]

chl_df.groupby('Disease')
set(chl_df["Country"])
### subset to east Africa countries
country_vector= ['Kenya', 'Uganda', 'Tanzania', 'South Sudan', 'Somalia', 'Ethiopia', 'Zambia', 'Malawi', 'DRC']

chl_df.loc[chl_df.Country == "Democratic Republic of the Congo", "Country"]= "DRC"

chl_df= chl_df[chl_df["Country"].isin(country_vector)]

# convet to datetime object
chl_df.loc[: , "end_date"]= pd.to_datetime(chl_df.End_Date)



combined_shape= gpd.read_file('X:/Spatial Stat/climate_cholera/cholera_aglr/results/combined_shape.shp')
combined_shape.crs

driver= gdal.GetDriverByName("GPKG")
driver.Register()
#combined_shape= gpd.read_file('X:/Spatial Stat/climate_cholera/cholera_aglr/results/combined_shape.gpkg')
#combined_shape.crs
combined_shape= gdal.Open('X:/Spatial Stat/climate_cholera/cholera_aglr/results/combined_shape.gpkg')

combined_shape.shape
combined_shape.info()


### read the rasters
#arc2014= rasterio.open("cholera_aglr/results/arc2014_rast.tif")
arc2014= gdal.Open("cholera_aglr/results/arc2014_rast.tif")
arc2014.RasterXSize
arc2014.RasterYSize
arc2014.GetMetadata()
arc2014.GetProjection()
show((arc2014, 365))
arc2014.indexes
arc2014.read(1)[200, 200]
arc2014.crs

plt.figure()
plt.imshow(arc2014)



arc2014_reproject= gdal.Warp("cholera_aglr/results/arc2014_rast_reproject.tif", arc2014, dstSRS='EPSG:4326')
arc2014_reproject.GetProjection()
arc2014_reproject.RasterCount
arc2014_reproject_band1= arc2014_reproject.GetRasterBand(1)
array= arc2014_reproject_band1.ReadAsArray()


plt.figure()
plt.imshow(array)

## read using rasterio
arc2014= rasterio.open("cholera_aglr/results/arc2014_rast_reproject.tif")
arc2014.crs
arc2014.meta
show(arc2014)

## trying to sum bands
show(arc2014.read(3))
show(arc2014.read(3)+ arc2014.read(4))


mean_2014 = zonal_stats(combined_shape, arc2014_reproject_band1, layer= 1, stats= "mean")

#zonal_stat= zonal_stats(combined_shape, "cholera_aglr/results/arc2014_rast.tif", band= 1 )
zonal_stat2= zonal_stats(combined_shape, "cholera_aglr/results/arc2014_rast.tif", band= 1, all_touched=True )
#zonal_stat= zonal_stats(combined_shape, arc2014, band= 1 )

pd.DataFrame(zonal_stat2)




arc2014.to_crs(combined_shape.cer)


### write a function to get the cumulative rainfall
### given a lag period + duration of cumulation from the end data

def get_cumulative_rainfall(end_date, lag=5, cumulative_duration=30):
    start= end_date - (pd.Timedelta(days= lag) + pd.Timedelta(days=cumulative_duration))
    st_date= start.day_of_year
    
    start.year
    
    end= end_date - pd.Timedelta(days= lag) 
    en_date= end.day_of_year
    
    dt_range= pd.date_range(start, end)
    
    date_of_year_range=dt_range.day_of_year
    
    for i in date_of_year_range :
        print(i)
        if i ==1:
            cumulative_raster = arc2014.read(i)
        else:
            cumulative_raster = cumulative_raster + arc2014.read(i)
    
    
    return [start, end, st_date, en_date]

get_cumulative_rainfall(chl_df.loc[1, "end_date"])
get_cumulative_rainfall(pd.to_datetime("2017, 1, 1"))

end_date= pd.to_datetime("2017, 5, 1")