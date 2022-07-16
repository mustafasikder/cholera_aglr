from math import comb
from webbrowser import get
import pandas as pd
import geopandas as gpd
import fiona
from pyproj import transform

help(fiona.open)
import os
import rasterio
from rasterio.transform import Affine
from rasterio.plot import show
from rasterstats import zonal_stats

import datetime as dt

# import gdal
from osgeo import gdal
from osgeo import ogr
from osgeo import osr
from osgeo import gdal_array
from osgeo import gdalconst

gdal.UseExceptions()

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np



## chaning option to display all columns
pd.set_option("display.max_columns", None)


os.chdir("X:/Spatial Stat/climate_cholera")
os.listdir("data/")
ch_df = pd.read_csv(
    "data/Africa_El_Nino_Cases_April 2015-present.csv", parse_dates=["End_Date"]
)

ch_df.head()
ch_df.info()
ch_df.shape


chl_df = ch_df[ch_df["Disease"] == "CHL"]

chl_df.groupby("Disease")
set(chl_df["Country"])
### subset to east Africa countries
country_vector = [
    "Kenya",
    "Uganda",
    "Tanzania",
    "South Sudan",
    "Somalia",
    "Ethiopia",
    "Zambia",
    "Malawi",
    "DRC",
]

chl_df.loc[chl_df.Country == "Democratic Republic of the Congo", "Country"] = "DRC"

chl_df = chl_df[chl_df["Country"].isin(country_vector)]

# convet to datetime object
chl_df.loc[:, "end_date"] = pd.to_datetime(chl_df.End_Date)


combined_shape = gpd.read_file(
    "X:/Spatial Stat/climate_cholera/cholera_aglr/results/combined_shape.shp"
)
combined_shape.crs
combined_shape.shape
combined_shape.info()
combined_shape.loc[:, "End_Dat"] = pd.to_datetime(combined_shape.End_Dat)

driver = gdal.GetDriverByName("GPKG")
driver.Register()
combined_shape = gdal.Open(
    "X:/Spatial Stat/climate_cholera/cholera_aglr/results/combined_shape.gpkg"
)


### read the rasters
# arc2014= rasterio.open("cholera_aglr/results/arc2014_rast.tif")
arc2014 = gdal.Open("cholera_aglr/results/arc2014_rast.tif")
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

arc2014 = gdal.Open("cholera_aglr/results/arc2014_rast.tif")

arc2015_2019 = gdal.Open("cholera_aglr/results/arc2015_2019_stack.tif")

arc2015_19_reproject = gdal.Warp(
    "cholera_aglr/results/arc2015_2019_stack_reproject.tif", arc2015_2019, dstSRS="EPSG:4326"
)
arc2015_19_reproject.GetProjection()

arc2014_reproject.RasterCount
arc2014_reproject_band1 = arc2014_reproject.GetRasterBand(1)
array = arc2014_reproject_band1.ReadAsArray()


plt.figure()
plt.imshow(array)

## read using rasterio
arc2014 = rasterio.open("cholera_aglr/results/arc2014_rast_reproject.tif")
arc2014.crs
arc2014.meta
show(arc2014)

arc2015_2019 = rasterio.open("cholera_aglr/results/arc2015_2019_stack.tif")
arc2015_2019_reproj = rasterio.open("cholera_aglr/results/arc2015_2019_stack_reproject.tif")

## trying to sum bands
show(arc2014.read(3))
show(arc2014.read(3) + arc2014.read(4))

### open raster with the recomended rasterio steps
crs= rasterio.crs.CRS({"init": "epsg:4326"})
with rasterio.open("cholera_aglr/results/arc2015_2019_stack.tif") as src:
    #src.transform= transform
    src.crs= crs





mean_2014 = zonal_stats(combined_shape, arc2014_reproject_band1, layer=1, stats="mean")

# zonal_stat= zonal_stats(combined_shape, "cholera_aglr/results/arc2014_rast.tif", band= 1 )
zonal_stat2 = zonal_stats(
    combined_shape, "cholera_aglr/results/arc2014_rast.tif", band=1, all_touched=True
)
# zonal_stat= zonal_stats(combined_shape, arc2014, band= 1 )

pd.DataFrame(zonal_stat2)


arc2014.to_crs(combined_shape.cer)


### write a function to get the cumulative rainfall
### given a lag period + duration of cumulation from the end data


def get_cumulative_rainfall(end_date, lag=5, cumulative_duration=30):
    start = end_date - (pd.Timedelta(days=lag) + pd.Timedelta(days=cumulative_duration))
    st_date = start.day_of_year

    start.year
    if start.year == 2015:
        previous_year_index = 0
    else:
        previous_year_index = (start.year - 2015) * 365

    end = end_date - pd.Timedelta(days=lag)
    en_date = end.day_of_year

    dt_range = pd.date_range(start, end)

    date_of_year_range = dt_range.day_of_year + previous_year_index

    for i in range(len(date_of_year_range)):
        print(i)
        if i == 0:
            cumulative_raster = arc2015_2019.read(st_date + i + previous_year_index)
        else:
            cumulative_raster = cumulative_raster + arc2015_2019.read(
                i + previous_year_index
            )

    # return [start, end, st_date, en_date]

    return [cumulative_raster, start, end, st_date, en_date]


get_cumulative_rainfall(chl_df.loc[1, "end_date"])
get_cumulative_rainfall(pd.to_datetime("2017, 1, 1"))

end_date = pd.to_datetime("2017, 5, 1")

get_cumulative_rainfall

## test: if we can use zonal statistics on the cumulative rainfall output

zonal_stat3 = zonal_stats(
    combined_shape.iloc[[1]],
    cumulative_raster,
    affine=arc2015_2019.transform,
    all_touched=True,
    stats=["min", "max", "mean", "median"],
)

combined_shape_ref= combined_shape
### function to use in the data frame
#%%
def get_cumulative_rainfall(combined_shape, lag=5, cumulative_duration=30):
    zonal_stat_df= pd.DataFrame()
    for i in range(combined_shape.index.start, combined_shape.index.stop):
        print(i)
        start = combined_shape.End_Dat[i] - (
            pd.Timedelta(days=lag) + pd.Timedelta(days=cumulative_duration)
        )
        
        st_date = start.day_of_year
        start.year
        if start.year == 2015:
            previous_year_index = 0
        else:
            previous_year_index = (start.year - 2015) * 365
            
        end = combined_shape.End_Dat[i] - pd.Timedelta(days=lag)
        end_date = end.day_of_year
        
        dt_range = pd.date_range(start, end)
        date_of_year_range = dt_range.day_of_year + previous_year_index
        
        raster_index= range(st_date+previous_year_index, st_date+previous_year_index+ cumulative_duration)
        
        for j in raster_index:
            if j == raster_index.start:
                cumulative_raster = arc2015_2019_reproj.read(j)
            else:
                cumulative_raster = cumulative_raster + arc2015_2019_reproj.read(j)
                        
        zonal_stat3 = zonal_stats(
            combined_shape.iloc[[i-combined_shape.index.start]],
            cumulative_raster,
            affine=arc2015_2019_reproj.transform,
            all_touched=True,
            stats=["min", "max", "mean", "median"],nodata=-999)
        zonal_stat3= pd.DataFrame(zonal_stat3)
        zonal_stat_df= pd.concat([zonal_stat_df, zonal_stat3])
        #print(i)
    return (zonal_stat_df)

#%%
test_data= combined_shape.iloc[451:459, :]
test_df= get_cumulative_rainfall(test_data, lag=5, cumulative_duration=60)
#test_df1= get_cumulative_rainfall(combined_shape.iloc[[113]], lag=5, cumulative_duration=5)
#%%
cumulative_rain_30= get_cumulative_rainfall(combined_shape, lag=5, cumulative_duration=30)

### find Nan 

np.where(cumulative_rain_30.loc[:, "mean"].isnull())
np.where(cumulative_rain_30.loc[:, "mean"]==0)


plt.scatter(combined_shape.loc[:, "Cnfrm_C"], cumulative_rain_30.loc[:, "mean"])


fig, axs = plt.subplots()
axs.axis([0, 70000, 0, 350])
axs.plot(combined_shape.loc[:, "Cnfrm_C"], cumulative_rain_30.loc[:, "mean"])
fig

fig, axs = plt.subplots()
axs.axis([0, 70000, 0, 350])
axs.plot(combined_shape.loc[:, "Cnfrm_C"], cumulative_rain_30.loc[:, "mean"])
fig


combined_shape.Country.unique()

fig, axs = plt.subplots()
#axs.axis([2014, 2019, 0, 350])
axs.plot(combined_shape.loc[:, "End_Dat"], cumulative_rain_30.loc[:, "mean"])
fig


fig, ax = plt.subplots()  # Create a figure containing a single axes.
ax.plot([1, 2, 3, 4], [1, 4, 2, 3])
plt.show()
### Join the two data frames
combined_df= cumulative_rain_30.reset_index(drop=True).join(combined_shape)
combined_df.to_csv("cholera_aglr/results/rain_combined.csv")

fig, axs = plt.subplots()
for i  in combined_shape.Country.unique():
    print(i)
    axs.plot(combined_df.loc[combined_df.Country==i, "End_Dat"], combined_df.loc[combined_df.Country==i, "mean"])
fig 
   
    

# %%
