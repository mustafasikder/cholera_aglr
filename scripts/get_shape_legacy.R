library(stringr)
library(ggplot2)
library(dplyr)e
library(sf)
library(textclean)

theme_set(theme_minimal())
theme_update(legend.position = "bottom")
options(ggplot2.continuous.colour="viridis")
options(ggplot2.continuous.fill = "viridis")

par(mar=c(1,1,1,1))



source('scripts/helper_functions.R')

chl_dt<- readRDS('results/chl_dt.RDS')
chl_dt_c<- readRDS('results/chl_dt_clean.RDS')
View(chl_dropna_dt3%>%group_by(Country, adm_level)%>%count())

chl_dt_c$Admin_Level_I<- replace_non_ascii(chl_dt_c$Admin_Level_I)
chl_dt_c$Admin_Level_II<- replace_non_ascii(chl_dt_c$Admin_Level_II)
chl_dt_c$Admin_Level_III<- replace_non_ascii(chl_dt_c$Admin_Level_III)
chl_dt_c$`Village/Town`<- replace_non_ascii(chl_dt_c$`Village/Town`)

iso3_vec_0<- c('KEN', 'UGA', 'SSD', 'SOM', 'ETH', 'ZMB', 'MWI', 'COD', 'TZA')
iso3_vec_1<- c('KEN', 'UGA', 'SSD', 'SOM', 'ETH', 'ZMB', 'MWI', 'COD', 'TZA')
iso3_vec_2<- c('KEN', 'UGA', 'SSD', 'SOM', 'ETH', 'ZMB', 'MWI', 'COD', 'TZA')
iso3_vec_3<- c('KEN', 'UGA', 'SSD', 'ETH', 'MWI', 'TZA')
iso3_vec_4<- c('UGA')

combined_shp_adm0<- st_as_sf(get_gadm_shp(iso3_vector = iso3_vec_0, admin_level = 0))
combined_shp_adm1<- st_as_sf(get_gadm_shp(iso3_vector = iso3_vec_1, admin_level = 1))
combined_shp_adm2<- st_as_sf(get_gadm_shp(iso3_vector = iso3_vec_2, admin_level = 2))
combined_shp_adm3<- st_as_sf(get_gadm_shp(iso3_vector = iso3_vec_3, admin_level = 3))
combined_shp_adm4<- st_as_sf(get_gadm_shp(iso3_vector = iso3_vec_4, admin_level = 4))

# process the shape data
combined_shp_adm0$NAME_0[combined_shp_adm0$NAME_0=="Democratic Republic of the Congo"]<- "DRC"
combined_shp_adm1$NAME_0[combined_shp_adm1$NAME_0=="Democratic Republic of the Congo"]<- "DRC"
combined_shp_adm2$NAME_0[combined_shp_adm2$NAME_0=="Democratic Republic of the Congo"]<- "DRC"
combined_shp_adm3$NAME_0[combined_shp_adm3$NAME_0=="Democratic Republic of the Congo"]<- "DRC"
#replace non-english to english names
combined_shp_adm1$NAME_1<- replace_non_ascii(combined_shp_adm1$NAME_1)
combined_shp_adm2$NAME_1<- replace_non_ascii(combined_shp_adm2$NAME_1)
combined_shp_adm2$NAME_2<- replace_non_ascii(combined_shp_adm2$NAME_2)
combined_shp_adm3$NAME_1<- replace_non_ascii(combined_shp_adm3$NAME_1)
combined_shp_adm3$NAME_2<- replace_non_ascii(combined_shp_adm3$NAME_2)
combined_shp_adm3$NAME_3<- replace_non_ascii(combined_shp_adm3$NAME_3)
#convert as data frame
combined_shp_adm1_df<- st_drop_geometry(combined_shp_adm1)
combined_shp_adm2_df<- st_drop_geometry(combined_shp_adm2)
combined_shp_adm3_df<- st_drop_geometry(combined_shp_adm3)

# Unique names in Admin 
unique(chl_dt$Admin_Level_III)
unique(chl_dt$Admin_Level_II)
unique(chl_dt$Admin_Level_I)


# Convert chl_dt_c as sf object
chl_c_sf<- st_as_sf(chl_dt_c, coords = c("Long", "Lat"), crs= st_crs(combined_shp_adm3))



# combined_shp_adm1%>%filter(grepl('Addis', NAME_1))%>%select(NAME_0, NAME_1)
# combined_shp_adm1%>%filter(grepl('Addis', NAME_1))%>%ggplot()+geom_sf()

# chl_dt_c%>%filter(adm_level=='adm_0')%>%select(Country)%>%unique

###############################################################################
###############################      Admin 0    ###############################
###############################################################################

# Check no discrepancy in country names 
combined_shp_adm0$NAME_0 %in% unique(chl_dt_c$Country)
# merge data with shape
chl_adm0_sf<- merge( combined_shp_adm0, chl_dt_c[adm_level== "adm_0",], by.x= 'NAME_0', by.y= 'Country', all= T)

###############################################################################
###############################      Admin 1    ###############################
###############################################################################

## join admin 1 shapes with chl_dt
# make sure country names are considered in the joint 
chl_dt_c%>%filter(adm_level=='adm_1')%>%select(Country, Admin_Level_I)%>%unique
chl_dt_c%>%filter(adm_level=='adm_1')%>%nrow # match with semi_join


# unmatched names; names need to match with admin shpefile
unmatched_admin1<- anti_join(chl_dt_c%>%filter(adm_level=='adm_1')%>%select(Country, Admin_Level_I),
          data.frame(combined_shp_adm1%>%select(NAME_0, NAME_1)), 
          by= c('Country'= 'NAME_0', 'Admin_Level_I'= 'NAME_1'))%>%arrange(Country)%>%print.data.frame()

## matched names; nrow of this will match with above when all issues are resolved
semi_join(chl_dt_c%>%filter(adm_level=='adm_1')%>%select(Country, Admin_Level_I),
          data.frame(combined_shp_adm1%>%select(NAME_0, NAME_1)), 
          by= c('Country'= 'NAME_0', 'Admin_Level_I'= 'NAME_1'))

### Resolve unmatched names 


### -------------------------------  DRC  -----------------------------------###
### Admin 1
unmatched_admin1%>%filter(Country=="DRC")%>%arrange(Admin_Level_I)
View(combined_shp_adm1_df%>%filter(NAME_0=="DRC")%>%select(NAME_0, NAME_1)%>%arrange(NAME_1))

chl_dt_c[Country=='DRC' & Admin_Level_I== 'North Kivu', Admin_Level_I:=  'Nord-Kivu']
chl_dt_c[Country=='DRC' & Admin_Level_I== 'South Kivu', Admin_Level_I:=  'Sud-Kivu']
chl_dt_c[Country=='DRC' & Admin_Level_I== 'TanganyikaA', Admin_Level_I:=  'Tanganyika']
chl_dt_c[Country=='DRC' & Admin_Level_I== 'Tqanganytka', Admin_Level_I:=  'Tanganyika']
chl_dt_c[Country=='DRC' & Admin_Level_I== 'Upper Katanga', Admin_Level_I:=  'Haut-Katanga']
chl_dt_c[Country=='DRC' & Admin_Level_I== 'Upper Lamami', Admin_Level_I:=  'Haut-Lomami']

### Admin 2
# NULL 

### Admin 2
# NULL 

### Admin 4
# changing to Admin 2 
chl_dt_c[Country=='DRC' & Admin_Level_I== 'Sud-Kivu' & `Village/Town`== 'Uvira', adm_level:=  'adm_2']
chl_dt_c[Country=='DRC' & Admin_Level_I== 'Sud-Kivu' & `Village/Town`== 'Uvira', Admin_Level_II:=  'Uvira']

chl_dt_c[Country=='DRC' & `Village/Town`== 'Lubumbashi', adm_level:=  'adm_2']
chl_dt_c[Country=='DRC' & `Village/Town`== 'Lubumbashi', c('Admin_Level_I', 'Admin_Level_II') :=  list('Haut-Katanga', 'Lubumbashi')]


### -----------------------------  ETHIOPIA  --------------------------------###
### Admin 1
# No mismatch

### Admin 2 
chl_dt_c[Country=='Ethiopia' & Admin_Level_II== 'Addis Abeba', Admin_Level_I:=  'Addis Abeba']

### Admin 3
chl_dt_c[Country=='Ethiopia' & Admin_Level_III== 'Addis Ababa cityA', adm_level:=  'adm_2']
chl_dt_c[Country=='Ethiopia' & Admin_Level_III== 'Addis Ababa cityA', Admin_Level_II:=  'Addis Abeba']

#chl_dt_c[Country=='Ethiopia' & Admin_Level_III== 'Addis Ababa cityA', adm_level:=  'adm_2']
chl_dt_c[Country=='Ethiopia' & Admin_Level_III== 'Dire Dawa city', c('Admin_Level_I', 'Admin_Level_II', 'Admin_Level_III') :=  list('Dire Dawa', 'Dire Dawa', 'Dire Dawa/Town')]


### ------------------------------  KENYA  ----------------------------------###
### Admin 1
unmatched_admin1%>%filter(Country=="Kenya")%>%arrange(Admin_Level_I)
View(combined_shp_adm1_df%>%filter(NAME_0=="Kenya")%>%select(NAME_0, NAME_1)%>%arrange(NAME_1))

chl_dt_c[Country=='Kenya' & Admin_Level_I== 'Elgeyo Marakwet', Admin_Level_I:=  'Elgeyo-Marakwet']
chl_dt_c[Country=='Kenya' & Admin_Level_I== 'Embu county', Admin_Level_I:=  'Embu']
chl_dt_c[Country=='Kenya' & Admin_Level_I== 'Narok County', Admin_Level_I:=  'Narok']
chl_dt_c[Country=='Kenya' & Admin_Level_I== 'Tharaka Nithi', Admin_Level_I:=  'Tharaka-Nithi']

### Admin 2
chl_dt_c[Country=='Kenya' & Admin_Level_I== 'Mandera' & Admin_Level_II== 'Kotulo', adm_level:=  'adm_3']
chl_dt_c[Country=='Kenya' & Admin_Level_I== 'Mandera' & Admin_Level_II== 'Kotulo', Admin_Level_II:=  'Kotulo']

### Admin 3
#chl_dt_c[Country=='Kenya' & Admin_Level_III== 'Addis Ababa cityA', adm_level:=  'adm_2']
chl_dt_c[Country=='Kenya' & Admin_Level_III== 'Nairobi', Admin_Level_III:=  'Nairobi Central']

### Admin 4
# changing to Admin 2 
chl_dt_c%>%filter(Country=="Kenya", adm_level=='adm_4')%>% select(`Village/Town`)%>%arrange(`Village/Town`)%>%unique
# Kenya has nine Admin 4 locations and the names don't necessarily match with NAME_3 of the 
# shape file. So using st_contains to pick polygons of admin_3 shape file that corresponds to the locations

ggplot()+ 
  geom_sf(data= combined_shp_adm0%>%filter(NAME_0=='Kenya'))+ 
  geom_sf(data=combined_shp_adm3[which(st_contains(combined_shp_adm3, chl_c_sf%>%filter(Country=='Kenya'& adm_level=="adm_4"), sparse = F)), ])+
  geom_sf(data= chl_c_sf%>%filter(Country=='Kenya', adm_level=='adm_4')) #+ 
  #coord_sf(xlim = c(34, 35), ylim = c(0, 1))

# create the contain matrix
test_contain<- st_contains(
  combined_shp_adm3, 
  chl_c_sf%>%filter(Country=='Kenya', adm_level== 'adm_4')%>%arrange(adm_level), 
  sparse = F)
# corresponding NAME_3  
combined_shp_adm3_df[which(apply(test_contain, 1, sum)>0), "NAME_3"]
# plot to check the overlaps and sanaty check 
ggplot(data=combined_shp_adm3[which(apply(test_contain, 1, sum)>0), ])+ 
  geom_sf()+ 
  geom_sf(data= combined_shp_adm0%>%filter(NAME_0=='Kenya'))+ 
  geom_sf(data= chl_c_sf%>%filter(Country=='Kenya', adm_level=='adm_4'), fill= 'red',alpha= .9) +
  geom_sf_text(aes(label= NAME_3))+ 
  geom_sf_text(data= chl_c_sf%>%filter(Country=='Kenya', adm_level=='adm_4'), 
               aes(label= `Village/Town`), 
               color= 'steel blue', 
               #check_overlap = T, 
               nudge_x = 1, 
               nudge_y = 1)

chl_dt_c[Country=='Kenya' & adm_level=='adm_4', adm_level:= "adm_3"]

chl_dt_c[Country=='Kenya' & `Village/Town`== 'BusiaA', c('Admin_Level_I', 'Admin_Level_II', 'Admin_Level_III'):=  list('Busia', 'Matayos', 'Burumba')]
chl_dt_c[Country=='Kenya' & `Village/Town`== 'Dadaab Refugee Camp', c('Admin_Level_I', 'Admin_Level_II', 'Admin_Level_III'):=  list('Garissa', 'Dadaab',   'Dadaab')]
chl_dt_c[Country=='Kenya' & `Village/Town`== 'Kabati', c('Admin_Level_I', 'Admin_Level_II', 'Admin_Level_III'):=  list("Murang'a", 'Kandara',   'Kagundu-Ini')]
chl_dt_c[Country=='Kenya' & `Village/Town`== 'Kibera slum, Nairobi', c('Admin_Level_I', 'Admin_Level_II', 'Admin_Level_III'):=  list('Nairobi', 'Kibra',   'Lindi')]
chl_dt_c[Country=='Kenya' & `Village/Town`== 'Kihoto', c('Admin_Level_I', 'Admin_Level_II', 'Admin_Level_III'):=  list('Kiambu', 'Githunguri',   'Githunguri')]
chl_dt_c[Country=='Kenya' & `Village/Town`== 'Migingo Island, Lake Victoria', c('Admin_Level_I', 'Admin_Level_II', 'Admin_Level_III'):=  list('Migori', 'Nyatike',   'Muhuru')]
chl_dt_c[Country=='Kenya' & `Village/Town`== 'Nairobi', c('Admin_Level_I', 'Admin_Level_II', 'Admin_Level_III'):=  list('Nairobi', 'Starehe',   'Nairobi Central')]
chl_dt_c[Country=='Kenya' & `Village/Town`== 'Shimo la Tewa prison, Mombasa', c('Admin_Level_I', 'Admin_Level_II', 'Admin_Level_III'):=  list('Mombasa', 'Kisauni',   'Shanzu')]
chl_dt_c[Country=='Kenya' & `Village/Town`== 'The Nairobi Hospital', c('Admin_Level_I', 'Admin_Level_II', 'Admin_Level_III'):=  list('Nairobi', 'Kibra',   'Woodley/Kenyatta Golf Course') ]


### ------------------------------  MALAWI  ---------------------------------###
                 
unmatched_admin2%>%filter(Country=="Malawi")%>%arrange(Admin_Level_II)
View(combined_shp_adm2_df%>%filter(NAME_0=="Malawi")%>%select(NAME_0, NAME_1, NAME_2)%>%arrange(NAME_2))
View(combined_shp_adm3_df%>%filter(NAME_0=="Malawi")%>%select(NAME_0, NAME_1, NAME_2, NAME_3)%>%arrange(NAME_2))

### Admin 1
# No admin 1 in chl_dt_c

### Admin 2
# Changing to Admin 1 as the names matchs
chl_dt_c[Country=='Malawi' & Admin_Level_I== 'Southern' & Admin_Level_II== 'Machinga', adm_level:=  'adm_1']
chl_dt_c[Country=='Malawi' & Admin_Level_I== 'Southern' & Admin_Level_II== 'Machinga', Admin_Level_I:=  'Machinga']

chl_dt_c[Country=='Malawi' & Admin_Level_I== 'Southern' & Admin_Level_II== 'Phalombe', adm_level:=  'adm_1']
chl_dt_c[Country=='Malawi' & Admin_Level_I== 'Southern' & Admin_Level_II== 'Phalombe', Admin_Level_I:=  'Phalombe']

### Admin 3
# No Admin 3 row

### Admin 4
# Changing to Admin 2
chl_dt_c[Country=='Malawi' & Admin_Level_I== 'Southern' & `Village/Town`== 'Mangochi', adm_level:=  'adm_2']
chl_dt_c[Country=='Malawi' & Admin_Level_I== 'Southern' & `Village/Town`== 'Mangochi', c('Admin_Level_II', 'Admin_Level_I'):=  list('Mangochi Town', 'Mangochi')]

chl_dt_c[Country=='Malawi' & `Village/Town`== 'Blantyre', adm_level:=  'adm_2']
chl_dt_c[Country=='Malawi' & `Village/Town`== 'Blantyre', c('Admin_Level_II', 'Admin_Level_I'):=  list('Blantyre City', 'Blantyre')]

chl_dt_c[Country=='Malawi' & Admin_Level_I== 'Southern' & `Village/Town`== 'Chikwawa', adm_level:=  'adm_2']
chl_dt_c[Country=='Malawi' & Admin_Level_I== 'Southern' & `Village/Town`== 'Chikwawa', c('Admin_Level_II', 'Admin_Level_I'):=  list('Chikwawa Boma', 'Chikwawa')]

chl_dt_c[Country=='Malawi' & Admin_Level_I== 'Northern' & `Village/Town`== 'Karonga', adm_level:=  'adm_2']
chl_dt_c[Country=='Malawi' & Admin_Level_I== 'Northern' & `Village/Town`== 'Karonga', c('Admin_Level_II', 'Admin_Level_I'):=  list('Karonga Town', 'Karonga')]

chl_dt_c[Country=='Malawi' & Admin_Level_I== 'Central' & `Village/Town`== 'Kasungu', adm_level:=  'adm_2']
chl_dt_c[Country=='Malawi' & Admin_Level_I== 'Central' & `Village/Town`== 'Kasungu', c('Admin_Level_II', 'Admin_Level_I'):=  list('Kasungu Boma', 'Kasungu')]

chl_dt_c[Country=='Malawi' & `Village/Town`== 'Lilongwe', adm_level:=  'adm_2']
chl_dt_c[Country=='Malawi' & `Village/Town`== 'Lilongwe', c('Admin_Level_II', 'Admin_Level_I'):=  list('Lilongwe City', 'Lilongwe')]

chl_dt_c[Country=='Malawi' & `Village/Town`== 'Nkhata Bay', adm_level:=  'adm_1']
chl_dt_c[Country=='Malawi' & `Village/Town`== 'Nkhata Bay', Admin_Level_I:=  'Nkhata Bay']

chl_dt_c[Country=='Malawi' & `Village/Town`== 'Zomba', adm_level:=  'adm_2']
chl_dt_c[Country=='Malawi' & `Village/Town`== 'Zomba', c('Admin_Level_II', 'Admin_Level_I'):=  list('Zomba City', 'Zomba')]

chl_dt_c[Country=='Malawi' & Admin_Level_I== 'Central' & `Village/Town`== 'Salima', adm_level:=  'adm_2']
chl_dt_c[Country=='Malawi' & Admin_Level_I== 'Central' & `Village/Town`== 'Salima', c('Admin_Level_II', 'Admin_Level_I'):=  list('Salima Town', 'Salima')]



### -----------------------------  SOMALIA  ---------------------------------###
### Admin 1
unmatched_admin1%>%filter(Country=="Somalia")%>%arrange(Admin_Level_I)
View(combined_shp_adm1_df%>%filter(NAME_0=="Somalia")%>%select(NAME_0, NAME_1)%>%arrange(NAME_1))

chl_dt_c[Country=='Somalia' & Admin_Level_I== 'Banadir', Admin_Level_I:=  'Banaadir']
chl_dt_c[Country=='Somalia' & Admin_Level_I== 'Banadir region', Admin_Level_I:=  'Banaadir']

### Admin 2
combined_shp_adm2[which(st_contains(
  combined_shp_adm2, 
  chl_c_sf%>%filter(Country=='Somalia', Admin_Level_II=='Madina'), 
  sparse = F)) , "NAME_2"]

chl_dt_c[Country=='Somalia' & Admin_Level_I== 'Banaadir' & Admin_Level_II== 'Madina', c('Admin_Level_I', 'Admin_Level_II') :=  list('Shabeellaha Hoose', 'Afgooye')]

### Admin 4
combined_shp_adm2[which(st_contains(
  combined_shp_adm2, 
  chl_c_sf%>%filter(Country=='Somalia', `Village/Town`=='Beledweyne'), 
  sparse = F)) , "NAME_2"]

chl_dt_c[Country=='Somalia' & `Village/Town`=='Beledweyne' , c('adm_level', 'Admin_Level_II'):=  list('adm_2', 'Beled Weyn')]



### ---------------------------  SOUTH SUDAN  -------------------------------###
### Admin 1
## ISSUE: SSD states were updated in 2020, so the names in chl_dt_c did't match with the current shapefile names.
## FIX: assigning approximate admin 1 and admin 2 shapes with the chl_dt_c data 
## TODO: If old SSD admin1 shape is received, will have to update this
## Used Wikipedia's 2015-2020 32 state map to find the current names and approximate locateions 
unmatched_admin1%>%filter(Country=="South Sudan")%>%arrange(Admin_Level_I)
View(combined_shp_adm1_df%>%filter(NAME_0=="South Sudan")%>%select(NAME_0, NAME_1)%>%arrange(NAME_1))

## Kept as Admin 1 and only updated the name
chl_dt_c[Country=='South Sudan' & Admin_Level_I== 'Bentiu', Admin_Level_I:=  'Unity'] # Bentiu is the capital of Unity
chl_dt_c[Country=='South Sudan' & Admin_Level_I== 'Eastern Lakes', Admin_Level_I:=  'Lakes']
chl_dt_c[Country=='South Sudan' & Admin_Level_I== 'Eastern Nile', Admin_Level_I:=  'Upper Nile']
chl_dt_c[Country=='South Sudan' & Admin_Level_I== 'Imatong', Admin_Level_I:=  'Eastern Equatoria'] # is in Eastern Equatoria
chl_dt_c[Country=='South Sudan' & Admin_Level_I== 'Jonglei', Admin_Level_I:=  'Jungoli'] ## the old boundary was much smaller 

## Changed to Admin 2 and/or updated the name
chl_dt_c[Country=='South Sudan' & Admin_Level_I== 'Jubek', adm_level:=  'adm_2']
chl_dt_c[Country=='South Sudan' & Admin_Level_I== 'Jubek', c('Admin_Level_I', 'Admin_Level_II'):=  list('Central Equatoria', 'Bahr al Jabal')]

chl_dt_c[Country=='South Sudan' & Admin_Level_I== 'Liech', adm_level:=  'adm_2']
chl_dt_c[Country=='South Sudan' & Admin_Level_I== 'Liech', c('Admin_Level_I', 'Admin_Level_II'):=  list('Unity', 'Al Leiri')]

chl_dt_c[Country=='South Sudan' & Admin_Level_I== 'Northern Liech', adm_level:=  'adm_2']
chl_dt_c[Country=='South Sudan' & Admin_Level_I== 'Northern Liech', c('Admin_Level_I', 'Admin_Level_II'):=  list('Unity', 'Rabkona')]

chl_dt_c[Country=='South Sudan' & Admin_Level_I== 'Terekeka', adm_level:=  'adm_2']
chl_dt_c[Country=='South Sudan' & Admin_Level_I== 'Terekeka', c('Admin_Level_I', 'Admin_Level_II'):=  list('Central Equatoria', 'Terkaka')]

chl_dt_c[Country=='South Sudan' & Admin_Level_I== 'Western Bieh', adm_level:=  'adm_2']
chl_dt_c[Country=='South Sudan' & Admin_Level_I== 'Western Bieh', c('Admin_Level_I', 'Admin_Level_II'):=  list('Jungoli', 'Wat')]

### Admin 4
combined_shp_adm3[which(st_contains(
  combined_shp_adm3, 
  chl_c_sf%>%filter(Country=='South Sudan', `Village/Town`=='Juba'), 
  sparse = F)) , "NAME_3"]

chl_dt_c[Country=='South Sudan' & `Village/Town`=='Juba' , c('adm_level', 'Admin_Level_I', 'Admin_Level_II', 'Admin_Level_III'):=  list('adm_3', 'Central Equatoria', 'Bahr al Jabal', 'Juba')]

combined_shp_adm3[which(st_contains(
  combined_shp_adm3, 
  chl_c_sf%>%filter(Country=='South Sudan', `Village/Town`=='Bentiu'), 
  sparse = F)) , "NAME_3"]

chl_dt_c[Country=='South Sudan' & `Village/Town`=='Bentiu' , c('adm_level', 'Admin_Level_I', 'Admin_Level_II', 'Admin_Level_III'):=  list('adm_3', 'Unity', 'Rabkona', 'Bentiu')]



### ----------------------------  TANZANIA  ---------------------------------###         
### Admin 1
unmatched_admin1%>%filter(Country=="Tanzania")%>%arrange(Admin_Level_I)
View(combined_shp_adm1_df%>%filter(NAME_0=="Tanzania")%>%select(NAME_0, NAME_1)%>%arrange(NAME_1))

#chl_dt_c[Country=='Tanzania' & Admin_Level_I== 'Dar es Salaam', Admin_Level_I:=  'Dar Es Salaam']
chl_dt_c[Country=='Tanzania' & Admin_Level_I== 'Kigoma region', Admin_Level_I:=  'Kigoma']
chl_dt_c[Country=='Tanzania' & Admin_Level_I== 'Tanga region', Admin_Level_I:=  'Tanga']
chl_dt_c[Country=='Tanzania' & Admin_Level_I== 'Unguja Island', Admin_Level_I:=  'Zanzibar West'] # Unguja Island is also called Zanzibar
chl_dt_c[Country=='Tanzania' & Admin_Level_I== 'Zanzibar', Admin_Level_I:=  'Zanzibar South and Central']

# Changing to Admin II
chl_dt_c[Country=='Tanzania' & Admin_Level_I== 'Nyasa', adm_level:=  'adm_2']
chl_dt_c[Country=='Tanzania' & Admin_Level_I== 'Nyasa', c('Admin_Level_I', 'Admin_Level_II') :=  list('Ruvuma', 'Nyasa')]

### Admin 2
chl_dt_c[Country=='Tanzania' & adm_level=='adm_2' & Admin_Level_II== 'Ngorongoro district', Admin_Level_II:=  'Ngorongoro']
chl_dt_c[Country=='Tanzania' & adm_level=='adm_2' & Admin_Level_II== 'Uvinza district', Admin_Level_II:=  'Uvinza']

### Admin 3
combined_shp_adm3[which(st_contains(
  combined_shp_adm3, 
  chl_c_sf%>%filter(Country=='Tanzania' & adm_level=='adm_3' & Admin_Level_III=='Dar es Salaam'), 
  sparse = F)) , "NAME_3"]

chl_dt_c[Country=='Tanzania' & adm_level=='adm_3' & Admin_Level_III=='Dar es Salaam', c('Admin_Level_I', 'Admin_Level_II', 'Admin_Level_III'):= list('Dar es Salaam', 'Kinondoni' , 'Ubungo')]

### Admin 4
combined_shp_adm3[which(st_contains(
  combined_shp_adm3, 
  chl_c_sf%>%filter(Country=='Tanzania' & adm_level=='adm_4' & `Village/Town`=='Kagunga'), 
  sparse = F)) , "NAME_3"]
# the lat long goes to Uganda assigning to Kigoma district 
chl_dt_c[Country=='Tanzania' & adm_level=='adm_4' & `Village/Town`=='Kagunga', c('Admin_Level_I', 'Admin_Level_II', 'Admin_Level_III', 'adm_level'):=  list('Kigoma', 'Kigoma Rural', 'Kagunga', 'adm_3')]

chl_dt_c[Country=='Tanzania' & adm_level=='adm_4' & `Village/Town`=='Dar es Salaam', c('Admin_Level_I', 'Admin_Level_II', 'Admin_Level_III', 'adm_level'):=  list('Dar es Salaam', 'Kinondoni', 'Ubungo', 'adm_3')]

combined_shp_adm3[which(st_contains(
  combined_shp_adm3, 
  chl_c_sf%>%filter(Country=='Tanzania' & adm_level=='adm_4' & `Village/Town`=='Mwanza'), 
  sparse = F)) , "NAME_3"]
chl_dt_c[Country=='Tanzania' & adm_level=='adm_4' & `Village/Town`=='Mwanza', c('Admin_Level_I', 'Admin_Level_II', 'Admin_Level_III', 'adm_level'):=  list('Mwanza', 'Nyamagana', 'Isamilo', 'adm_3')]


### ----------------------------  UGANDA  ---------------------------------###
### Admin 1
unmatched_admin1%>%filter(Country=="Uganda")%>%arrange(Admin_Level_I)
View(combined_shp_adm1_df%>%filter(NAME_0=="Uganda")%>%select(NAME_0, NAME_1)%>%arrange(NAME_1))

chl_dt_c[Country=='Uganda' & Admin_Level_I== 'Kampala District', Admin_Level_I:=  'Kampala']

### Admin 2
# Wakiso, Nebbi, Amudat
combined_shp_adm2_df[which(st_contains(
  combined_shp_adm2, 
  chl_c_sf%>%filter(Country=='Uganda' & adm_level=='adm_2' & Admin_Level_II=='Amudat'), 
  sparse = F)) , c("NAME_1", "NAME_2")] 
chl_dt_c[Country=='Uganda' & adm_level=='adm_2' & Admin_Level_II=='Wakiso', c('Admin_Level_I', 'Admin_Level_II'):=  list('Wakiso', 'Busiiro')]
chl_dt_c[Country=='Uganda' & adm_level=='adm_2' & Admin_Level_II=='Nebbi', c('Admin_Level_I', 'Admin_Level_II'):=  list('Nebbi', 'Padyere')]
chl_dt_c[Country=='Uganda' & adm_level=='adm_2' & Admin_Level_II=='Amudat', c('Admin_Level_I', 'Admin_Level_II'):=  list('Nakapiripirit', 'Pokot')]

### Admin 4
uga_adm4<- chl_dt_c%>%filter(Country=='Uganda', adm_level=='adm_4')%>%arrange(`Village/Town`)%>%select(`Village/Town`)%>%unique
#  run this for each Admin 4 
combined_shp_adm3_df[which(st_contains(
      combined_shp_adm3, 
      chl_c_sf%>%filter(Country=='Uganda' & adm_level=='adm_4' & `Village/Town`=='Moroto'), 
      sparse = F)) , c("NAME_1", "NAME_2", "NAME_3")] 

chl_dt_c[Country=='Uganda' & adm_level=='adm_4' & `Village/Town`=='Busia', c('Admin_Level_I', 'Admin_Level_II', 'Admin_Level_III', 'adm_level'):=  list('Busia', 'Samia-Bugwe', 'Busia Tc', 'adm_3')]
chl_dt_c[Country=='Uganda' & adm_level=='adm_4' & `Village/Town`=='Hoima', c('Admin_Level_I', 'Admin_Level_II', 'Admin_Level_III', 'adm_level'):=  list('Hoima', 'Bugahya', 'Hoima Tc', 'adm_3')]
chl_dt_c[Country=='Uganda' & adm_level=='adm_4' & `Village/Town`=='Kampala', c('Admin_Level_I', 'Admin_Level_II', 'Admin_Level_III', 'adm_level'):=  list('Kampala', 'Kampala', 'Kawempe Division', 'adm_3')]
chl_dt_c[Country=='Uganda' & adm_level=='adm_4' & `Village/Town`=='Kasese', c('Admin_Level_I', 'Admin_Level_II', 'Admin_Level_III', 'adm_level'):=  list('Kasese', 'Busongora', 'Kasese Tc', 'adm_3')]
chl_dt_c[Country=='Uganda' & adm_level=='adm_4' & `Village/Town`=='Mbale', c('Admin_Level_I', 'Admin_Level_II', 'Admin_Level_III', 'adm_level'):=  list('Mbale', 'Mbale', 'Northern Division', 'adm_3')]
chl_dt_c[Country=='Uganda' & adm_level=='adm_4' & `Village/Town`=='Moroto', c('Admin_Level_I', 'Admin_Level_II', 'Admin_Level_III', 'adm_level'):=  list('Moroto', 'Moroto', 'Southern Division', 'adm_3')]



### -----------------------------  ZAMBIA  ----------------------------------###
### Admin 1
unmatched_admin1%>%filter(Country=="Zambia")%>%arrange(Admin_Level_I)
View(combined_shp_adm1_df%>%filter(NAME_0=="Zambia")%>%select(NAME_0, NAME_1)%>%arrange(NAME_1))

## changing to Admin II and updating the name as Lukanga is in Kabwe
chl_dt_c[Country=='Zambia' & Admin_Level_I== 'Lukanga', adm_level:=  'adm_2']
chl_dt_c[Country=='Zambia' & Admin_Level_I== 'Lukanga', c('Admin_Level_I', 'Admin_Level_II') := list('Central' , 'Kabwe')]

chl_dt_c[Country=='Zambia' & Admin_Level_I== 'Luapula' & Admin_Level_II== 'Chienge', Admin_Level_II:=  'Chiengi']







##### Unmatched admins after cleaning, this shuold result 0 rows if all the issues are resolved 
unmatched_admin1_postclean<- anti_join(chl_dt_c%>%filter(adm_level=='adm_1')%>%select(Country, Admin_Level_I),
                             data.frame(combined_shp_adm1%>%select(NAME_0, NAME_1)), 
                             by= c('Country'= 'NAME_0', 'Admin_Level_I'= 'NAME_1'))%>%arrange(Country)%>%print.data.frame()



## Merge Admin 1 shape with chl_dt_c rows 
chl_adm1_sf<- left_join( chl_dt_c%>%filter(adm_level== "adm_1"), combined_shp_adm1, by= c('Country'= 'NAME_0', 'Admin_Level_I'= 'NAME_1'), all= T)

chl_adm1_sf<- st_as_sf(chl_adm1_sf)

ggplot()+ geom_sf(data= chl_adm1_sf)


###############################################################################
###############################      Admin 2    ###############################
###############################################################################

################# join admin 2 shapes with chl_dt
chl_dt_c%>%filter(adm_level=='adm_2')%>%select(Admin_Level_II)%>%unique
chl_dt_c%>%filter(adm_level=='adm_2')%>%nrow


# unmatched names; names need to match with admin shpefile
unmatched_admin2<- anti_join(chl_dt_c%>%filter(adm_level=='adm_2')%>%select(Country, Admin_Level_I, Admin_Level_II),
                             data.frame(combined_shp_adm2%>%select(NAME_0, NAME_1, NAME_2)), 
                             by= c('Country'= 'NAME_0', 'Admin_Level_I'= 'NAME_1' , 'Admin_Level_II'= 'NAME_2'))%>%arrange(Country)%>%print.data.frame()

## matched names; nrow of this will match with above when all issues are resolved
semi_join(chl_dt_c%>%filter(adm_level=='adm_2')%>%select(Country, Admin_Level_I, Admin_Level_II),
          data.frame(combined_shp_adm2%>%select(NAME_0, NAME_1, NAME_2)), 
          by= c('Country'= 'NAME_0', 'Admin_Level_I'= 'NAME_1' , 'Admin_Level_II'= 'NAME_2'))%>%arrange(Country)%>%print.data.frame()



### $$$$$$  Kenya        
unmatched_admin2%>%filter(Country=="Kenya")%>%arrange(Admin_Level_II)
View(combined_shp_adm2_df%>%filter(NAME_0=="Kenya")%>%select(NAME_0, NAME_1, NAME_2)%>%arrange(NAME_2))
View(combined_shp_adm3_df%>%filter(NAME_0=="Kenya")%>%select(NAME_0, NAME_1, NAME_2, NAME_3)%>%arrange(NAME_2))

## changing to Admin III as Kutullo is a sub county in admin 3 shape



##################### ADMIN 3 !!!!!!!!

# unmatched names; names need to match with admin shpefile
unmatched_admin3<- anti_join(chl_dt_c%>%filter(adm_level=='adm_3')%>%select(Country, Admin_Level_I, Admin_Level_II, Admin_Level_III),
                             combined_shp_adm3_df%>%select(NAME_0, NAME_1, NAME_2, NAME_3), 
                             by= c('Country'= 'NAME_0', 'Admin_Level_I'= 'NAME_1' , 'Admin_Level_II'= 'NAME_2', 'Admin_Level_III'= 'NAME_3'))%>%arrange(Country)%>%print.data.frame()






















## Update the names in the chl_dt_c to match with shapefiles
combined_shp_adm2$NAME_0[combined_shp_adm2$NAME_0=="Democratic Republic of the Congo"]<- "DRC"

combined_shp_adm2_df<- st_drop_geometry(combined_shp_adm2)
View(combined_shp_adm2_df%>%select(NAME_0, NAME_1, NAME_2)%>%filter(NAME_0=='DRC'))



### South Sudan          
#unmatched_admin1%>%filter(Country=="South Sudan")%>%arrange(Admin_Level_I)
View(combined_shp_adm2_df%>%filter(NAME_0=="South Sudan")%>%select(NAME_0, NAME_1, NAME_2)%>%arrange(NAME_1))

# chl_dt_c[Country=='South Sudan' & Admin_Level_I== 'Banadir', Admin_Level_I:=  'Banaadir']
# chl_dt_c[Country=='South Sudan' & Admin_Level_I== 'Banadir region', Admin_Level_I:=  'Banaadir']


### Tanzania          
View(combined_shp_adm2_df%>%filter(NAME_0=="Tanzania")%>%select(NAME_0, NAME_1, NAME_2)%>%arrange(NAME_1))


### Zambia
View(combined_shp_adm2_df%>%filter(NAME_0=="Zambia")%>%select(NAME_0, NAME_1, NAME_2)%>%arrange(NAME_1))


# unmatched names
#sort(unique(chl_dt_c$Admin_Level_II[chl_dt_c$adm_level=='adm_2'])[!(unique(chl_dt_c$Admin_Level_II[chl_dt_c$adm_level=='adm_2']) %in% combined_shp_adm2$NAME_2)])

View(data.frame(combined_shp_adm1$NAME_1))
# plot the locations
combined_shp_adm2%>%filter(NAME_0=="Uganda", NAME_1==contains("Central"))





# Wakiso is in Admin 1
# Chl_dt_c, Uganda Admin_Level_II are matched with Name_1 in the shapefiles 
chl_dt_c%>%filter(Country=='Uganda'& Admin_Level_I=='Central' & Admin_Level_II=='Wakiso')




#### South Sudan Admin 3
SSD_old_adm3<- st_as_sf(readRDS('X:/Spatial Stat/climate_cholera/data/SSD_adm3_GADM data version 2.8.rds'))
ssd_old_df<- st_drop_geometry(SSD_old)
