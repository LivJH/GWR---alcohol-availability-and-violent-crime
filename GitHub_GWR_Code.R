
#__________________________________________________________________________________________________________

'1. Add in Libraries'

#__________________________________________________________________________________________________________

library(car) 
library(data.table) 
library(tidyverse)
library(rgdal)
library(ggplot2)
library(normalr)
library(maptools)
library(tmaptools)
library(sf)
library(sp)
library(raster)
library(RColorBrewer)
library(spgwr)
library(dplyr)
library(spData)
library(tmap)
library(leaflet)
library(shiny)
library(scgwr)
library(grid)
library(MuMIn)
library(e1071)
library(viridisLite)



#__________________________________________________________________________________________________________

'2. Read in Data'

#__________________________________________________________________________________________________________


# Violent crime data
#--------------------
Crime <- read_csv("Policedata.csv") - later calculate rate and log


# Alcohol outlet availability data
#---------------------------
AHAH <- read_csv("AHAH26_02_2019.csv") # read in AHAH data


# Deprivation
#-------------
IMD <- fread("IMD2019.csv") # read in IMD data


# Populatio€n data
#-----------------
Pop <- read_csv("MidYear.csv")
'note: column names were renamed in Excel prior to reading in'






#__________________________________________________________________________________________________________

'2. Data Cleaning'

#__________________________________________________________________________________________________________



# 2.1. Crime data 
#-----------------------------------------------------------------------------------------------------------


# 2.1.1. Crime type - violent crimes only
#---------------------------------------

## Convert crime type to a character for step below - only if you used read.csv()
Crime$Crime.type <- as.character(Crime$Crime.type)

## Filter for violent and sexual offences, public order, ASB
Crime <- Crime %>%
  filter(`Crime type` %in% c("Violence and sexual offences"))


# 2.1.2. Aggregate violent crimes to LSOA level
#--------------------------------------

## Create a column in the data which states each row is one crime
Crime$No.Crime <- '1'
Crime$No.Crime <- as.numeric(Crime$No.Crime) # turn the variable into a numeric str as it is currently a character (character str returns an error when running code below)

## Agregate violent crimes to LSOA level using No.Crime column
Violent_crime <- Crime %>%
  group_by(lsoa11) %>%
  summarise(Violent_crime = sum(No.Crime))



# 2.2. All data
#-----------------------------------------------------------------------------------------------------------


# 2.2.1 Remove any not necessary variables/columns/fields
#--------------------------------------------------------

'remove any variables not needed for your analysis from all datasets at this point, you can do this by subsetting the data. 
Examples below:'

data <- data[, c(1,2,5:10)] # *keep* columns 1,2,5 through to 10 in the dataset
data <- data[, -c(1,2,5:10)] # *remove* columns 1,2,5 through to 10 in the dataset


# 2.2.2. Merge datasets together using the LSOA code


# 2.2.3. Calculate violent crime rate, for example: 

# data$Violent_crime_rate <- (data$Violent_crime / data$mid_year_population) 







#__________________________________________________________________________________________________________

'3. Pre-processing data for analysis'

#__________________________________________________________________________________________________________



# 3.1. Explore data distribution and transform if necessary
#-----------------------------------------------------------------------------------------------------------

'Determine the distribution of the data, if the data is skewed it may need to be transformed for the later analysis'

# histogram of a data variable, e.g. violent crime
data <- hist(data$Violent_crime_rate)

'if the data is positively skewed, apply the correct transformation. This could be log() if the data is positively skewed:'

# log data
data$Violent_crime_rate_log <- log(data$Violent_crime_rate)



# 3.2. Inverse distance of alcohol outlet variables
#-----------------------------------------------------------------------------------------------------------

'The value for the outlet variables in the AHAH data is the distance in km to the alcohol outlet from the centroid of the LSOA.
This means the higher the value, the further away the alcohol outlet is.

When we interpret the relationship between alcohol outlets and violent crime, we want the model to recognise that the higher value is closer to the alcohol outlet
as in theory: closer the alcohol outlet, higher violent crime.

To do this, we need to inverse the distances of the outlet variables, For example:'

# Inverse distance
data$off_trade_inv <- data$off_trade * (-1)




# 3.3. Attach LSOA centroid coordinates to the data - to do GWR we need to attach LSOA coords to the data.
#-----------------------------------------------------------------------------------------------------------

# Read in the centroids file - https://geoportal.statistics.gov.uk/datasets/ons::lsoa-dec-2011-population-weighted-centroids-in-england-and-wales/explore
LSOA_centroids <- read.csv('england_welsh_scotland_pwc2011.csv')

# Merge to LSOA centroids to the dataset
data <- left_join(data, LSOA_centroids, by = "lsoa11") 

# Convert easting and northing to longitude and latitude 

  ## Create coords variable
coords <- cbind(Easting = as.numeric(as.character(data$Easting)),
                Northing = as.numeric(as.character(data$Northing)))
  ## Variables for holding the coord system types
ukgrid = "+init=epsg:27700"
latlong = "+init=epsg:4326"
  ## Create spatial df 
sp_df <- SpatialPointsDataFrame(coords, data = data, proj4string = CRS("+init=epsg:27700"))
  ## convert from eastings and northings to long and lat
data <- spTransform(sp_df, CRS(latlong))
  ## convert back to df
data <- as.data.frame(data)
  # we need to rename columns
colnames(data)[colnames(data) == "Easting.1"] <-"Longitude"
colnames(data)[colnames(data) == "Northing.1"] <-"Latitude"






#__________________________________________________________________________________________________________

'4. Scalable geographically weighted regression (ScGWR) Models'

#__________________________________________________________________________________________________________



# 4.1. Run GWR Model
#-----------------------------------------------------------------------------------------------------------

'
1. Run the model with variables of interest
2. Determine which variables to include or exclude based on the models AdjR2, AICc and CV Scores.
'

# first we need a vector of crime variable
y <- data[, "violent_crime_rate"]
# then we need a matrix of explanatory variables
x = data[, c("Income_Deprivation", "MalePopulation", "YoungerPopulation", "All_off_trade", "All_on_trade")]
# run model 
model <- scgwr(coords = coords, y = y, x)
# model results
model 



# 4.2. Add coefficients to data frame
#-----------------------------------------------------------------------------------------------------------


# get coefficients for every LSOA
pred <- predict0(mod = model1, coords0 = coords, x0=x) # find coefficients 
b <- pred$b # retrieve

# attach coefficients to a brand new dataframe of LSOAs for mapping
model_df <- data[, c(2,3)] # create a new df from data, this just has LSOAs and agg_crime
model_df$Income_Deprivation <- b$Income_Deprivation
model_df$MalePopulation <- b$MalePopulation
model_df$YoungerPopulation <- b$YoungerPopulation
model_df$All_off_trade <- b$All_off_trade
model_df$All_on_trade <- b$All_on_trade





#__________________________________________________________________________________________________________

'5. Visualise model results'

#__________________________________________________________________________________________________________



# 5.1. Read in the LSOA shapefile

temp_shapefile <- tempfile() # tempfile returns a vector of character strings which can be used as names for temporary files.
download.file("https://datashare.is.ed.ac.uk/bitstream/handle/10283/2546/LSOA_2011_EW_BFC_shp.zip", temp_shapefile) # download the zipfolder of shapefiles data from the url 
unzip(temp_shapefile) # unzip the zipped folder
LSOA <- st_read('LSOA_2011_EW_BFC.shp') # read in the file. These are LSOA polygons for all of England and Wales
# st_read() will fortify LSOA polygons so they can be used in tmap
LSOA <- read_sf('LSOA_2011_EW_BFC.shp')


# 5.2. Merge polygons to model results for mapping

model_df <- merge(model_df, LSOA, by = c("lsoa11" = "LSOA11CD"))
model_sdf <- st_as_sf(model_df) # convert to sf for mapping



# 5.3. Map the 'All on-trade' outlets relationship with violent crime - change the variable in the code to one you're interested in looking at.

tm_shape(m1_sdf) +
  tm_fill(col = "All_on_trade", midpoint = 0, palette = viridis(9), title = "SVCs") + # show the Income coefficient only
  tm_layout(legend.position = c("right", "top"), legend.title.size = 0.85, legend.text.size = 0.59, legend.hist.width = 0.5
  ) + # this changes the order of the colour palette - because I want positive number to mean more crime and more crime to be in Red, less crime as Green
  tm_scale_bar(text.size = 0.7)

