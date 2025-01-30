# this is a mess -.- 
# STEP 1 - Preparing the R space 
# Import adequate libraries
  library(raster)
  library(ggplot2)

# Load the satellite image of Montreal 
  # Source .. 
  # Copernicus ... 
  mtl <- rast("Montreal2018_4 2.jpg")
  plotRGB(mtl, r=1, g=2, b=3)
  plot(mtl[[3]]) # NIR band
  plot(mtl[[4]]) # red band
  
# Get and sort out GBIF bird dataset
  birds <- read.table("occurrence.txt", header=TRUE, sep="\t", )
  
  # Latitude coordinates and cleaning
  lat_birds <- birds[[98]] # Latitude coordinates in decimal degrees (DD)
    sum(is.na(lat_birds))  # There is no NA data in this column
    class(lat_birds) # The data is classed as character data
    as.numeric(lat_birds) # Changing the data to a numeric class
    sum(unique(lat_birds) # Some points are words or spaces, not numeric values
    lat_birdsC <- lat_birds[!(lat_birds, c("CONTINENT_DERIVED_FROM_COORDINATES;TAXON_MATCH_TAXON_CONCEPT_ID_IGNORED", "Icterus galbula (Linnaeus, 1758)", "Sitta carolinensis Latham, 1790",  ))] # Cleaning the data from those non-numeric values
  
  long_birds <- birds[[99]] # Longitude coordinates in decimal degrees (DD)
    sum(is.na(long_birds)) # 513 points are NAs in this column
    long_birdsC <- na.omit(long_birds) # Remove rows with NA values
    unique(long_birdsC)
    
  unc_birds <- birds[[100]] # Coordinate uncertainty in meters
    sum(is.na(unc_birds)) # There is no NA data in this column
# Density map of observations
  # install.packages("spatstat")
  library(spatstat)
  plot(long_birds~lat_birds)
  
# STEP 2 - NDVI

DVImtl <- mtl[[1]]-mtl[[2]] # DVI= NIR - red
NDVImtl <- DVImtl/(mtl[[1]]+mtl[[2]])
plot(NDVImtl)
Mmtl<- im.classify(NDVImtl, num_clusters=2)
plot(Mmtl)
mtlTEST <- im.classify(mtl, num_clusters=2)
plot(mtlTEST)


mtl2 <- im.import("Montreal2018_4 2.jpg")
