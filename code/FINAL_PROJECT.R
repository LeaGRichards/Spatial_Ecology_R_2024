# this is a mess -.- 

# STEP 1 - Preparing the R space 
# Import adequate libraries
  library(raster)
  library(ggplot2)

# Load the satellite image of Montreal 
  # Source .. 
  # Copernicus ... 
  mtl <- rast("Montreal2018_4 2.jpg")
  plotRGB(mtl, r=1, g=2, b=3) # Human eye visual image of the study area
  plot(mtl[[3]]) # NIR band
  plot(mtl[[4]]) # red band
  
# Get and sort out GBIF bird dataset
  birds <- read.table("occurrence.txt", header=TRUE, sep="\t", )
   
  # Latitude coordinates and cleaning
  
  lat_birds <- birds[[98]] # Latitude coordinates in decimal degrees (DD)
    sum(is.na(lat_birds))  # There is no NA data in this column
    unique(lat_birds) # Some points are words or spaces, not numeric values
                      # some are character strings (ex: "45.4285") rather than numerical
    lat_birds2 <- lat_birds[!(lat_birds %in% c("", "CONTINENT_DERIVED_FROM_COORDINATES;TAXON_MATCH_TAXON_CONCEPT_ID_IGNORED", 
                                                   "Icterus galbula (Linnaeus, 1758)", "Sitta carolinensis Latham, 1790", 
                                                   "Melospiza melodia (A.Wilson, 1810)", "Setophaga ruticilla (Linnaeus, 1758)", "Sayornis phoebe (Latham, 1790)"))] 
    lat_birds3 <- gsub('"', '', lat_birds2)  # Remove the double quotes
    lat_birds4 <- trimws(lat_birds3)
    class(lat_birds5) # The data is classed as character data
    lat_birds5 <- as.numeric(lat_birds4) # Changing the data to a numeric class 
    unique(lat_birds5) # These 'unique' points are fine, simply reflect some duplicates or close values which is to be expected
  
  # Longitude coordinates and cleaning
    
  long_birds <- birds[[99]] # Longitude coordinates in decimal degrees (DD)
    sum(is.na(long_birds)) # 513 points are NAs in this column
    long_birds2 <- na.omit(long_birds) # Remove rows with NA values
    unique(long_birds2) # Looks good!
    
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
