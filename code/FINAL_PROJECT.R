########### FINAL PROJECT ###########

# Does a density map of observations of birds by citizens (GBIF dataset) 
# correlate with vegetation cover in the region on Montreal in June 2018?

# Context: 
  # Study area (decimal degrees coordinates): Cosmopolitain Region of Montreal
      # -73.95502, 45.70391
      # -73.49812, 45.70391
      # -73.49812, 45.37882
      # -73.95502, 45.37882
  # Bird observations in June 2018 in the regions of Montreal

# Bird observation dataset sources : 
  # GBIF.org (30 January 2025) GBIF Occurrence Download https://doi.org/10.15468/dl.gjxh38
  # Auer T, Barker S, Barry J, Charnoky M, Curtis J, Davies I, Davis C, Downie I, Fink D, 
  # Fredericks T, Ganger J, Gerbracht J, Hanks C, Hochachka W, Iliff M, Imani J, Jordan A, 
  # Levatich T, Ligocki S, Long M T, Morris W, Morrow S, Oldham L, Padilla Obregon F, 
  # Robinson O, Rodewald A, Ruiz-Gutierrez V, Schloss M, Smith A, Smith J, Stillman A, 
  # Strimas-Mackey M, Sullivan B, Weber D, Wolf H, Wood C (2024). EOD – eBird Observation Dataset. 
  # Cornell Lab of Ornithology. Occurrence dataset https://doi.org/10.15468/aomfnb accessed 
  # via GBIF.org on 2025-01-30.
  # Questagame (2023). Earth Guardians Weekly Feed. Occurrence dataset 
  # https://doi.org/10.15468/slqqt8 accessed via GBIF.org on 2025-01-30.
  # iNaturalist contributors, iNaturalist (2025). iNaturalist Research-grade Observations. 
  # iNaturalist.org. Occurrence dataset https://doi.org/10.15468/ab3s5x accessed via GBIF.org on 2025-01-30.

# Montreal satellite image dataset source : 
  # to be determined


########################################
### Density map of bird observations ###
########################################

# GBIF dataset
  birds <- read.table("occurrence.txt", header=TRUE, sep="\t", fill = TRUE)
   
# Latitude coordinates and cleaning
    lat_birds <- birds[[98]] # Latitude coordinates in decimal degrees (DD)

  # Looking if there are any NA values 
    sum(is.na(lat_birds))  # There is no NA values
    
  # Looking for any non-numeric values (words, spaces, NA, etc.)
    unique(lat_birds) # Presence of words, string-characters, spaces 
    
  # Removing the words from the coordinates
    lat_birds2 <- lat_birds[!(lat_birds %in% c("", "CONTINENT_DERIVED_FROM_COORDINATES;TAXON_MATCH_TAXON_CONCEPT_ID_IGNORED", 
                  "Icterus galbula (Linnaeus, 1758)", "Sitta carolinensis Latham, 1790", "Melospiza melodia (A.Wilson, 1810)", 
                  "Setophaga ruticilla (Linnaeus, 1758)", "Sayornis phoebe (Latham, 1790)"))] 

  # Removing the "" around some of the coordinates (accidently considered words) and any space considered a value
    lat_birds3 <- gsub('"', '', lat_birds2)  

  # Removing any space before or after numbers
    lat_birds4 <- trimws(lat_birds3)

  # Making sure the data is classed as numeric
    class(lat_birds4) # The data is classed as character data
    lat_birds5 <- as.numeric(lat_birds4) # Changing the data to a numeric class 
    
  # Looking again for any value that should be cleaned
    unique(lat_birds5) # These values are fine, they simply reflect some duplicates or close values which are to be expected

  # Making sure all value are in the expected coordinate range
    # Expected coordinate range : 45.37882 to 45.70391
    range(lat_birds5) # This is wrong

  # Removing the rows with values outside the range
    lat_birds6 <- lat_birds5 >= 45.37882 & lat_birds5 <= 45.70391
    lat_birds7 <- lat_birds5[lat_birds6]
    range(lat_birds7) # This is right

  # The latitude coordinates are now clean

# Longitude coordinates and cleaning
    long_birds <- birds[[99]] # Longitude coordinates in decimal degrees (DD)
  
  # Looking if there are any NA values 
    sum(is.na(long_birds)) # 513 points are NAs

  # Removing rows with NA values 
    long_birds2 <- na.omit(long_birds)
    
  # Looking for any non-numeric values (words, spaces, NA, etc.)
    unique(long_birds2) # Looks good, but some values are too high

  # Making sure all value are in the expected coordinate range
    # Expected coordinate range : -73.95502 to -73.49812
    range(long_birds2) # This is wrong

  # Removing the rows with values outside the range
    long_birds3 <- long_birds2 >= -73.95502 & long_birds2 <= -73.49812
    long_birds4 <- long_birds2[long_birds3]
    range(long_birds4) # This is right
  
  # The longitude coordinates are now clean

# Plotting the coordinates to visualize them
  plot(long_birds4, lat_birds7)
  
# Make a density map using spatstat
  # We need to tranform the coordinates into a point pattern object (ppp)
  # that fits into a defined window (study area)

  # Call the appropriate library (and install the package if not already done)
    # install.packages("spatstat")
    library(spatstat)
    
  # Defining the desired window with the owin() function
    win <- owin(xrange = c(-73.95502, -73.49812), yrange = c(45.37882, 45.70391))
    
  # Transforming the coordinates into a point pattern object with the ppp() function
    birds_ppp <- ppp(x = long_birds4, y = lat_birds7, window = win)
    # Warning of duplicate points is fine as it's expected with this type of dataset
    
  # Converting the ppp to a raster with the density() function
    dm_birds <- density(birds_ppp)
    # Now the data is in a raster format rather than points
    
  # Make a plot with the raster (density map)
    plot(dm_birds)


########################################
#### Vegetation cover cluster map  #####
########################################

# Load the satellite image of Montreal 
  mtl <- rast("Montreal2018_4 2.jpg")

  plotRGB(mtl, r=1, g=2, b=3) # Human eye visual image of the study area
  plot(mtl[[3]]) # NIR band
  plot(mtl[[4]]) # red band  
# STEP 2 - NDVI

DVImtl <- mtl[[1]]-mtl[[2]] # DVI= NIR - red
NDVImtl <- DVImtl/(mtl[[1]]+mtl[[2]])
plot(NDVImtl)
Mmtl<- im.classify(NDVImtl, num_clusters=2)
plot(Mmtl)
mtlTEST <- im.classify(mtl, num_clusters=2)
plot(mtlTEST)


mtl2 <- im.import("Montreal2018_4 2.jpg")
