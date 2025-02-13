#####################################
########### FINAL PROJECT ###########
#####################################

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

# Installing the appropriate packages and calling their libraries

  # install.packages("terra") 
  library(terra) # Needed for the rast(), classify() and mask() function

  #install.packages("devtools")
  library(devtools)                            ## Do I need it??? ##

  #install.packages("ggplot2")
  library(ggplot2)                            ## Do I need it ??? ##

  # Calling the imageRy package from devtools     ## Why from devtools??? ###
    # install_github("ducciorocchini/imageRy")
    # library(imageRy)
    # Because I have a mac, I follow these steps instead :
      # dowload file from the link in the error message: 
      # https://api.github.com/repos/ducciorocchini/imageRy/tarball/HEAD
      # Call the package directly from where is is on the mac (probably in Downloads folder) :
      # install.packages("/Users/lea/Desktop/R/ducciorocchini-imageRy-a971c18.tar.gz",repos = NULL, type = "source")
      # install.packages("dichromat")  
      # install.packages("fields") 
      # install.packages("ggplot2") 
      # install.packages("viridis") 

      library(imageRy)

# Loading the satellite images of Montreal as rasters
  mtlB8 <- rast("Montreal2018_B8.tiff") # This is the NIR band
  mtlB4 <- rast("Montreal2018_B4.tiff") # This is the red band

# Getting some information on the rasters
  mtlB8
  mtlB4
  # Both images have the righ coordinates

# Plotting the rasters to have a look
  plot(mtlB8)
  plot(mtlB4)

# Calculating the Difference Vegetation Index (DVI)
  DVImtl <- mtlB8 - mtlB4 # DVI = NIR band - red band

# Calculating the Normalized Difference Vegetation Index (NDVI)
  NDVImtl <- DVImtl/(mtlB8 + mtlB4) # NDVI = DVI / (NIR band + red band)

# Plotting the NDVI raster
  plot(NDVImtl)

# Creating classes of NDVI to cluster the NDVI raster
  # I have chose to cluster the NDVI values in 2 groups
  # Gr. 1 : values ranging from -1.0 to 0.5 -> Water, soil, most buildings, no or little vegetation
  # Gr. 2 : values ranging from 0.5 to 1.0 -> Moderate to dense vegetation
  NDVI_class <- matrix(c(-1.0, 0.5, 1, 
                        0.5, 1.0, 2), ncol = 3, byrow = TRUE)

# Classifying the NDVI map into those 2 clusters
  mtl_class <- classify(NDVImtl, NDVI_class)

# Removing -1 values as they will appear as a third cluster
  # and assigning them to NA
  mtl_class[mtl_class == -1] <- NA

# Plotting the clustered raster
  plot(mtl_class)

########################################
######## Statistical Analysis  #########
########################################

# Convert the bird observations density map into a raster
rast_birds <- rast(dm_birds)

# Visualise the bird observation density raser
plot(rast_birds)

# Get information of both rasters (birds and NDVI)
print(rast_birds)
print(mtl_class)
  # The resolutions of the rasters are different
  # The extent (coordinates) are the same
  # The Coordinate Reference Systems (CRS) is missing for the bird data raster

# Attribute the right CRS to the bird data raster
  crs(rast_birds) <- crs(mtl_class)
  print(rast_birds) # The CRS is adequate now
  
# Resample the bird data raster so the resolution matches the clustered NDVI raster
  rast_birds2 <- resample(rast_birds, mtl_class, method = "bilinear")
  # Using the bilinear method as it's recommended when trying to get smooth raster ### CHECK THIS ###
  print(rast_birds2)
  print(mtl_class)
  # Both rasters now have the same resolution

# Extracting the cell values of the bird raster
  birds <- values(rast_birds2)
  
# Making sure the birds values are a vector
  class(birds) # It is classed as a matrix
  birds2 <- as.vector(birds)
  class(birds2) # Classed as numeric, this is good

# Extract the cell values of the NDVI raster
  NDVI <- values(mtl_class)

# Making sure the NDVI values are a vector
  class(NDVI) # It is classed as a matrix
  NDVI2 <- as.vector(NDVI)
  class(NDVI2) # Classed as numeric, this is good
  
# Create a data frame with both cluster
  df_BN <- data.frame(birds2 = birds2, NDVI2 = NDVI2)
  head(df_BN) # This data frame looks good, it has the right column names

# Remove NA values in the darta frame
  df_BN0 <- na.omit(df_BN)

# Visualizing the distribution of the data
  hist(df_BN0$birds, breaks = 50, main = "Bird Observation Density Histogram", 
       xlab = "Bird Observation Density")
  # We can already see the data is not distributed normally
  # It is skewed to the right, the tail is towards the right

# Testing if the data is normally distributed
  # The shapiro-test only works with =< 5000 data points
  # The data frame has 6,500,000 points, too many for this test
  
  # An alternative is to measure the symmetry and tailddness of the data
  # and see if the results are ones expected for a normal distribution
  # A normal distribution:
    # is symmetrical : skewness tends to 0, between -0.5 and 0.5 is considered good enough
    # Has a taildness which the kurtosis value is 3 
  
  
  # With the skewness function of the e1071 package
  library(e1071)
  skewness(df_BN$birds2)
  # 1.551999 --> The data is skewed to the right (NOT normally distributed)
  
  # With the kurtosis function of the e1071 package
  kurtosis(df_BN$birds2)
  # 4.147184 --> The data has a heavy tail (tail extends slower than a normal distribution)
  
  # Conclusion : The data is NOT normally distributed (as expected when vizualising the histogram)

# Creating groups of data distinguised by their associated NDVI cluster (1 and 2)
  df_BN1 <- df_BN0[df_BN0$NDVI == 1, ]  # Cluster 1
  df_BN2 <- df_BN0[df_BN0$NDVI == 2, ]  # Cluster 2

# Comparing density numbers in cluster 1 to those in cluster 2 
  # Using the Wilcoxon Rank-Sum test (as it can be used if the data is not 
  # distributed normally, and it requires of 2 continuous variables
  wilcox.test(df_BN1$birds2, df_BN2$birds2)
  
# Result and interpretation of the Wilcoxon Rank-Sum test
  # p-value = 9.965e-10; p-value < 0.05
  # W = 4.6465e+12 (the test statistic value)
  
  # Since the p-value is smaller than 0.05, the bird observation densities differ 
  # significantly (at a 95% confidence interval) between NDVI clusters 1 and 2. 
  # We reject the null hypothesis that both groups have similar distributions.
  # This means bird observation densities differ between urbanized areas (water, 
  # soil, most buildings, no or little vegetation) and vegetation areas (moderate 
  # to dense vegetation). It would be interesting to know if this relationship 
  # is strong or weak.

# Visualizing the data of the 2 clusters with boxplots
  boxplot(birds2 ~ NDVI2, data = df_BN0, 
          main = "Bird Density by NDVI Cluster", 
          xlab = "NDVI Cluster", 
          ylab = "Bird Density", 
          col = c("darkgrey", "lightgrey"))
  
  # The data is distributed very similarly. 
  # Both clusters have similar medians and interguartile range.
  # There are MANY outliers (thick black line above the boxes), expected with a 
    # skewed distribution.
  # There might be a statistically significant correlation, but it seams very weak 
    # as we can't visualize it. We should try and measure the strength of this 
    # significant relationship.

# Testing for correlation with the Spearman's Rank Correlation
  # It does not require normality, works well with heavy-tailes skewed distributions
  # It works with categorical variables (NDVI clusters)
  # It can measure the strength of a significant relationship between categories
  cor.test(df_BN$birds2, df_BN$NDVI, method = "spearman")
  
# Result of the Spearman Rank Correlation interpretation
  # p-value = 9.964e-10 (<0.05)
  # Spearman's rho = 0.002463804
  
  # The p-value indicates there is a statistically significant correlation (at 
  # 95% confidence interval) between the bird observation densities 
  # and the NDVI clusters. The rho values is very close to 0 which shows that 
  # the relationship is weak. This might be explained by such a large dataset, 
  # in which it might be more probably to observe a significant correlation that 
  # is not strong or meaningful practically.

# Final conclusion
  # There is a weak relationship between bird observations densities from GBIF 
  # users and urbanized areas VS moderate to dense vegetation.
  
  # It might be interesting to observe that relationship at a more close-up scale 
  # of NDVI clusters. The relationship might be "blurred" by the scale of the 
  # satellite image of Montreal which is quite wide and might lack details.
  
  # It might be interesting to test the relationship at multiple different 
  # urbanized areas in the world and see if there is consistency across them.

#####################################
############# THE END ###############
#####################################
