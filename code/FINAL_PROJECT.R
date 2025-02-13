#####################################
########### FINAL PROJECT ###########
#####################################

# Do bird observation densities correlate with moderate to 
# dense vegetation in the region on Montreal in June 2018?

# Context: 
  # Study area (decimal degrees coordinates): Cosmopolitain Region of Montreal
      # -73.95502, 45.70391
      # -73.49812, 45.70391
      # -73.49812, 45.37882
      # -73.95502, 45.37882
  # Bird observations from GBIF in June 2018 in the regions of Montreal

# Bird observation dataset sources: 
  # GBIF.org (30 January 2025) GBIF Occurrence Download https://doi.org/10.15468/
  # dl.gjxh38 Auer T, Barker S, Barry J, Charnoky M, Curtis J, Davies I, Davis C, 
  # Downie I, Fink D, Fredericks T, Ganger J, Gerbracht J, Hanks C, Hochachka W, 
  # Iliff M, Imani J, Jordan A, Levatich T, Ligocki S, Long M T, Morris W, Morrow 
  # S, Oldham L, Padilla Obregon F,  Robinson O, Rodewald A, Ruiz-Gutierrez V, 
  # Schloss M, Smith A, Smith J, Stillman A, Strimas-Mackey M, Sullivan B, Weber D, 
  # Wolf H, Wood C (2024). EOD – eBird Observation Dataset. Cornell Lab of 
  # Ornithology. Occurrence dataset https://doi.org/10.15468/aomfnb accessed via GBIF
  # .org on 2025-01-30. Questagame (2023). Earth Guardians Weekly Feed. Occurrence 
  # dataset https://doi.org/10.15468/slqqt8 accessed via GBIF.org on 2025-01-30.
  # iNaturalist contributors, iNaturalist (2025). iNaturalist Research-grade 
  # Observations. iNaturalist.org. Occurrence dataset https://doi.org/10.15468/ab3s5x 
  # accessed via GBIF.org on 2025-01-30.

  # Parameters of download: 
    # Basis of record: Human observation
    # Continent: North America
    # Country of area: Canada
    # Geometry: POLYGON((-73.95502 45.70391, -73.49812 45.70391, -73.49812 45.37882, 
        # -73.95502 45.37882, -73.95502 45.70391))
    # Has coordinate: true
    # Has geospatial issue: false
    # Month: June
    # Occurence status: present
    # Scientific name: Aves
    # Year: Between start of 2018 and end of 2018

# Montreal satellite image dataset source : 
  # Copernicus Sentinel data (2025), processed by LeaGRichards, retrieved from EO Browser 
  # (https://browser.dataspace.copernicus.eu/?zoom=11&lat=45.54147&lng=-73.7265&themeId=
  # DEFAULT-THEME&visualizationUrl=U2FsdGVkX1%2FJtzgd7lmq2npjCupmbbb0kJIAYzSCZ0ZvESpXPtiZ
  # VtELmLwFPOZf1b3e%2BMDuuCoT2Umvkb4ytei35zuvCpCIE8PSAGip36yvpSlwDOpKHlnDh4%2FH6R1B&data
  # setId=S2_L2A_CDAS&fromTime=2018-06-15T00%3A00%3A00.000Z&toTime=2018-06-15T23%3A59%3A59
  # .999Z&layerId=1_TRUE_COLOR&demSource3D=%22MAPZEN%22&cloudCoverage=30&dateMode=SINGLE)

  # Parameters of download : 
    # Polygon : 
        # POLYGON((-73.95502 45.70391, -73.49812 45.70391, -73.49812 45.37882, -73.95502 
          # 45.37882, -73.95502 45.70391))
        # LINESTRING(-73.95502 45.70391, -73.49812 45.70391, -73.49812 45.37882, -73.95502 
          # 45.37882, -73.95502 45.70391)
    # Show captions : OFF
    # Crop to AOI : ON
    # Image format : TIFF (16-bit)
    # Image resolution : HIGH (2460 x 2500 px)
    # Layers: Vizualized : True color
    # Layers Raw : B04 and B08
    # Coordinate system : WGS 84 (EPSG:4326)
      # Resolution:
      # lat.: 0.0001300 deg/px (0.5sec/px)
      # long.: 0.0001857 deg/px (0.7sec/px)
    
########################################
### Density map of bird observations ###
########################################

# Calling the GBIF dataset
  birds <- read.table("occurrence.txt", header=TRUE, sep="\t", fill = TRUE)
  # "\t" for tabs as seperators
  # fill = TRUE adds blank fields if rows aren't equal

# Removing quotation marks and spaces from latitudes and longitudes columns
  birds[[98]] <- gsub('"', '', birds[[98]]) # Latitudes
  birds[[99]] <- gsub('"', '', birds[[99]]) # Longitudes

# Classing the longitude and latitude as numeric values
  class(birds[[98]]) # It is classed a characters
  class(birds[[99]]) # It is classed a characters
  
  birds[[98]] <- as.numeric(birds[[98]]) # Latitudes
  birds[[99]] <- as.numeric(birds[[99]]) # Longitudes

# Removing rows with NA values in either or both coordinate values
  birds1 <- birds[!is.na(birds[[98]]) & !is.na(birds[[99]]), ]

# Keeping only the rows with values in the coordinate expected range
  birds2 <- birds1[
    birds1[[98]] >= 45.37882 & birds1[[98]] <= 45.70391 &
    birds1[[99]] >= -73.95502 & birds1[[99]] <= -73.49812, ]

# Making a dataframe with only the cleaned coordinate variables
  birds3 <- data.frame(lat = birds2[[98]], long = birds2[[99]])
  head(birds3) # This looks good

# Plotting the coordinates to visualize them
  plot(birds3$long, birds3$lat, main = "Bird Observation Map", 
    xlb = "Longitude (DD)", ylab = "Latitude (DD)")
  
# Making a density map using spatstat
  # We need to tranform the coordinates into a point pattern object (ppp)
  # that fits into a defined window (study area)

  # CallING the appropriate library
    # install.packages("spatstat")
    library(spatstat)
    
  # Defining the desired window
    win <- owin(xrange = c(-73.95502, -73.49812), yrange = c(45.37882, 45.70391))
    
  # Transforming the coordinates into a point pattern object
    birds_ppp <- ppp(x = birds3$long, y = birds3$lat, window = win)
    # Warning of duplicate points is fine as it's expected with this type of dataset
    
  # Computing the ppp to a density map
    dm_birds <- density(birds_ppp)

  # Creating a color ramp palette to plot with
    cl <- colorRampPalette(c("black", "darkgrey", "white")) (100)

  # Make a plot with the density map
    plot(dm_birds, main = "Density Map of Bird Observations", col = cl)


########################################
#### Vegetation cover cluster map  #####
########################################

# Calling the appropriate package
  # install.packages("terra") 
  library(terra) # Needed for the rast, classify and resample functions

# Loading the satellite images of Montreal as rasters
  mtlB8 <- rast("Montreal2018_B8.tiff") # This is the NIR band
  mtlB4 <- rast("Montreal2018_B4.tiff") # This is the red band

# Getting some information on the rasters
  mtlB8
  mtlB4
    # Both images have the righ coordinates

# Plotting the rasters to have a look
  plot(mtlB8, main = "Near-Infrared band of Montreal on the 15th of June 2018", col = cl)
  plot(mtlB4, main = "Red band of Montreal on the 15th of June 2018", col = cl)

# Calculating the Difference Vegetation Index (DVI)
  DVImtl <- mtlB8 - mtlB4 # DVI = NIR band - red band

# Calculating the Normalized Difference Vegetation Index (NDVI)
  NDVImtl <- DVImtl/(mtlB8 + mtlB4) # NDVI = DVI / (NIR band + red band)

# Plotting the NDVI raster
  plot(NDVImtl, main = "NDVI Map of Montreal on the 15th of June 2018", col = cl)

# Creating classes of NDVI that will be used to create clusters
  # I have chosen to cluster the NDVI values in 2 groups
  # Gr. 1 : values ranging from -1.0 to 0.5 -> Water, soil, most buildings, 
    # no or little vegetation
  # Gr. 2 : values ranging from 0.5 to 1.0 -> Moderate to dense vegetation
  NDVI_class <- matrix(c(-1.0, 0.5, 1, 
                        0.5, 1.0, 2), ncol = 3, byrow = TRUE)

# Classifying the NDVI map into those 2 clusters
  mtl_class <- classify(NDVImtl, NDVI_class)

# Removing -1 values as they will appear as a third cluster
  # and assigning them to NA
  mtl_class[mtl_class == -1] <- NA

# Choosing colors for the clusters
  colors <- c("black", "lightgrey")

# Plotting the clustered raster
  plot(mtl_class, main = "Clustered NDVI map of Montreal on the 15th of June 2018", 
    col = colors)

########################################
######## Statistical Analysis  #########
########################################

# Converting the bird observations density map into a raster
  rast_birds <- rast(dm_birds)

# Visualizing the bird observation density raster
  plot(rast_birds, main = "Density Map of Bird Observations", col = cl)

# Getting information of both rasters (bird observations and NDVI)
  print(rast_birds)
  print(mtl_class)
    # The resolutions of the rasters are different
    # The extent (coordinates) are the same
  
# Resample the bird data raster so the resolution matches the clustered NDVI raster
  rast_birds2 <- resample(rast_birds, mtl_class, method = "bilinear")
    # Using the bilinear method is recommended when using continuous variables and 
    # when trying to get a smooth raster. The value of the new pixel is a weighed 
    # average of the 4 nearest pixels. 
    
  print(rast_birds2)
  print(mtl_class)
    # Both rasters now have the same resolution

# Extracting all cell values of the bird observation raster
  birds <- values(rast_birds2)
  
# Making sure the bird observation values are a vector
  class(birds) # It is classed as a matrix
  birds2 <- as.vector(birds)

# Extract the cell values of the NDVI raster
  NDVI <- values(mtl_class)

# Making sure the NDVI values are a vector
  class(NDVI) # It is classed as a matrix
  NDVI2 <- as.vector(NDVI)
  
# Create a data frame with both cluster
  df_BN <- data.frame(birds2 = birds2, NDVI2 = NDVI2)
  head(df_BN) # This data frame looks good

# Remove rows with at least one NA value in the data frame
  df_BN0 <- na.omit(df_BN)

# Visualizing the distribution of the data
  hist(df_BN0$birds, breaks = 50, main = "Bird Observation Density Histogram", 
    xlab = "Bird Observation Density")
    # Breaks will be the number of bins of the histogram
  
  # We can already see the data is not distributed normally
  # It is skewed to the right (the tail is towards the right)

# Testing if the data is normally distributed
  # The shapiro-test only works with =< 5000 data points
  # The data frame has 6,500,000 points, too many for this test
  
  # An alternative is to measure the symmetry and taildness of the data
  # and see if the results are ones expected for a normal distribution
  # A normal distribution:
    # is symmetrical : skewness tends to 0, between -0.5 and 0.5 is considered good enough
    # Has a taildness which the kurtosis value is 3 
  
  
  # Measuring the skewness
  library(e1071)
  skewness(df_BN$birds2)
  # 1.551999 --> The data is skewed to the right (NOT normally distributed)
  
  # Measuring the taildness
  kurtosis(df_BN$birds2)
  # 4.147184 --> The data has a heavy tail 
  # (The tail extends slower than a normal distribution)
  
  # Conclusion : The data is NOT normally distributed 
  # (As expected when visualizing the histogram)

# Creating groups of data distinguised by their associated NDVI cluster
  df_BN1 <- df_BN0[df_BN0$NDVI == 1, ]  # Cluster 1
  df_BN2 <- df_BN0[df_BN0$NDVI == 2, ]  # Cluster 2

# Comparing density numbers in cluster 1 to those in cluster 2 
  # Using the Wilcoxon Rank-Sum test
    # It can be used if the data is not distributed normally
    # It requires 2 continuous variables
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
          main = "Bird Observation Density for both NDVI Clusters", 
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
  # It does not require normality, works well with heavy-tailed skewed distributions
  # It works with categorical variables (NDVI clusters)
  # It can measure the strength of a significant relationship between categories
  cor.test(df_BN$birds2, df_BN$NDVI, method = "spearman")
  
# Result of the Spearman Rank Correlation interpretation
  # p-value = 9.964e-10 (<0.05)
  # Spearman's rho = 0.002463804
  
  # The p-value indicates there is a statistically significant correlation (at 
  # 95% confidence interval) between the bird observation densities 
  # and the NDVI clusters, as expected. The rho values is very close to 0 which shows 
  # that the relationship is weak. This might be explained by such a large dataset, 
  # with which it might be more probable to observe a significant correlation that 
  # is not strong or meaningful practically.

# Final conclusion
  # There is a weak relationship between bird observations densities from GBIF 
  # users and urbanized areas VS moderate to dense vegetation.
  
  # It might be interesting to observe that relationship at a more close-up scale 
  # of NDVI clusters as the relationship might be "blurred", lacking details, by the 
  # resolution of the satellite image of Montreal which is quite wide. This map had 
  # pixels of about 14.5m square.
  
  # It might be interesting to test the relationship at multiple different 
  # urbanized areas in the world and see if there is consistency across them.

#####################################
############# THE END ###############
#####################################
