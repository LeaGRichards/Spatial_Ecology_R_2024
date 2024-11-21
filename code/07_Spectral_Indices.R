### Spectral Indices ###

# Open necessary packages
  library(terra)
  library(imageRy)

# List data files
  im.list

# Import data --> deforestation of Mato Grosse in Brazil (2006)
  m2006 <- im.import("matogrosso_ast_2006209_lrg.jpg")
  # Aster is a satellite with ++ bands, it has wide resolution of pixel (low resolution)
  # In on the Terra satellite of NASA
  # Bands : 1 = NIR, 2 = red, 3 = green

# Creating basic RGB plot  
  im.plotRGB(m2006, r=1, g=2, b=3)
    # If you invert green and blue, it doesn't make a big difference
    # Where you place the NIR makes a difference
  
# We can enhance the bear soil by putting the NIR in the blue band in plotRGB
    # The bear soil becomes yellow = areas affected by humans
    # The water becomes orange as it is polluted... has "material" in it
  im.plotRGB(m2006, r=2, g=3, b=1)    

# Mouting the NIR on the green band makes the bear soil pink and vegetation green
  im.plotRGB(m2006, r=2, g=1, b=3)   
  
# To prove the NIR shows more information than the other visible wavelengths
  plot(m2006[[1]]) # plotting the NIR 
  plot(m2006[[2]]) # Plotting the red band
  plot(m2006[[3]]) # Plotting the green band

# Import data --> deforestation of Mato Grosse in Brazil (1992)
  # Landsat = satellite from NASa, most ancient commencial satellite
    # Collectes data since 1972 !!! 
  m1992 <- im.import("matogrosso_l5_1992219_lrg.jpg")
  im.plotRGB(m1992, r=1, g=2, b=3)

# Multiframe of the forest in 1992 and 2006  
  par(mfrow=c(1,2))
  im.plotRGB(m1992, r=1, g=2, b=3)
  im.plotRGB(m2006, r=1, g=2, b=3)
  # Visually shows High amount of deforestation 
    # into articial human impacted land between 1992 and 2006 (12 years!)

# Multiframe of 6 images in pairs with NIR on the same component, differing between rows
  par(mfrow=c(3,2))
  im.plotRGB(m1992, r=1, g=2, b=3)
  im.plotRGB(m2006, r=1, g=2, b=3)
  
  im.plotRGB(m1992, r=3, g=1, b=2)
  im.plotRGB(m2006, r=3, g=1, b=2)
  
  im.plotRGB(m1992, r=2, g=3, b=1)
  im.plotRGB(m2006, r=2, g=3, b=1)

# Theory :
  
  # Spectral signature of each species of plant (see online)
    # The graph of reflectance of a given plant according to the wavelength
  # DVI = difference vegetation index
    # Difference between NIR reflectance and red reflactance of different plants
      # NIR is highly reflected by plants (100)
      # Red is absorbed ++ by plants (10)
      # DVI = 100 - 10 = 90
    # When plant dies, it doesn't absorb blue and red as much and won't freflect NIR and green as much
      # NIR = 80
      # RED = 20
      # DVI = 60

# Compare DVI values of 1992 and 2006 (DVI = NIR - RED)
  
  # DVI in 1992
  dvi1992 = m1992[[1]] - m1992[[2]]
  cl <- colorRampPalette(c("darkblue", "yellow", "red", "black")) (100)
  dev.off()
  plot(dvi1992, col=cl)  

  # DVI 2006
  dvi2006 = m2006[[1]] - m2006[[2]]
  plot(dvi2006, col=cl) 
  
  # Multiframe with both pictures
  par(mfrow=c(1,2))
  plot(dvi1992, col=cl)
  plot(dvi2006, col=cl)

# NDVI
  # Normalized difference vegetation index
  # It places all DVI ranges from -1 to 1
  # Used when DVI ranges aren't same in order to compare them
  # In general, always use NDVI over DVI to avoid any 
  
  # NDVI 1992 
  ndvi1992 = dvi1992/(m1992[[1]]+m1992[[2]])
  ndvi1992

  # NDVI 2006
  ndvi2006 = dvi2006/(m2006[[1]]+m2006[[2]])
  
  # Multiframe of both NDVI plots
  par(mfrow=c(1,2))
  plot(ndvi1992, col=cl)
  plot(ndvi2006, col=cl)
  # Decrease of general NDVI from 1992 to 2006
