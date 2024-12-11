### Importing external data into R ###

# Go to Earth Observatory and search for different images
# download them by clicking JPEG below the image
# Right click and "save image as"

library(terra)

# Set the working directory
  setwd("/Users/lea/Desktop/R")
  # Windows users: C:\\path\Downloads -> C://path/Downloads
  getwd() # Will give you the working directory
  
# Importing data with rast() function in terra package
  qc <- rast("jamesbay_l5_02jun05_30m.jpg")
  # It's normal to have a warning about "unknown extent"

  qc1 <-rast("quebecfires_oli_2013189_lrg.jpg")
  qc2 <-rast("quebecfires_oli_2013173_lrg.jpg")
  
# Plotting the imported image
  plotRGB(qc, r=1, g=2, b=3) # So cool :)
  
  plotRGB(qc1, r=1, g=2, b=3)
  plotRGB(qc2, r=1, g=2, b=3)
  
  par(mfrow=c(1,2))
  plotRGB(qc1, r=1, g=2, b=3)
  plotRGB(qc2, r=1, g=2, b=3)

  # Multitemporal change detection  
  qcdif = qc1[[1]]-qc2[[1]]
  cl <- colorRampPalette(c("brown", "grey", "orange")) (100)
  plot(qcdif, col=cl)  
  
  qcdif2 = qc1[[2]]-qc2[[2]]
  plot(qcdif2, col=cl)
  
  qcdif3 = qc1[[3]]-qc2[[3]]
  plot(qcdif3, col=cl)
