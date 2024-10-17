### Population Distribution ###

# Install Packages needed and open them with library function
  # sdm = species distribution modelling
  # terra = spatial data analysis

install.packages("terra")
install.packages("sdm")

library(terra)
library(sdm)

# Bring up dataset to work on from the sdm package by using system.file function
  # necessary to call package="" as the folder external is in all packages
file <- system.file("external/species.shp", package="sdm")
file

# Translate .shp file to a usable file in R (SpatVector) with vect function 
rana <- vect(file)
rana$Occurrence

# Plot Distribution of rana 
plot(rana)

  # Make dataset of only occurance = 1 (presence of rana) by using sequel (sql) language
    # all statements in sql function end with ; rahter than :
  pres <- rana[rana$Occurrence==1]
  pres
  pres$Occurrence
  
  # Make dataset of only occurance = 0 (absence of rana) by using sequel (sql) language
    # Absence is more uncertain as a dataset than the presence 
    # as when present = sure to have right data, absence, could be missing a present rana)
  abse <- rana[rana$Occurrence==0]
  abse
  
  # Make multiframe made of complete rana dataset and abse/pres dataset
  par(mfrow=c(2,2))
  plot(pres, main="Presence Only")
  plot(abse, main="Absence Only")
  plot(rana, main="Complete")
  
  # Plot presences in blue and absences in red in the same graph
  dev.off()
  plot(pres, col="blue", pch=19, cex=2)
  points(abse, col="red", pch=19, cex=2)

# Covariates : try to explain rana distribution with elevation data
  # All .asc files are raster datasets  -- :) cool -- 
  # Using the rast function to change data into a raster dataset
  elev <- system.file("external/elevation.asc", package="sdm")
  elevmap <- rast(elev)
  
  # Make plot with elev and rana datasets divided in pres/abse with funky colours
  cln<-colorRampPalette(c("firebrick","darkseagreen","darkslateblue"))(100)
  plot(elevmap, col=cln )
  points(pres, col="yellow", pch=19, cex=1)
  points(abse, col="darkorchid4", pch=19, cex=1)
  
