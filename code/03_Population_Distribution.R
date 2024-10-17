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

  # Make multiframe made of complete rana dataset and pres dataset
  par(mfrow=c(1,2))
  plot(rana, main="Complete")
  plot(pres, main="Presence only")
