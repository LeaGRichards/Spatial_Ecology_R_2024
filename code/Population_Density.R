### Population Density ###

#Install packages necessary to calculate the density of individuals in populations
# install.packages("spatstat") --> put in comment to avoid furthuer downloading

# Opening Spatstat package on R
library(spatstat)

## Bei

# Open dataset Bei (coordinate in space)
bei
  # Plot bei data with desired visual appearance
plot(bei, pch=19, cex=0.5)

  # Open bei.extra which has covariate data (composed of images of elevation and slope elevation-grad )
bei.extra
  # Plot bei.extra
plot(bei.extra)

#elevation
  #extract only elevation data of bei.extra by creating a new object for it ($ is symbol for link)
elevation <- bei.extra$elev
    # or by extrating first image dataset/image from bei.extra
elevation2 <- bei.extra[[1]]
  # Plot elevation and elevation2 --> will give same graph
plot(elevation)
plot(elevation2)

# Density map of bei data starting from individual points 
    #with function density() inside spatstat package
densitymap <- density(bei)
densitymap #data is in pixels/raster rather than points due to density function
plot(densitymap)

#Show map of density AND original points of trees
points(bei, cex=0.5, col="green")
