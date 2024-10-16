### Population Density ###
  # Day 1 
#Install packages necessary to calculate the density of individuals in populations
# install.packages("spatstat") --> put in comment to avoid furthuer downloading

# Opening Spatstat package on R
library(spatstat)

## Bei
# # https://CRAN.R-project.org/package=spatstat

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

# Density map of bei data starting from individual points and going to continuous surface
    # with function density() inside spatstat package
densitymap <- density(bei)
densitymap #data is in pixels/raster rather than points due to density function
plot(densitymap)

#Show map of density AND original points of trees
points(bei, cex=0.5, col="green")

  # Day 2

# Plot maps of elevation an density map next to one another with multiframe of par function
  # In this case we want one row of frames and 2 columns
  # Using mfrow followed by a vactor of 2 parts --> which is number of rows and number of columns

par(mfrow=c(1,2))
plot(elevation2)
plot(densitymap)

# Plot a multifrma of these 2 images one on top of the other
par(mfrow=c(2,1))
plot(elevation2)
plot(densitymap)

# To go back to ploting NOT in a multiframe
  # It will not close R studio, but wipe all plots
  # An alternative is to delete all plots from 
dev.off() 

# Change colours of maps with the function colorRampPalette
  # http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf <-- colours available in R
  # place the colours desired in the function and concatenate, add after in () the amount of gradients desired
  # Assign that palette to a name to be used in the plot function
cl<-colorRampPalette(c("darkolivegreen", "darkorange3", "darkolivegreen1"))(100)
cln<-colorRampPalette(c("firebrick","darkseagreen","darkslateblue"))(100)
plot(densitymap, col=cl)

# Build a multiframe of densitymap with 2 different colour palettes next to each other and add some points data on one of them
par(mfrow=c(1,2))
plot(densitymap, col=cl)
plot(densitymap, col=cln)
points(elevattion2)
