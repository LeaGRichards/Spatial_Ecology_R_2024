### Time series analysis ###

library(terra)
library(imageRy)

# Listing all the available files
im.list()

## EN --> European nitrogen data (NOx in atmosphere), mainly dioxyde
  # RGB layers with non-colorblind friendly package

  EN01 <- im.import("EN_01.png") 
  EN13 <- im.import("EN_13.png") # March

  # Plot a multiframe of both images
  par(mfrow=c(1,2))
  plot(EN01)
  plot(EN13)
    # We can observe a big decrease during the stop of human activity

  # Differencing images in time --> taking the dataset and observing the difference between both
  dev.off() # to close the multiframe
  difEN = EN01[[1]] - EN13[[1]]
  plot(difEN)

## Example 2: Greenland data

  # Importing 4 data sets about greenland
    # 2000, 2005, 2010, 2015
  gr <- im.import("greenland")
  
  # Plot a dataset of first and last dataset 
  par(mfrow=c(1,2))
  plot(gr[[1]])
  plot(gr[[4]])

  # Differencing images in time --> taking the dataset and observing the difference between both
  difgr <- gr[[1]] - gr[[4]]
  dev.off()
  plot(difgr)
    # These areas have decreasing temperature
  
  # Plotting temperature highs
  im.plotRGB(gr, r=1, g=2, b=4)
    # Every red pixel had a higher temperature value in 2000, and every green had a higher value in 2005 and every blue is 2015
  
  # Plotting different frequencies
  im.ridgeline(gr, scale=2, option="A")
    # the professor is developing this plot
