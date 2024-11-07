### Multivariate Analysis of Communities ###
 # Observe relationship of space and spp composing the community #

  # Install package Vegan
  install.packages("vegan")
  library(vegan)

## Dune data
  # Loading dune dataset with data() function
  data("dune")
  # Show first 6 lines of dune dataset with head() function
  head(dune)
  # Show dataset in table with View() function
  View(dune)

## Multivariate Analysis of dune dataset
  # Detrended Correspondance Anaysis with function decorana()
  # Similar to PCA but powerful when range of data is quite dispersed, good at compacting it mathematically
  multivar<- decorana(dune)
    # This shows us the length of different axis which is the amount of representation of each axis
    # We can use a maximum of 3 axis (since our brain works in maximum 3 dimensions)
  
  # Calculate proportions(%) of representation of each axis
    # This will help us know how much information we loose or rather how much information we use 
    # when compacting the data mathematically into 2 dimensions
    dca1 = 3.7004
    dca2 = 3.1166
    dca3 = 1.30055
    dca4 = 1.47888
    total = dca1 + dca2 + dca3 + dca4
    # Proportions
    prop1 = dca1/total
    prop2 = dca2/total
    prop3 = dca3/total
    prop4 = dca4/total
    # Percentage
    perc1 = dca1/total*100
    perc2 = dca2/total*100
    perc3 = dca3/total*100
    perc4 = dca4/total*100

  # Variability is most explained by which group of 2 axis
    perc1+perc2 # = 71.03683 %
    # As dca1 and dca2 together explain most of the variability 
    # = will use them to plot the multivariate analysis
    # We use ~71% of information in data
    # So we loose ~29% of information in data by choosing to use these 2 axis
    
    
  # Plot multivariate analysis of dune
    # Passing from big table of unexplainable/unobservable data to a graph 
    # showing if species have a strong or weak relationship together
   plot(multivar)  
