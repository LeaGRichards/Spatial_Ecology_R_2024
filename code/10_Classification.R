### Classifying imagery data in imageRy ###

# --- Setting things up --- #
  library(terra)
  library(imageRy)
  library(ggplot2)
  # install.packages("patchwork")
  library(patchwork)

  im.list()
  sun <- im.import("Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg")
  
  # Cluster areas of an image into different clusers 2 and more
    sunc <- im.classify(sun, num_clusters=3)

# --- Mato Grosso example --- #
  m1992 <- im.import("matogrosso_l5_1992219_lrg.jpg")
  m2006 <- im.import("matogrosso_ast_2006209_lrg.jpg")
  
  # Cluster the area in 2 clusters
    m1992c <- im.classify(m1992, num_clusters=2)
      # class 1 = forest
      # class 2 = human related areas + water
    m2006c <- im.classify(m2006, num_clusters=2)
      # class 1 = human related areas + water
      # class 2 = forest
    
  # Calculate frequencies, total and proportions in % of pixels
      # Number of pixels of each clusters
    f1992 <- freq(m1992c)
    f1992
    tot1992 <- ncell(m1992c) # number of cell function give the total amount of cells
    tot1992 
    
    p1992 <- f1992 * 100 / tot1992
    p1992  
      # Class 1 (forest) is 83% of pixels
      # Class 2 (human related areas + water) is 17% of pixels
    
    # 2006
    f2006 <- freq(m2006c)
    tot2006 <- ncell(m2006c)
    p2006 <- f2006 * 100 / tot2006 
    p2006    
      # Class 1 (human related areas + water) is 55% of pixels
      # Class 2 (forest) is 45% of pixels
    
  # Build a dataframe with these values
    class <- c("Forest","Human")
    y1992 <- c(83, 17)
    y2006 <- c(45, 55)
    
    tabout <- data.frame(class, y1992, y2006)
    tabout
  
  # Building a graph with this dataframe using ggplot2
    p1 <- ggplot(tabout, aes(x=class, y=y1992, color=class)) + 
      geom_bar(stat="identity", fill="white") +
      ylim(c(0, 100))
      # Shows a quantitative and visual way the amount of pixels per class
    p2 <- ggplot(tabout, aes(x=class, y=y2006, color=class)) + 
      geom_bar(stat="identity", fill="pink") +
      ylim(c(0, 100))
  
  # Using Patchwork to make a graph with both plots of 1992 and 2006 
    p1 + p2 # One next to the other
    p1 / p2 # One over the other
