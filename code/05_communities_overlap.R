### Estimate temporal overlap between species of a community ###

# Observe relationship of TIME and spp composing the community #
  # Example prey-predator relationship through time

# Install package overlap
  install.packages("overlap")
  library(overlap)

# Kerinci data (a natural reserve)
  data(kerinci)
  head(kerinci)
  summary(kerinci)

# Convert time in radians --> time is a linear dimension, this will make it a circular one
  # data * 2 pi =  data in rad
  # Convert time by creating a new column witin the kerinci dataset called timecirc
  kerinci$timecirc <- kerinci$Time * 2 * pi
  
# Isolation of only tiger data within a new object
  # Because the language is sql, we need to put a , to close the quiery
  tiger <- kerinci[kerinci$Sps=="tiger",]
  head(tiger)
  tigertime <- tiger$timecirc
    
# Make a density plot of time data of tiger data
  densityPlot(tigertime, rug=T) # from overlap package; with rug=T original data shows at bottom of plot
  densityplot(tigertime) # from lattive package, it does not stretch over 24h
    
# Isolation + density plot of macaque data
  macaque <- kerinci[kerinci$Sps=="macaque",]
  head(macaque) 
  macaquetime <- macaque$timecirc
  densityPlot(macaquetime, rug=T)

# Overlap both tiger and macaque denisty plots
  overlapPlot(tigertime, macaquetime)
  # Visual estimation shows that the macaque avoid peak acitve time of tigers
  # They are active during the day
  # There is some overlap during the day = macaque are most at risk of tiger attack (possibly)

## --- SQL language tests --- ##
  # == means equals, != means does NOT equal
  nomacaque <- kerinci[kerinci$Sps!="macaque",]
  summary(nomacaque)  # macaque does not exist in this dataset
