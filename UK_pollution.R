# Import original data
load("/Users/sunxiaotan/Desktop/University of Toronto/STA442/STA442 A4/England_shp.RData")

# Create new variable
UK_shp$logExpected = log(UK_shp$E)

# Remove an island
UK2 = UK_shp[grep("Wight", UK_shp$Name, invert = TRUE),]

# Fit BYM model
englandRes = diseasemapping::bym(cases ~ offset(logExpected) +
  Ethnicity + modelledpm25 + Unemployment, prior = list(sd = c(0.5,
  0.5), propSpatial = c(0.5, 0.5)), family = "poisson",data = UK2)

# Save BYM model results
save(englandRes, file = "/Users/sunxiaotan/Desktop/STA442 A4/englandRes.RData")

# Graphs
casesCol = mapmisc::colourScale(UK2$cases, dec = -3, breaks = 12,
  col = "Spectral", style = "quantile", rev = TRUE)
Ecol = mapmisc::colourScale(UK2$E, breaks = casesCol$breaks,
  col = casesCol$col, style = "fixed")
pmCol = mapmisc::colourScale(UK2$modelledpm25, breaks = 9,
  dec = 0, style = "quantile")
ethCol = mapmisc::colourScale(UK2$Ethnicity, breaks = 9,
  digits = 1, style = "quantile")
uCol = mapmisc::colourScale(UK2$Unemployment, breaks = 12,
  dec = 0, style = "quantile")
rCol = mapmisc::colourScale(englandRes$data$random.mean,
  breaks = 12, dec = -log10(0.25), style = "quantile")
fCol = mapmisc::colourScale(englandRes$data$fitted.exp,
  breaks = 9, dec = 1, style = "quantile")
insetEngland1 = mapmisc::openmap(UK2, zoom = 3, fact = 4,
  path = "waze", crs = CRS("+init=epsg:3035"))
library("raster")
insetEngland = raster::crop(insetEngland1, extend(extent(insetEngland1),
  -c(25, 7, 4, 9.5) * 100 * 1000))
library("sp")
mapmisc::map.new(UK2)
mapmisc::insetMap(UK_shp, "topright", insetEngland, width = 0.4)
plot(UK2, col = casesCol$plot, add = TRUE, lwd = 0.2)
mapmisc::legendBreaks("left", casesCol, bty = "n")
mapmisc::map.new(UK2)
plot(UK2, col = Ecol$plot, add = TRUE, lwd = 0.2)
mapmisc::legendBreaks("left", casesCol, bty = "n")
mapmisc::map.new(UK2)
plot(UK2, col = pmCol$plot, add = TRUE, lwd = 0.2)
mapmisc::legendBreaks("left", pmCol, bty = "n")
mapmisc::map.new(UK2)
plot(UK2, col = ethCol$plot, add = TRUE, lwd = 0.2)
mapmisc::legendBreaks("left", ethCol, bty = "n")
mapmisc::map.new(UK2)
plot(UK2, col = uCol$plot, add = TRUE, lwd = 0.2)
mapmisc::legendBreaks("left", uCol, bty = "n")
mapmisc::map.new(UK2)
plot(UK2, col = rCol$plot, add = TRUE, lwd = 0.2)
mapmisc::legendBreaks("left", rCol, bty = "n")
mapmisc::map.new(UK2)
plot(UK2, col = fCol$plot, add = TRUE, lwd = 0.2)
mapmisc::legendBreaks("left", fCol, bty = "n")

# Summary of transformed model output
PARTABLE<-englandRes$parameters$summary[, c(1, 3:5)]
knitr::kable(exp(PARTABLE), digits = 5)
