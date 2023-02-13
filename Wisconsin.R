# Import original data
load("/Users/sunxiaotan/Desktop/University of Toronto/STA442/STA442 A4/wisconsin.RData")

# Fit BYM model
resTrump = diseasemapping::bym(trump ~ logPdens + propWhite +
  propInd, data = wisconsinCsubm, prior = list(sd = c(log(2.5),
  0.5), propSpatial = c(0.5, 0.5)), Ntrials = wisconsinCsubm$Total,
  family = "binomial")

# Save BYM model results
save(resTrump, file = "/Users/sunxiaotan/Desktop/STA442 A4/resWisconsin.RData", compress = "xz")

# Import BYM model results
load("/Users/sunxiaotan/Desktop/STA442 A4/resWisconsin.RData")

# Graphs
theColTrump = mapmisc::colourScale(wisconsinCsubm$propTrump,
  col = "RdBu", breaks = sort(unique(setdiff(c(0, 1, seq(0.2,
    0.8, by = 0.1)), 0.5))), style = "fixed", rev = TRUE)
theColPop = mapmisc::colourScale(wisconsinCsubm$pdens, col = "Spectral",
  breaks = 11, style = "equal", transform = "log", digits = 1,
  rev = TRUE)
theColWhite = mapmisc::colourScale(wisconsinCsubm$propWhite,
  col = "Spectral", breaks = c(0, 0.5, 0.8, 0.9, seq(0.9,
    1, by = 0.02)), style = "fixed", rev = TRUE)
theColInd = mapmisc::colourScale(wisconsinCsubm$propInd,
  col = "Spectral", breaks = seq(0, 1, by = 0.1), style = "fixed",
  rev = TRUE)
theBg = mapmisc::tonerToTrans(mapmisc::openmap(wisconsinCm,
  fact = 2, path = "stamen-toner"), col = "grey30")
theInset = mapmisc::openmap(wisconsinCm, zoom = 6, path = "stamen-watercolor",
  crs = mapmisc::crsMerc, buffer = c(0, 1500, 100, 700) *
1000)
library("sp")
mapmisc::map.new(wisconsinCsubm, 0.85)
sp::plot(wisconsinCsubm, col = theColTrump$plot, add = TRUE,
  lwd = 0.2)
raster::plot(theBg, add = TRUE, maxpixels = 10^7)
mapmisc::insetMap(wisconsinCsubm, "bottomright", theInset,
  outer = TRUE, width = 0.35)
mapmisc::scaleBar(wisconsinCsubm, "top", cex = 0.8)
mapmisc::legendBreaks("topright", theColTrump, bty = "n",
  inset = 0)
mapmisc::map.new(wisconsinCsubm, 0.85)
plot(wisconsinCsubm, col = theColPop$plot, add = TRUE, lwd = 0.2)
plot(theBg, add = TRUE, maxpixels = 10^7)
mapmisc::legendBreaks("right", theColPop, bty = "n", inset = 0)
mapmisc::map.new(wisconsinCsubm, 0.85)
plot(wisconsinCsubm, col = theColInd$plot, add = TRUE, lwd = 0.2)
plot(theBg, add = TRUE, maxpixels = 10^7)
mapmisc::legendBreaks("right", theColInd, bty = "n", inset = 0)
mapmisc::map.new(wisconsinCsubm, 0.85)
plot(wisconsinCsubm, col = theColWhite$plot, add = TRUE,
  lwd = 0.2)
plot(theBg, add = TRUE, maxpixels = 10^7)
mapmisc::legendBreaks("right", theColWhite, bty = "n", inset = 0)
theColRandom = mapmisc::colourScale(resTrump$data$random.mean,
  col = "Spectral", breaks = 11, style = "quantile", rev = TRUE,
  dec = 1)
theColFit = mapmisc::colourScale(resTrump$data$fitted.invlogit,
  col = "RdBu", rev = TRUE, breaks = sort(unique(setdiff(c(0,
    1, seq(0.2, 0.8, by = 0.1)), 0.5))), style = "fixed")
mapmisc::map.new(wisconsinCsubm, 0.85)
plot(resTrump$data, col = theColRandom$plot, add = TRUE,
lwd = 0.2)
6
plot(theBg, add = TRUE, maxpixels = 10^7)
mapmisc::legendBreaks("topright", theColRandom)
mapmisc::map.new(wisconsinCsubm, 0.85)
plot(resTrump$data, col = theColFit$plot, add = TRUE, lwd = 0.2)
plot(theBg, add = TRUE, maxpixels = 10^7)
mapmisc::legendBreaks("topright", theColFit)

# Summary of transformed model output
parTable<-resTrump$parameters$summary[, paste0(c(0.5,0.025, 0.975), "quant")]
knitr::kable(exp(parTable), digits = 5)
