###
# Seaview Survey Functions
##

# Function that splits the depth vector from the altimeter dataframe 
# according to a grep pattern and returns a vector depths in metres

altimeter.depth.split <- function(vector, splitter) {
                         depth_metres <- strsplit(as.character(vector), 
                                                  splitter)
                         metres <- NULL 
                         for(i in 1:length (depth_metres)) {
                           subset <- depth_metres [[i]] [4]
                           metres <- append (metres, subset)
                         }
                         return(metres)
}

# Function to convert to POSIX time
time.as.POSIXct <- function(vector, formatting) {
                   vec     <- strptime(vector, format = formatting)
                   posixct <- as.POSIXct(vec)
                   return(posixct)
}

# Standard error of mean given stdev and n # samples
se <- function(standev, n) {
  rt_n <- sqrt(n)
  se   <- (standev / rt_n)
  return(se)
}

# isolate number (including decimal place) from string
takenum <- function(y) {
  as.numeric(gsub("[^0-9]/.", "", x=y))
}

# Remove extra spaces from GPS coordinates and replace with a ˚ symbol
CoordFix <- function(x){
  require("stringr")
  replace_deg <- str_replace(x, '[" "]', "°")
  replace_sp  <- gsub(" ", "", replace_deg)
}

# Convert Lat/Long to decimal degrees position
LatLongToDD <- function(coord){
  tmp1  <- strsplit(coord[1], "°")
  tmp2  <- strsplit(tmp1[[1]][2], "'")
  tmp3  <- strsplit(tmp2[[1]][2], "\"")
  dec   <- c(as.numeric(tmp1[[1]][1]), 
             as.numeric(tmp2[[1]][1]), 
             as.numeric(tmp3[[1]]))
  c <- abs((dec[1]) + dec[2] / 60 + dec[3] / 3600)
  c <- ifelse(dec[1] < 0, -c, c)
  return(c)
}

turn.neg <- function(x){
  x * - 1
}

raster.and.mask <- function(points, rast, z, fun, poly){
  rast.full <- rasterize(points, rast, z, fun)
  rast.crop <- crop(rast.full, extent(poly))
  rast.mask <- mask(rast.crop, poly)
  return(rast.mask)
}

aggregate.interpolate.Tps <- function(raster, aggregate.factor){
  ra  <- aggregate(raster, aggregate.factor)
  xy  <- data.frame(xyFromCell(ra, 1:ncell(ra)))
  v   <- getValues(ra)
  tps <- Tps(xy, v)
  return(tps)
}


####
Blank.Raster<-function(extent, resolution){
  rast<-raster()
  extent(rast)<-extent(extent)
  res(rast)<-resolution
  proj4string(rast)<-proj4string(extent)
  return(rast)
}




panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
usr <- par("usr"); on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r = (cor(x, y,use="pairwise"))
txt <- format(c(r, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, cex = cex )
}

panel.cor.scale <- function(x, y, digits=2, prefix="", cex.cor)
{
usr <- par("usr"); on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r = (cor(x, y,use="pairwise"))
txt <- format(c(r, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, cex = cex * abs(r))
}


panel.hist <- function(x, ...)
{
usr <- par("usr"); on.exit(par(usr))
par(usr = c(usr[1:2], 0, 1.5) )
h <- hist(x, plot = FALSE)
breaks <- h$breaks; nB <- length(breaks)
y <- h$counts; y <- y/max(y)
rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}


pairs.panels <- function (x,y,smooth=TRUE,scale=FALSE)
{if (smooth ){
if (scale) {
pairs(x,diag.panel=panel.hist,upper.panel=panel.cor.scale,lower.panel=panel.smooth)
}
else {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor,lower.panel=panel.smooth)
} #else {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor,lower.panel=panel.smooth)
}
else #smooth is not true
{ if (scale) {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor.scale)
} else {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor) }
} #end of else (smooth)
} #end of function

# function to return max values, or second-max values, or index thereof.
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]

# convert factor to numeric
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

# predict onto a raster, using values from a raster stack, and collate Standard Error, too.
pred.and.se <- function(model, data) {
  v <- predict(model, data, se.fit=TRUE, type='response')
  cbind(p=as.vector(v$fit), se=as.vector(v$se.fit))
}