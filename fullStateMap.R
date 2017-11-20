source("~/")
setwd("~/Downloads/cb_2016_us_county_5m")

make_empty_map <- function(){
  prev <- getwd()
  #setwd(paste(getwd(), '/CountyBoundaryShoreline_shp', sep = ''))
  nc <- readOGR('cb_2016_us_county_5m.shp')
  nc2 <- nc[nc$STATEFP == '37',]
  return(nc2)
}

get_LAUS_data <- function(year){
  url <- LAUS_call(year = year)
  require(lubridate)
  require(jsonlite)
  lines <- readLines(url)
  read <- fromJSON(lines)
  readf <- as.data.frame(read)
  #index <- readf$records.fields$areaname %in% counties
  #selected <- readf[index,]
  readf$dates <- ymd(readf$records.fields$date)
  return(readf)
}


make_choropleth <- function(change, nc2 = nc2){
  shades <- auto.shading(change, n = 9, cutter = quantileCuts, cols = brewer.pal(9, "Reds"))
  wnc_choro <- choropleth(nc2, change, shades, 
                          main = paste('Western NC Unemployment', '\n', as.character(month(max(current$dates), label = T, abbr = F)),' ' , as.character(endyr), sep = ''),
                          sub = 'Note: Low unemployment rates are better than high unemployment rates.', cex.sub = .5)
  choro.legend(-83.5, 35, title = "Change in Rate", sh = shades, cex = .75, bty = 'n')
}

plot(nc)
current_yr <- get_LAUS_data(2017)
nc@data <- nc@data[order(nc@data$NAME),]
change <- change(current_yr)
change <- abs(change)

make_choropleth(change = change, nc2 = nc)

check <- rbind(change, nc@data$NAME)

