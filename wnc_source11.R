#WNC Source

#Counties
counties <- c('Alexander County', 'Alleghany County', 'Ashe County', 'Avery County', 'Buncombe County', 'Burke County', 'Caldwell County', 'Catawba County', 'Cherokee County', 'Clay County', 'Graham County', 'Haywood County', 'Henderson County', 'Jackson County', 'Macon County', 'Madison County', 'McDowell County', 'Mitchell County', 'Polk County', 'Rutherford County', 'Swain County', 'Transylvania County', 'Watauga County', 'Wilkes County', 'Yancey County')
counties_too <- c("Alexander", "Alleghany", "Ashe", "Avery", "Buncombe", "Burke",
                  "Caldwell", "Catawba", "Cherokee", "Clay", "Graham", "Haywood",
                  "Henderson", "Jackson", "Macon", "Madison", "McDowell", "Mitchell",
                  "Polk", "Rutherford", "Swain", "Transylvania", "Watauga", "Wilkes",
                  "Yancey")
l <- c('Watauga', 'Wilkes', 'Ashe', 'Avery', 'Caldwell')
mountain <- c('Yancey', 'Mitchell', 'Avery', 'Watauga', 'Ashe', 'Alleghany')
foothills <- c('Wilkes', 'Caldwell', 'Burke', 'Alexander', 'McDowell')
piedmont <- c('Rutherford', 'Catawba')
hendersonville <- c('Transylvania', 'Polk', 'Henderson')
asheville <- c('Madison', 'Buncombe')
western <- c('Haywood', 'Jackson', 'Swain', 'Macon', 'Graham', 'Clay', 'Cherokee')

#Time
endpd <- as.numeric(readChar('month.txt', nchars = 2))
endpd_agg <- endpd
current_year <- as.numeric(readChar('year.txt', nchars = 4))
acting_date <- ymd(paste(current_year, endpd, '1', sep = '-'))
filter_date <- ymd(paste(current_year-2, endpd, '1', sep = '-'))

startyr <- year(filter_date)
startyr_agg <- startyr
endyr <- current_year
endyr_agg <- endyr

#Data Retrieval
LAUS_base <- 'https://accessnc.opendatasoft.com/api/records/1.0/search/?dataset=labor-force-laus&rows=-1&facet=periodyear&facet=adjusted&facet=areafacet&facet=datefacet&facet=areatyname&refine.areatyname=County&refine.periodyear='

bls_base <- paste('https://api.bls.gov/publicAPI/v2/timeseries/data/?registrationkey=14bb6f973495401788cc1e3b4025cf76&startyea=2007&endyear=', as.character(current_year),'&seriesid=', sep = '')
us_unemp <- 'LNU03000000'
nc_unemp <- 'LAUST370000000000004'
nc_rate <- 'LAUST370000000000003'
us_rate <- 'LNU04000000'

options(tz="EST")

#Functions
LAUS_call <- function(base = LAUS_base, year){
  LAUS_url <- paste(base, year, sep = '')
  assign('LAUS_url', LAUS_url, envir = .GlobalEnv)
}

get_LAUS_data <- function(year){
  url <- LAUS_call(year = year)
  require(lubridate)
  require(jsonlite)
  lines <- readLines(url)
  read <- fromJSON(lines)
  readf <- as.data.frame(read)
  index <- readf$records.fields$areaname %in% counties
  selected <- readf[index,]
  selected$dates <- ymd(selected$records.fields$date)
  return(selected)
}

past_ten <- function(){
  unemp17 <- get_LAUS_data(2017)
  unemp16 <- get_LAUS_data(2016)
  unemp15 <- get_LAUS_data(2015)
  unemp14 <- get_LAUS_data(2014)
  unemp13 <- get_LAUS_data(2013)
  unemp12 <- get_LAUS_data(2012)
  unemp11 <- get_LAUS_data(2011)
  unemp10 <- get_LAUS_data(2010)
  unemp09 <- get_LAUS_data(2009)
  unemp08 <- get_LAUS_data(2008)
  unemp07 <- get_LAUS_data(2007)
  return(list(unemp07,unemp08, unemp09, unemp10, unemp11, unemp12, unemp13, unemp14, unemp15, unemp16, unemp17))
}

get_unemp_rate <- function(sets, c){
  #sets needs to be a vector of LAUS datasets
  #c is a county name string
  startyr = year(filter_date)
  endyr = current_year
  unemployment <- c()
  for(s in sets){
    s <- as.data.frame(s)
    s <- s[order(s$dates),]
    df <- data.frame(x = s$dates, y = s$records.fields$unemprate, z = as.vector(s$records.fields$areaname))
    df2 <- na.omit(df)
    df3 <- df2[df2$z %in% c,]
    df4 <- spread(df3, x, y)
    unemployment <- append(unemployment, colMeans(df4[,-1]))
  }
  return(unemployment)
}

get_agg_unemp <- function(sets){
  endyr = year(sets[[length(sets)]]$dates[1])
  endpd = max(na.omit(month(sets[[length(sets)]]$dates))) + 1
  agg_unemp <- c()
  for(s in sets){
    df <- data.frame(x = s$dates, y = s$records.fields$unemp, z = s$records.fields$areaname)
    df <- df %>% group_by(x) %>% summarise(sum = sum(y))
    agg_unemp <- append(agg_unemp, df$sum)
  }
  return(agg_unemp)
}

make_series <- function(unemployment = unemployment, startyr = startyr, endyr = endyr, endpd = endpd){
  #from agg_unemp, must specify each fn arg
  time_series <- ts(unemployment, start = c(startyr, endpd), end = c(endyr, endpd), frequency = 12)
  assign('time_series', time_series, envir = .GlobalEnv)
}

make_grouped_data <- function(sets){
  grouped_df_all <- data.frame()
  for (raw_data in sets){
    centroid <- unique(raw_data$records.fields$geo_point_2d)
    nolist <- unlist(unique(raw_data$records.fields$geo_point_2d))
    mat <- matrix(nolist, ncol = 2, byrow = T)
    mat <- cbind(mat, unique(raw_data$records.fields$areaname))
    split <- unlist(strsplit(mat[,3], ' '))
    split2 <- matrix(split, byrow = T, ncol = 2)
    mat <- cbind(mat, split2)
    mat <- mat[order(mat[,4]),]
    grouped <- data.frame(date = raw_data$dates, unemp = raw_data$records.fields$unemp, name = raw_data$records.fields$areaname, rate = raw_data$records.fields$unemprate)
    mat <- mat[,-1]
    mat <- mat[,-1]
    colnames(mat) <- c('name', 'name2', 'county')
    matdf <- as.data.frame(mat)
    #maybe split into a few functions
    grouped$group <- 0
    grouped <- left_join(grouped, matdf, by = 'name')
    
    grouped$group[grouped$name2 %in% mountain] <- "Mountain"
    grouped$group[grouped$name2 %in% foothills] <- "Foothills"
    grouped$group[grouped$name2 %in% piedmont] <- "Piedmont"
    grouped$group[grouped$name2 %in% hendersonville] <- "Hendersonville"
    grouped$group[grouped$name2 %in% asheville] <- "Asheville"
    grouped$group[grouped$name2 %in% western] <- "Western"
    grouped_df_all <- rbind(grouped_df_all,  grouped, stringsAsFactors = F)
  }
  return(grouped_df_all)
}

change_dif <- function(data){
  #RETURNS DIFFERENCE NOT PERCENT CHANGE.
  #IF YOU WANT PCT CHANGE return(pct_chg)
  pd_1 <- ifelse(nchar(endpd-1) == 1, paste('0', (endpd-1), sep = ''), endpd)
  pd_2 <- ifelse(nchar(endpd) == 1, paste('0', endpd, sep = ''), as.character(endpd))
  if(pd_1 =='00'){
    pd_1 <- '12'
    last <- get_LAUS_data(current_year - 1)
    pd1 <- last[last$records.fields$period == pd_1,]
    pd2 <- data[data$records.fields$period == pd_2,]
    pd1_c <- cbind(pd1$records.fields$unemprate, pd1$records.fields$areaname)
    pd2_c <- cbind(pd2$records.fields$unemprate, pd2$records.fields$areaname)
    pd1_o <- pd1_c[order(pd1_c[,2]),]
    pd2_o <- pd2_c[order(pd2_c[,2]),]
    diff <- as.numeric(pd2_o[,1]) - as.numeric(pd1_o[,1])
    #pct_chg <- as.matrix((diff/as.numeric(pd1_o[,1])) * 100)
    return(diff)
    }
  else{
    pd1 <- data[data$records.fields$period == pd_1,]
    pd2 <- data[data$records.fields$period == pd_2,]
    pd1_c <- cbind(pd1$records.fields$unemprate, pd1$records.fields$areaname)
    pd2_c <- cbind(pd2$records.fields$unemprate, pd2$records.fields$areaname)
    pd1_o <- pd1_c[order(pd1_c[,2]),]
    pd2_o <- pd2_c[order(pd2_c[,2]),]
    diff <- as.numeric(pd2_o[,1]) - as.numeric(pd1_o[,1])
    #pct_chg <- as.matrix((diff/as.numeric(pd1_o[,1])) * 100)
    return(diff)
    }
  #fin <- data.frame(pct_chg, pd2$records.fields$areaname[order(pd2$records.fields$areaname)])
  #assign('fin', fin, envir = .GlobalEnv)
  }

make_empty_map <- function(){
  prev <- getwd()
  nc <- readOGR(paste(getwd(),'/CountyBoundaryShoreline_shp/CountyBoundaryShoreline_polys.shp', sep = ''))
  index <- nc@data$NAME_LOCAS %in% counties_too
  nc2 <- nc[index,]
  nc2 <- nc2[order(nc2@data$NAME_LOCAS),]
  setwd(prev)
  return(nc2)
}

make_choropleth <- function(change, nc2 = nc2){
  cut_fn <- ifelse(length(unique(round(change, digits = 2))) < 3, rangeCuts, quantileCuts)
  shades <- auto.shading(change, n = 5, cutter = rangeCuts, cols = brewer.pal(5, "Reds"))
  wnc_choro <- choropleth(nc2, change, shades, 
                          main = paste('Western NC Unemployment', '\n', as.character(lubridate::month(acting_date, label = TRUE, abbr = FALSE)),' ' , as.character(endyr), sep = ''),
                          sub = 'Note: Low unemployment rates are better than high unemployment rates.', cex.sub = .5)
  choro.legend(406832.7, 1196924, title = "Change in Rate", sh = shades, cex = .75, bty = 'n', space_reduction = 40000)
}

bls_unemp <- function(id, startyear = startyr_agg, endyear = current_year, bls_base = bls_base){
  payload <- list('seriesid' = id, 'startyear' = startyear, 'endyear' = endyear)
  res <- blsAPI(payload)
  json <- fromJSON(res)
  data <- as.data.frame(json$Results$series$data)
}

make_level_series <- function(){
  us_tot_df <- bls_unemp(id = us_unemp, startyear = startyr_agg, endyear = current_year, bls_base = bls_base)
  nc_tot_df <- bls_unemp(id = nc_unemp, startyear = startyr_agg, endyear = current_year, bls_base = bls_base)

  nc_tot_df <- nc_tot_df[order(nc_tot_df$year, nc_tot_df$period),]
  nc_tot_df <- as.numeric(nc_tot_df$value)
  
  us_tot_df <- us_tot_df[order(us_tot_df$year, us_tot_df$period),]
  us_tot_df <- as.numeric(us_tot_df$value)
  
  u <- unemployment[-seq(1,length(unemployment), 13)]
  
  us_series <- make_series(unemployment = (1000*us_tot_df), startyr = startyr, endyr = endyr, endpd = endpd)
  nc_series <- make_series(unemployment = nc_tot_df, startyr = startyr, endyr = endyr, endpd = endpd)
  wnc_series <- make_series(unemployment = rate_series, startyr = startyr_agg, endyr = endyr_agg, endpd = endpd_agg)
  
  assign('us_series', us_series, envir = .GlobalEnv)
  assign('nc_series', nc_series, envir = .GlobalEnv)
  assign('wnc_series', wnc_series, envir = .GlobalEnv)
  }

make_rate_series <- function(){
  us_rate_df <- bls_unemp(id = us_rate, startyear = startyr_agg, endyear = current_year, bls_base = bls_base)
  nc_rate_df <- bls_unemp(id = nc_rate, startyear = startyr_agg, endyear = current_year, bls_base = bls_base)
  
  us_series <- make_series(unemployment = us_rate_df$value, startyr = startyr, endyr = endyr, endpd = endpd)
  nc_series <- make_series(unemployment = nc_rate_df$value, startyr = startyr, endyr = endyr, endpd = endpd)
  
  assign('us_rate_series', us_series, envir = .GlobalEnv)
  assign('nc_rate_series', nc_series, envir = .GlobalEnv)
  }

make_wnc_table <- function(){
  this_yr <- as.data.frame(yrs_list[length(yrs_list)], row.names = NULL)
  last_yr <- as.data.frame(yrs_list[length(yrs_list) - 1], row.names = NULL)
  
  this_yr <- data.frame('dates' = this_yr$dates, 'areaname' = this_yr$records.fields$areaname, 'rate' = this_yr$records.fields$unemprate)
  last_yr <- data.frame('dates' = last_yr$dates, 'areaname' = last_yr$records.fields$areaname, 'rate' = last_yr$records.fields$unemprate)
  this_yr <- na.omit(this_yr)
  last_yr <- na.omit(last_yr)
  wnc_table <- rbind(this_yr, last_yr)
  wnc_table <- wnc_table %>% spread(key = lubridate::month(dates), value = rate )
  wnc_table <- data.frame(wnc_table[,1], wnc_table[,8:9], wnc_table[,20:21])
  #the month selection is hardcoded not flexible
  
  pctchg <- (wnc_table[,5]-wnc_table[,3])
  #wnc_table <- data.frame('dates' = current$dates, 'areaname' = current$records.fields$areaname, 'rate' = current$records.fields$unemprate)
  #wnc_table <- wnc_table %>% spread(key = month(dates), value = rate )
  wnc_table <- data.frame('County' = wnc_table[,1], 
                          'Prev Month Last Yr' = wnc_table[,2],
                          "Current Month Last Yr" = wnc_table[,3],
                          'Prev Month' = wnc_table[,5],
                          "Current Month" = wnc_table[,4],
                          'Rank Current Month' = rank(wnc_table[,5], ties.method = 'first'),
                          'Change' = pctchg,
                          'Rank Change' = rank(pctchg, ties.method = 'first'),
                          check.names = F
  )
  return(wnc_table)
}

choro.legend <- function (px, py, sh, under = "under", over = "over", between = "to", 
                          fmt = "%g", cex = 1, space_reduction = 0, ...) 
{
  x = sh$breaks
  lx = length(x)
  if (lx < 3) 
    stop("break vector too short")
  res = character(lx + 1)
  res[1] <- ifelse(x[1] != 0,  paste(under, sprintf(fmt, x[1])), sprintf(fmt, x[1]))
  
  for (i in 1:(lx - 1)) res[i + 1] <- paste(sprintf(fmt, x[i]), 
                                            between, sprintf(fmt, x[i + 1]))
  res[lx + 1] <- paste(over, sprintf(fmt, x[lx]))
  maxwidth <- max(strwidth(res)) - space_reduction
  temp <- legend(x = px, y = py, legend = rep(" ", length(res)), 
                 fill = sh$cols, text.width = maxwidth, cex = cex, ...)
  text(temp$rect$left + temp$rect$w, temp$text$y, res, pos = 2, 
       cex = cex)
}
