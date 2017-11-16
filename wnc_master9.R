#WNC Master
library(lubridate)
library(dplyr)
library(jsonlite)
library(ggplot2)
#library(plotly)
library(GISTools)
library(rgdal)
library(formattable)
library(blsAPI)
library(TTR)
library(tidyr)
setwd("~/Desktop/Programming/R/School/GA")
source('wnc_source7.R')
getwd()

current_year  <- 2017

#Returns a list of unemployment information from 2007 - 2017.
current <- get_LAUS_data(current_year)
yrs_list <- past_ten()

#Aggregates unemployment to one vector.
unemployment <- get_agg_unemp(yrs_list)
rate <- get_unemp_rate(yrs_list, counties)

#Makes the above a time series object.
series <- make_series(unemployment = unemployment, startyr = startyr_agg, endyr = endyr_agg, endpd = endpd_agg)
rate_series <- make_series(unemployment = rate, startyr = startyr_agg, endyr = endyr_agg, endpd = endpd_agg)
rate_series_d <- decompose(rate_series, type = 'mult')
mov <- SMA(rate_series)

#Grouped graph
grouped <- make_grouped_data(yrs_list)
grouped2 <- grouped %>% filter(date >= '2015-01-01') %>% group_by(date, group) %>% summarise(sum = sum(unemp))
c_graph <- ggplot(data = grouped2, aes(x = date, y = sum, colour = group)) +
geom_line() + theme_minimal() + ylim(0, (max(grouped2$sum) + 1500)) + labs (x = 'Date', 
                                                       y = 'Number Unemployed', 
                                                       caption = as.character("Groups are defined by county population density.\nAsheville: Madison, Buncombe.\nFoothills: Wilkes, Caldwell, Burke, Alexander, McDowell.\nHendersonville: Transylvania, Polk, Henderson.\nMountain: Yancey, Mitchell, Avery, Watauga, Ashe, Alleghany.\nPiedmont: Rutherford, Catawba.\nWestern: Haywood, Jackson, Swain, Macon, Graham, Clay, Cherokee."),
                                                       legend.title = 'Group', 
                                                       colour = 'Group',
                                                       title = 'WNC Level of Unemployment') + theme(plot.caption = element_text(size = 4, hjust = 0))
png(filename='counties_level.png', 
    width = 5,
    height = 4,
    units = 'in',
    res = 300)
plot(c_graph)
dev.off()

#Same as above, but with unemployment rate.
grouped_rate <- grouped %>% filter(date >= '2015-01-01') %>% group_by(date, group) %>% summarise(avg = mean(rate))
rate_graph <- ggplot(data = grouped_rate, aes(x = date, y = avg, colour = group)) +
  geom_line() + theme_minimal() + labs (x = 'Date', 
                                        y = 'Unemployment Rate', 
                                        caption = as.character("Groups are defined by county population density.\nAsheville: Madison, Buncombe.\nFoothills: Wilkes, Caldwell, Burke, Alexander, McDowell.\nHendersonville: Transylvania, Polk, Henderson.\nMountain: Yancey, Mitchell, Avery, Watauga, Ashe, Alleghany.\nPiedmont: Rutherford, Catawba.\nWestern: Haywood, Jackson, Swain, Macon, Graham, Clay, Cherokee."), 
                                        legend.title = 'Group', 
                                        colour = 'Group',
                                        title = 'Western North Carolina Unemployment Rate') + theme(plot.caption = element_text(size = 4, hjust = 0))
png(filename='counties_rate.png', 
    width = 5,
    height = 4,
    units = 'in',
    res = 300)
plot(rate_graph)
dev.off()

grouped$fuc <- as.character(grouped$name2)
local <- grouped[grouped$fuc %in% l,]
local_rate <- local %>% filter(date >= '2015-01-01') %>% group_by(date, name2) %>% summarise(avg = mean(rate))
g <- ggplot(data = local_rate, aes(x = date, y = avg, colour = name2)) +
  geom_line() + theme_minimal() + labs (x = 'Date', 
                                        y = 'Unemployment Rate',                                         legend.title = 'Group', 
                                        colour = 'County',
                                        title = 'Local Unemployment Rate')

png(filename='local_rate.png', 
    width = 5,
    height = 4,
    units = 'in',
    res = 300)
plot(g)
dev.off()

#Makes a choropleth map of Unemployment Rate % Change.
change <- percent_change(current)
change2 <- abs(change)
#wnc_choro <- return_choropleth(change = change, counties_too = counties_too)
nc2 <- make_empty_map()
png(filename='wnc_map.png',
    width = 4.25,
    height = 4,
    units = 'in',
    res = 300)
par(mar = c(0,0,4,0))
make_choropleth(change = change2, nc2 = nc2)
dev.off()

#Get data from BLS.
make_rate_series()
make_level_series()

latest <- max(current$dates)
dates <- seq(ymd('2007-1-1'), ymd(paste(as.character(year(latest)), as.character(month(latest)+1), '1', sep = "-" )), by = 'month')

######Graph 1#######
#compare_level <- data.frame(nc_series, wnc_series)
#cts <- ts(compare_level)
#autoplot(cts, facets = F)

compare_rate <- data.frame('US' = as.numeric(rev(us_rate_series)), 'NC' = as.numeric(rev(nc_rate_series)), 'WNC' = rate_series, 'date' = dates)
compare_rate <- melt(compare_rate, id = c('date'))
compare_rate <- compare_rate %>% filter(date >= '2015-01-01')

compare_graph <- ggplot() + 
  geom_line(data = compare_rate, aes(x = compare_rate$date, y = compare_rate$value, color = compare_rate$variable)) +
  theme_minimal() +
  labs(y = 'Unemployment Rate',
       x = 'Date',
       title = 'Unemployment Rate',
       subtitle = 'Comparison between US, NC, WNC.') +
  theme(plot.margin = margin(4,9,4,4)) +
  guides(color = guide_legend('Group'))

#compare_graph <- compare_graph + scale_fill_discrete(name = "Group")

png(filename='compare_graph.png',
    width = 4.5,
    height = 4,
    units = 'in',
    res = 300)
plot(compare_graph)
dev.off()

#####Make table#####
#sort by date and county

wnc_table <- make_wnc_table()

table <- formattable(wnc_table, list('Prev Month Last Yr' = color_tile('transparent', 'yellow'),
                            'Current Month Last Yr' = color_tile('transparent', 'yellow'),
                            'Prev Month' = color_tile('transparent', 'red'),
                            'Current Month' = color_tile('transparent', 'red'),
                            'Change' = color_tile('transparent', 'blue')
                            )
            )
#save(table, file = 'table.html')
library(htmltools)

#library(htmlwidgets)
#table <- as.htmlwidget(table)
#saveWidget(widget=table, file='table.html', selfcontained = T)
save_html(table, 'table.html')
webshot::webshot("table.html", file='table_out.png', delay=5, zoom = 5)
