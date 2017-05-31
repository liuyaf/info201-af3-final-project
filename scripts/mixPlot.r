# load library
library(plotly)
library(dplyr)


# the function takes a dataframe and colorvar as variable
# and returns a barchart listing top 100 donation to specific candidate
BuildBarchart <- function(map.df, colorvar) {
  
  # margin setting
  m <- list(l = 50, r = 50, b = 280, t = 100, pad = 4)
  
  # use string name as variable name
 
  # chart 1: barchart 
  
  # arrange bar with total contribution desc
  map.df$name <- factor(map.df$name, 
                        levels = map.df$name[order(map.df$total,decreasing = TRUE)])
  
  # build barchart
  bar <- map.df %>% plot_ly(x = ~name, y = ~total, type = 'bar',
                       name = 'Top 100 Contribution',color = eval(parse(text = colorvar)),
                       width = 1200, height = 1000) %>% 
    layout(margin = m, autosize = F)
  
  return(bar)
}


# chart 2: map
# the function takes dataframe as variable
# and returns a map showing where are the top 100 donations coming from
BuildMap <- function(map.df) { 
  
  # geo setting
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showland = TRUE,
    landcolor = toRGB("gray95"),
    subunitcolor = toRGB("gray85"),
    countrycolor = toRGB("gray85"),
    countrywidth = 0.5,
    subunitwidth = 0.5
  )
  
  # build map
  map <- map.df %>% plot_geo(lat = ~Latitude, lon = ~Longitude) %>%
    add_markers(
      hoverinfo = 'text',
      size = ~total, color = ~total, opacity = 0.8,
      text = ~paste0(name, ' <br> ',
                     industry, ' <br> ', '# of records ' ,records, ' <br> ',
                     '$',total)) %>%
    layout(title = 'Location of Top 100 Contribution<br />(Hover for details)', geo = g)

  
  return(map)
}


# chart 3: pie chart
# this function takes a dataframe as variable
# and returns a pie chart showing the parcentage of each industry's contribution
BuildPie <- function(pie.df) {
  
  # build piechart
  pie <- pie.df %>% plot_ly(labels = ~industry, values = ~percent, type = 'pie',
                       textposition = 'inside',
                       textinfo = 'label+percent',
                       hoverinfo = 'text',
                       text = ~paste0(industry, ' <br>',
                                      '$', total),
                       width = 800, height = 800) %>% 
    layout(title = 'Percentage of Industry Contribution')
  
  return(pie)
}
