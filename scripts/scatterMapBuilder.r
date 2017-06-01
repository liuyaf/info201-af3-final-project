# Matthew Li
# scatterMapBuilder.r
# Contains a function for creating a leaflet map when passed in a specific dataframe.

library(geojsonio)
library(leaflet)
library(ggmap)
library(dplyr)
library(sp)


# Pass in dataframe with df, colnames of lat, long, label string,
# df should be: state, city, Amount, lat, long, color
BuildScatterMap <- function(out.map) {
  
  # create sizes based on percentile. Places cities in 1-10, 10 being top 10% contributor,
  # 1 being bottom 10% contributor
  out.map$size <- ntile(out.map$Amount, 10) 
  
  # create labels
  labels <- sprintf(
    "<strong>%s, %s</strong><br/>$%g in Contributions<br/>%s",
    out.map$City, out.map$State, out.map$Amount, out.map$General_Party
  ) %>% lapply(htmltools::HTML) 
  
  # Color pallet used for coloring markers on map.
  factpal <- colorFactor(c("blue", "red", "green"), domain = c("Democratic", "Republican", "Third-Party"))
  
  # Leaflet map production.
  leaflet(out.map) %>% addTiles() %>%
    setView(-96, 37.8, 4) %>%
    addProviderTiles(providers$CartoDB.DarkMatter) %>%
    addCircleMarkers(
      ~lon, ~lat,
      radius = ~size,
      stroke = FALSE, fillOpacity = 0.7,
      label = labels,
      color = ~factpal(General_Party)
    ) %>%
    addLegend("bottomright", pal = factpal, values = ~General_Party,
              title = "Party Affiliation",
              labFormat = labelFormat(prefix = ""),
              opacity = 1
    ) %>% return()
}