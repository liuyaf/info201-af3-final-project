library(geojsonio)
library(leaflet)
library(ggmap)
library(dplyr)
library(sp)

# df <- as.data.frame(read.csv("../data/WashingtonSenateContributions.csv", stringsAsFactors = FALSE))
# locations <- as.data.frame(read.csv("../data/zip_codes_states.csv",stringsAsFactors = FALSE))
# locations$city <- toupper(locations$city)
# 
# df.2 <- df %>% group_by(State, City, General_Party)  %>% summarise(Amount = sum(Amount), records = n())
# 
# locations.2 <- locations %>% group_by(state, city) %>% summarise(lat = min(latitude), lon = min(longitude))
# 
# known <- left_join(df.2, locations.2, by = c("State" = "state", "City" = "city"))
# 
# BuildScatterMap(known, lat = "lat", lon = "lon", label = "<strong>%s, 
#                 %s</strong><br/>$%g in Contributions<br/>%s", color = "General_Party")

# Pass in dataframe with df, colnames of lat, long, label string,
# df should be: state, city, Amount, lat, long, color
BuildScatterMap <- function(out.map) {
  out.map$size <- ntile(out.map$Amount, 10) 
  
  labels <- sprintf(
    "<strong>%s, %s</strong><br/>$%g in Contributions<br/>%s",
    out.map$City, out.map$State, out.map$Amount, out.map$General_Party
  ) %>% lapply(htmltools::HTML) 
  
  factpal <- colorFactor(c("blue", "red", "green"), domain = c("Democratic", "Republican", "Third-Party"))
  
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