library(geojsonio)
library(leaflet)
library(ggmap)
library(dplyr)
library(sp)


df <- as.data.frame(read.csv("../data/WashingtonSenateContributions.csv", stringsAsFactors = FALSE))
locations <- as.data.frame(read.csv("../data/zip_codes_states.csv",stringsAsFactors = FALSE))
locations$city <- toupper(locations$city)

df <- df %>% group_by(State, City)  %>% summarise(total = sum(Amount), records = n())

locations.2 <- locations %>% group_by(state, city) %>% summarise(lat = min(latitude), lon = min(longitude))

known <- left_join(df, locations.2, by = c("State" = "state", "City" = "city"))



known$size <- ntile(known$total, 10)  

map.label <- "<strong>%s</strong><br/>$%g in Contributions<br/>%g Contributor(s)"


labels <- sprintf(
  map.label,
  eval(parse(text = "known$City")), known$total, known$records
) %>% lapply(htmltools::HTML) 


leaflet(known) %>% addTiles() %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addCircleMarkers(
    ~lon, ~lat,
    radius = ~size,
    stroke = FALSE, fillOpacity = 0.5,
    label = labels
  )

df <- as.data.frame(read.csv("../data/WashingtonSenateContributions.csv", stringsAsFactors = FALSE))
locations <- as.data.frame(read.csv("../data/zip_codes_states.csv",stringsAsFactors = FALSE))
locations$city <- toupper(locations$city)

df.2 <- df %>% group_by(State, City, General_Party)  %>% summarise(value = sum(Amount), records = n())

locations.2 <- locations %>% group_by(state, city) %>% summarise(lat = min(latitude), lon = min(longitude))

known <- left_join(df.2, locations.2, by = c("State" = "state", "City" = "city"))

BuildScatterMap(known, lat = "lat", lon = "lon", label = "<strong>%s, 
                %s</strong><br/>$%g in Contributions<br/>%s", color = "General_Party")

# Pass in dataframe with df, colnames of lat, long, label string,
# df should be: state, city, value, lat, long, color
BuildScatterMap <- function(df, lat = "lat", lon = "lon", label = "label", color = "color") {
  lat.equation <- paste0('~', lat)
  lon.equation <- paste0('~', lon)
  color.equation <- paste0('~', color)
  
  df$size <- ntile(df$value, 10) 
  
  labels <- sprintf(
    label,
    df$City, df$State, df$value, df$General_Party
  ) %>% lapply(htmltools::HTML) 
  
  factpal <- colorFactor(c("blue", "red", "green"), domain = c("Democratic", "Republican", "Third-Party"))
  #qpal <- colorQuantile("RdYlBu", eval(parse(text = paste0("df$",color))), n = 5)
  
  leaflet(df) %>% addTiles() %>%
    setView(-96, 37.8, 4) %>%
    addProviderTiles(providers$CartoDB.DarkMatter) %>%
    addCircleMarkers(
      eval(parse(text = lon.equation)), eval(parse(text = lat.equation)),
      radius = ~size,
      stroke = FALSE, fillOpacity = 0.7,
      label = labels,
      color = ~factpal(General_Party)
    ) %>%
    addLegend(pal = factpal, values = ~General_Party, opacity = 1)
}