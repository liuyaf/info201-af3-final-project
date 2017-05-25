
library(geojsonio)
library(leaflet)
library(ggmap)
library(dplyr)
library(sp)


df <- as.data.frame(read.csv("../data/WashingtonSenateContributions.csv", stringsAsFactors = FALSE))

map <- df %>% group_by(State) %>% summarize(total = sum(Amount))

state.abb2 <- c(state.abb, "DC")
map <- map[ map$State %in% state.abb2, ]
state.name2 <- c(state.name, "District of Columbia")
map$State <- state.name2[match(map$State, state.abb2)]
map <- select(map, State, total)

value.max <- map %>% filter_(paste0("total",' == max(',"total",')')) %>% select_("total")
value.max <- max(map$total)
bins <- seq(0, value.max,length=5)

colnames(map) <- c("name","total")
BuildUSMap(map, "total",map.label = "<strong>%s</strong><br/>$%g in Contributions")

# Builds a US Choropleth Map at the NATIONAL (values by state) level. 
# dataframe name, total
BuildUSMap <- function(df, values = "total", map.label = "<strong>%s</strong><br/>$%g in Contributions") {
  # Creates a spatial polygon dataframe --> for mapping
  states <- geojson_read("../json/us-states.json", what = "sp")
  
  # Get max value of the passed in dataframe
  value.max <- max(df$total)
  
  # Merge state polygon with state values.
  state.data <- sp::merge(states, df, by = "name")
  
  # Create "bins" legend intervals
  bins <- seq(0, value.max,length=8)
  
  # Create colors
  pal <- colorBin("Greens", domain = state.data$total, bins = bins)
  
  # Create Labels
  labels <- sprintf(
    map.label,
    state.data$name, state.data$total
  ) %>% lapply(htmltools::HTML)  
  
  leaflet(state.data) %>%
    setView(-96, 37.8, 4) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      fillColor = ~pal(total),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
    addLegend(pal = pal, values = ~total, opacity = 0.7, title = NULL,
              position = "bottomright") %>%
    return()
}