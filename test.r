
library(plotly) 
library(dplyr)

data.senate <- read.csv("./data/SenateElection.csv")
data.filtered <- data.senate %>% select(Candidate, Status_of_Candidate, General_Party, Election_Jurisdiction, Election_Year, Incumbency_Status, Total_.)
data.filtered <- data.filtered %>% filter(Election_Jurisdiction == "CA", Election_Year == "2012")

data.filtered$size <- ntile(data.filtered$Total_., 5)


p <- plot_ly(data = data.filtered, y = ~Status_of_Candidate, x = ~Candidate, type = 'scatter',
             color = ~General_Party, size = ~Total_.) %>%
  layout(margin = list(b = 160), xaxis = list(tickangle = 45))
