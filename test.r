
library(plotly) 
library(dplyr)

data.senate <- read.csv("./data/SenateElection.csv")
data.filtered <- data.senate %>% select(Candidate, Status_of_Candidate, General_Party, Election_Jurisdiction, Election_Year, Incumbency_Status, Total_.)
data.filtered <- data.filtered %>% filter(Election_Jurisdiction == "CA", Election_Year == "2012")

data.filtered$size <- ntile(data.filtered$Total_., 5)

# Function should be passed a dataframe with the following columns: General_Party, Candidate, 
# Total_., Incumbency_Status

BuildElectionResult <- function(data.filtered) {
  
  col3 <- c(`Third-Party` = 'green', `Republican` = "red", `Democratic` = "blue")
  sy <- c(`Won` = 11, `Withdrew` = 15, `Lost` = 4)
    
  p <- plot_ly(data = data.filtered, y = ~Total_., x = ~Candidate, type = 'scatter',
              color = ~General_Party, colors = col3,
              symbol = ~Status_of_Candidate, symbols = sy, 
              marker = list(size = 12), 
              hoverinfo = 'text',
              text = ~paste0(Candidate, '</br>',
                            'Status: ', Incumbency_Status, '</br>',
                            'Contributions: $', Total_., '</br>',
                            'Party: ', General_Party)) %>%
      layout(margin = list(b = 160, l = 115), xaxis = list(tickangle = 45, title = "Candidate"), 
             yaxis = list(title = "Total Recieved Contributions"))
  return(p)
}
