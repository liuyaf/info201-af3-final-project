# Matthew Li
# WinPlotMaker.R
# Contains a function for creating a plotly scatter plot when passed in a specific dataframe.

library(plotly) 
library(dplyr)

# Produces a plotly scatter plot that has a x-axis containing each candidate, y-axis for total 
# contributions recieved, and specific symbols for win/loss.
BuildElectionResult <- function(data.filtered) {
  
  # Color pallet.
  col3 <- c(`Third-Party` = 'green', `Republican` = "red", `Democratic` = "blue")
  # Symbol pallet.
  sy <- c(`Won` = 11, `Withdrew` = 15, `Lost` = 4)
    
  # Creation of plotly output.
  p <- plot_ly(data = data.filtered, y = ~Total_., x = ~Candidate, type = 'scatter',
              color = ~General_Party, colors = col3,
              symbol = ~Status_of_Candidate, symbols = sy, 
              marker = list(size = 12), 
              hoverinfo = 'text',
              text = ~paste0(Candidate, '</br>',
                            'Status: ', Incumbency_Status, '</br>',
                            'Contributions: $', Total_., '</br>',
                            'Party: ', General_Party)) %>%
      layout(margin = list(b = 160, l = 120), xaxis = list(tickangle = 45, title = "Candidate"), 
             yaxis = list(title = "Total Recieved Contributions"))
  return(p)
}
