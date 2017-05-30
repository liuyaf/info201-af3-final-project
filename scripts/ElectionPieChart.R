library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)

BuildPieChart <- function(house.results.temp, min, max) {
  house.filtered <- house.results.temp %>% filter(Total_. > min & Total_. < max)

  final.house.data <- house.filtered %>% filter(Status_of_Candidate == "Won" | Status_of_Candidate == "Lost" )
  total.rows <- sum(complete.cases(final.house.data))
  num.winners <- sum(final.house.data$Status_of_Candidate == "Won")
  num.losers <- sum(final.house.data$Status_of_Candidate == "Lost")
  perc.lost <- num.losers / total.rows
  perc.won <- num.winners / total.rows

  data.graph <- data.frame("Won/Lost" = c("Won", "Lost"), "Results" = c(num.winners, num.losers), "Percentages" = c(perc.won, perc.lost))
  percent <- num.winners / total.rows

   pie.results <- plot_ly(data.graph, labels = ~Won.Lost, values = ~Percentages, type = 'pie', width = 300, height = 300,
                         hoverinfo = 'text', 
                         text = ~paste0("Won/Lost = ", Won.Lost, " ,", " Percent = ", round(Percentages, digits = 2)*100, "%" )) %>%
     layout(title = 'Politicians Election Results Based On Money Raised',
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
  
  return(pie.results)

}
