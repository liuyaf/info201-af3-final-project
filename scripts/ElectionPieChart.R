library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)

BuildPieChart <- function(house.results.temp, min, max) {
  #filter the given data to abide by the slider data 
  house.filtered <- house.results.temp %>% filter(Total_. > min & Total_. < max)
  
  #Filter data to only show wins and loses, not pending or other info
  final.house.data <- house.filtered %>% filter(Status_of_Candidate == "Won" | Status_of_Candidate == "Lost" )
  #count total members, total winners, and total losers to calculate the percentages
  total.rows <- sum(complete.cases(final.house.data))
  num.winners <- sum(final.house.data$Status_of_Candidate == "Won")
  num.losers <- sum(final.house.data$Status_of_Candidate == "Lost")
  perc.lost <- num.losers / total.rows
  perc.won <- num.winners / total.rows

  #create data frame to be used in the chart creation
  data.graph <- data.frame("Won/Lost" = c("Won", "Lost"), "Results" = c(num.winners, num.losers), "Percentages" = c(perc.won, perc.lost))
  percent <- num.winners / total.rows

  
   pie.results <- plot_ly(data.graph, labels = ~Won.Lost, values = ~Percentages, type = 'pie',
                         hoverinfo = 'text',
                         text = ~paste0("Won/Lost = ", Won.Lost, " ,</br>", " Percent = ", round(Percentages, digits = 2)*100, "%,</br>",
                                        "Number of Politicians = ", Results)) %>%
     layout(width = 750, height = 750)

  #Create pie chart that displays the percentage that won the election and lost based on money raised, including a hover text option
   pie.results <- plot_ly(data.graph, labels = ~Won.Lost, values = ~Percentages, type = 'pie', width = 300, height = 300,
                         hoverinfo = 'text', 
                         text = ~paste0("Won/Lost = ", Won.Lost, " ,", " Percent = ", round(Percentages, digits = 2)*100, "%" )) %>%
     layout(title = 'Politicians Election Results Based On Money Raised',
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
  #return the pie chart
  return(pie.results)

}
