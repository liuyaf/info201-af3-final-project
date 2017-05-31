library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)

BuildPieChart <- function(gov.branch, min, max) {
  gov.branch.filtered <- gov.branch %>% filter(Total_. > min) %>% filter(Total_. < max)

  final.branch.data <- gov.branch.filtered %>% filter(Status_of_Candidate == "Won" || Status_of_Candidate == "Lost" )
  total.rows <- sum(complete.cases(final.branch.data))
  num.winners <- sum(final.branch.data$Status_of_Candidate == "Won")
  num.losers <- sum(final.branch.data$Status_of_Candidate == "Lost")
  perc.lost <- num.losers / total.rows
  perc.won <- num.winners / total.rows

  data.graph <- data.frame("Won/Lost" = c("Won", "Lost"), "Results" = c(num.winners, num.losers), "Percentages" = c(perc.won, perc.lost))
  percent <- num.winners / total.rows

  
   pie.results <- plot_ly(data.graph, labels = ~Won.Lost, values = ~Percentages, type = 'pie',
                         hoverinfo = 'text',
                         text = ~paste0("Won/Lost = ", Won.Lost, " ,</br>", " Percent = ", round(Percentages, digits = 2)*100, "%,</br>",
                                        "Number of Politicians = ", Results)) %>%
     layout(width = 750, height = 750,
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
  
  return(pie.results)

}
