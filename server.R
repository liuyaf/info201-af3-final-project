# Group AF3
# Info 201 Section AF
# Final Project

library(tools)
library(plotly)
library(shiny)


# Build shinyServer
shinyServer(function(input, output, session) {
  
  output$bar.con <- renderPlotly({
    con.to.candidate <- GetContributor(input$canname)
    return(BuildBarchart(con.to.candidate, input$colorvar))
  })
  
  output$map.con <- renderPlotly({
    con.to.candidate <- GetContributor(input$canname)
    return(BuildMap(con.to.candidate))
  })
  
  output$pie.con <- renderPlotly({
    industry.candidate <- GetIndustryPercent(input$canname)
    return(BuildPie(industry.candidate))
  })
  
  output$ui <- renderUI({
   if (input$govBranch == "Senate") {
     return (sliderInput("moneyRange", label = h3("Adjust Slider to Select Money Range"), min = 0, max = 30122024, value = c(0, 1000000)))
   } else {
     return (sliderInput("moneyRange", label = h3("Adjust Slider to Select Money Range"), min = 0, max = 80122596, value = c(0, 1000000)))
   }
  })
})