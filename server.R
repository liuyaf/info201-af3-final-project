# Group AF3
# Info 201 Section AF
# Final Project

library(tools)
library(plotly)
library(shiny)



out.senate <- as.data.frame(read.csv("./data/mapping/Senate_City.csv", stringsAsFactors = FALSE))
out.house <- as.data.frame(read.csv("./data/mapping/House_City.csv", stringsAsFactors = FALSE))
out.pres <- as.data.frame(read.csv("./data/mapping/Pres_City.csv", stringsAsFactors = FALSE))
city.locations <- as.data.frame(read.csv("./data/mapping/city_locations.csv", stringsAsFactors = FALSE))


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
    return (sliderInput("moneyRange", label = h3("Adjust Slider to Select Money Range"), min = 0, max = 80122596, value = c(0, 1000000)))
   } else {
    return (sliderInput("moneyRange", label = h3("Adjust Slider to Select Money Range"), min = 0, max = 34104655, value = c(0, 1000000)))
    }
  })
  
  source("./scripts/ElectionPieChart.R")
  
  output$electionChart <- renderPlotly({
    if (input$govBranch == "Senate") {
      legislature.branch <- read.csv("data/SenateElection.csv", stringsAsFactors=FALSE)
    } else {
      legislature.branch <- read.csv("data/HouseElection.csv", stringsAsFactors=FALSE)
    }
    wanted.data.from.branch <- legislature.branch[,c(13,16,36)]
    return(BuildPieChart(wanted.data.from.branch, input$moneyRange[1], input$moneyRange[2]))
  })
  
  #######################################MAPPING EVENTS#############################################
  
  data.full <- reactive({
    if(input$election != 'Loading...'){
      if(input$election == 'Presidential') {
        return(out.pres)
      } else if(input$election == 'Senate'){
        return(out.senate)
      } else if(input$election == 'House of Representatives') {
        return(out.house)  
      }
    } else {
      ###Return Loading Data Frame
      loading <- data.frame(c("Loading..."), c("Loading..."), c("Loading..."))
      colnames(loading) <- c("Election_Year", "General_Party", "Candidate")
      return(loading)
    }
  })
  
  data.selected <- reactive({
    out.filtered <- data.full() %>% filter_(paste0('Election_Year == "', input$year,'"'), paste0('General_Party == "', input$party,'"'))
    return(out.filtered)
  })
  
  observeEvent(input$election, {
    if(input$election == 'Loading...') {
      updateSelectInput(session, "election", choices = c("Presidential", "Senate", "House of Representatives"), selected = "Presidential")
    }
    updateSelectInput(session, "year", choices = unique(data.full()$Election_Year))
    updateSelectInput(session, "party", choices = unique(data.full()$General_Party))
    updateSelectizeInput(session, "candidate", choices = unique(data.selected()$Candidate), server = TRUE)
  })
  
  observeEvent(input$year, {
    if(input$election != 'Loading...') {
      updateSelectizeInput(session, "candidate", choices = unique(data.selected()$Candidate), server = TRUE)
    }
  })
  
  observeEvent(input$party, {
    if(input$election != 'Loading...') {
      updateSelectizeInput(session, "candidate", choices = unique(data.selected()$Candidate), server = TRUE)
    }
  })
  
})