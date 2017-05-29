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
out <- out.pres


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
  
  
  #######################################MAPPING EVENTS#############################################
  
  observeEvent(input$election, {
    if(input$election == 'Loading...') {
      updateSelectInput(session, "election", choices = c("Presidential", "Senate", "House of Representatives"))
      out <- out.pres
    } else if(input$election == "Senate") {
      out <- out.senate
    } else if(input$election == 'House of Representatives') {
      out <- out.house
    } else {
      out <- out.pres
    }
    
    updateSelectInput(session, "year", choices = unique(out$Election_Year))
    updateSelectInput(session, "party", choices = unique(out$General_Party))
    
    
    if(input$year != "Loading..." && input$party != "Loading...") {
      out.filtered <- out %>% filter_(paste0('Election_Year == "', input$year,'"'),paste0('General_Party == "', input$party,'"'))
      updateSelectizeInput(session, "candidate", choices = unique(out.filtered$Candidate), server = TRUE)
    }
  })
  
  observeEvent(input$year, {
    if(input$year != 'Loading...' && input$party != "Loading...") {
      out.filtered <- out %>% filter_(paste0('Election_Year == "', input$year,'"'),paste0('General_Party == "', input$party,'"'))
      updateSelectizeInput(session, "candidate", choices = unique(out.filtered$Candidate), server = TRUE)
    }
  })
  
  observeEvent(input$party, {
    if(input$year != 'Loading...' && input$party != "Loading...") {
      out.filtered <- out %>% filter_(paste0('Election_Year == "', input$year,'"'),paste0('General_Party == "', input$party,'"'))
      updateSelectizeInput(session, "candidate", choices = unique(out.filtered$Candidate), server = TRUE)
    }
    print(input$candidate)
  })
  
})