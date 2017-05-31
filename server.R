# Group AF3
# Info 201 Section AF
# Final Project

library(tools)
library(plotly)
library(shiny)
library(dplyr)
library(leaflet)
library(httr)
library(jsonlite)


max_plots <- 5
source("./scripts/scatterMapBuilder.r")
source('./scripts/readFile.R')
source('./scripts/mixPlot.r')
out.senate <- as.data.frame(read.csv("./data/mapping/Senate_City.csv", stringsAsFactors = FALSE))
out.house <- as.data.frame(read.csv("./data/mapping/House_City.csv", stringsAsFactors = FALSE))
out.pres <- as.data.frame(read.csv("./data/mapping/Pres_City.csv", stringsAsFactors = FALSE))
city.locations <- as.data.frame(read.csv("./data/mapping/city_location.csv", stringsAsFactors = FALSE))
city.locations <- city.locations %>% group_by(State, City, County) %>% summarise(lat = min(Latitude), lon = min(Longitude))
plots_made <- 0


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
  # Important! : creationPool should be hidden to avoid elements flashing before they are moved.
  #              But hidden elements are ignored by shiny, unless this option below is set.
  output$creationPool <- renderUI({})
  outputOptions(output, "creationPool", suspendWhenHidden = FALSE)
  # End Important
  
  # Important! : This is the make-easy wrapper for adding new tabPanels.
  addTabToTabset <- function(Panels, tabsetName){
    titles <- lapply(Panels, function(Panel){return(Panel$attribs$title)})
    Panels <- lapply(Panels, function(Panel){Panel$attribs$title <- NULL; return(Panel)})
    
    output$creationPool <- renderUI({Panels})
    session$sendCustomMessage(type = "addTabToTabset", message = list(titles = titles, tabsetName = tabsetName))
  }
  # End Important
  
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
      loading <- data.frame(c("Loading..."), c("Loading..."), c("Loading..."), c("Loading..."), c("Loading..."))
      colnames(loading) <- c("Election_Year", "General_Party", "Candidate", "State", "City")
      return(loading)
    }
  })
  
  candidates <- reactive({
    out.filtered <- data.full()
    if(input$year != "All"){
      out.filtered <- out.filtered %>% filter_(paste0('Election_Year == "', input$year,'"'))
    }
    if(input$party != "All"){
      out.filtered <- out.filtered %>% filter_(paste0('General_Party == "', input$party,'"'))
    }
    return(out.filtered)
  })
  
  observeEvent(input$election, {
    if(input$election == 'Loading...') {
      updateSelectInput(session, "election", choices = c("Presidential", "Senate", "House of Representatives"), selected = "Presidential")
    }
    updateSelectInput(session, "year", choices = c("All", unique(data.full()$Election_Year)))
    updateSelectInput(session, "party", choices = c("All", unique(data.full()$General_Party)))
    updateSelectizeInput(session, "candidate", choices = c("All", unique(candidates()$Candidate)), server = TRUE)
  })
  
  observeEvent(input$year, {
    if(input$election != 'Loading...') {
      updateSelectizeInput(session, "candidate", choices = c("All", unique(candidates()$Candidate)), server = TRUE)
    }
  })
  
  observeEvent(input$party, {
    if(input$election != 'Loading...') {
      updateSelectizeInput(session, "candidate", choices = c("All", unique(candidates()$Candidate)), server = TRUE)
    }
  })
  
  output$plots <- renderUI({
    plot_output_list <- lapply(1:input$n, function(i) {
      plotname <- paste("plot", i, sep="")
      leafletOutput(plotname)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  
  output$goTab <- renderUI({
    tab_output_list <- lapply(1:input$n, function(i){
      tabname <- paste("Map ", i, sep="")
      tabPanel(tabname,
               h2("New Plot"),
               leafletOutput(paste0("plot",i)),
               hr(),
               h2("Raw Data"),
               dataTableOutput(paste0("table",i))) 
    })
    do.call(tabsetPanel, tab_output_list)
  })
  
  
  output$tabs <- renderUI({
    tab_output_list <- lapply(1:input$n, function(i){
      tabname <- paste("tab", i, sep="")
      tabPanel(tabname, 
          dataTableOutput(paste0("table",i))) 
    })
    do.call(tabsetPanel, tab_output_list)
  })
  
  # to store observers and make sure only once is created per button
  
  obsList <- list()
  
  output$go_buttons <- renderUI({
    buttons <- as.list(1:input$n)
    buttons <- lapply(buttons, function(i) {
      btName <- paste0("btn",i)
      # creates an observer only if it doesn't already exists
      
      if (is.null(obsList[[btName]])) {
        # make sure to use <<- to update global variable obsList
        obsList[[btName]] <<- observeEvent(input[[btName]], {
            output[[paste0("plot",i)]] <- renderLeaflet({
              isolate({
                candidate.filtered <- candidates()
                
                if(input$candidate != "" && input$candidate != "All") {
                  candidate.filtered <- candidate.filtered %>% filter_(paste0('Candidate == "', input$candidate,'"'))
                }
                out.map <- candidate.filtered %>% group_by(Candidate, General_Party, State, City) %>% summarise(Amount = sum(Amount))
                out.map <- left_join(out.map, city.locations)
                
                if(input$election != "Loading...") {
                  BuildScatterMap(out.map)
                } else {
                  leaflet() %>% addTiles() %>%
                    setView(-96, 37.8, 4) %>%
                    addProviderTiles(providers$CartoDB.DarkMatter) 
                }
              })#end isolate
          }) #end renderLeaflet
          output[[paste0("table",i)]] <- renderDataTable({
            isolate({
              candidate.filtered <- candidates()
              
              if(input$candidate != "" && input$candidate != "All") {
                candidate.filtered <- candidate.filtered %>% filter_(paste0('Candidate == "', input$candidate,'"'))
              }
              out.map <- candidate.filtered %>% group_by(Candidate, General_Party, State, City) %>% summarise(Amount = sum(Amount))
              return(out.map)
            })
          })
        }) #end observeEvent
      }
      actionButton(btName,paste("Update Plot ",i))
    })
  })
  
  # render initial map plots
  for (i in 1:max_plots) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep="")
      tabname <- paste("table", my_i, sep="")
      
      output[[tabname]] <- renderDataTable(iris)
      
      output[[plotname]] <- renderLeaflet({
          leaflet() %>% addTiles() %>%
            setView(-96, 37.8, 4) %>%
            addProviderTiles(providers$CartoDB.DarkMatter) 
      })
    })
  }
  
})