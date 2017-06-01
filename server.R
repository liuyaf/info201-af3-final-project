# Group AF3
# Info 201 Section AF
# Final Project


# load libraries
library(tools)
library(plotly)
library(shiny)
library(dplyr)
library(leaflet)
library(httr)
library(jsonlite)


# source function file

source("./scripts/scatterMapBuilder.r")
source('./scripts/readFile.R')
source('./scripts/mixPlot.r')

# read csv file and store global variable
out.senate <- as.data.frame(read.csv("./data/mapping/Senate_City.csv", stringsAsFactors = FALSE))
out.house <- as.data.frame(read.csv("./data/mapping/House_City.csv", stringsAsFactors = FALSE))
out.pres <- as.data.frame(read.csv("./data/mapping/Pres_City.csv", stringsAsFactors = FALSE))
city.locations <- as.data.frame(read.csv("./data/mapping/city_location.csv", stringsAsFactors = FALSE))
city.locations <- city.locations %>% group_by(State, City, County) %>% summarise(lat = min(Latitude), lon = min(Longitude))

n_tabs <- 0
max_plots <- 10



# Build shinyServer
shinyServer(function(input, output, session) {
  
  # Washington Candidates contribution part
  # the observeEvent listens the submit button and updates the name
  # of candidate 
  observeEvent(input$update.can, {
    
    canname <- input$canname
    output$bar.con <- renderPlotly({
      con.to.candidate <- GetContributor(canname)
      return(BuildBarchart(con.to.candidate, input$colorvar))
    })
    
    output$map.con <- renderPlotly({
      con.to.candidate <- GetContributor(canname)
      return(BuildMap(con.to.candidate))
    })
    
    output$pie.con <- renderPlotly({
      industry.candidate <- GetIndustryPercent(canname)
      return(BuildPie(industry.candidate))
    })
  })
  
 
  
  output$ui <- renderUI({
   if (input$govBranch == "Senate") {
    return (sliderInput("moneyRange", label = h3("Adjust Slider to Select Money Range"), min = 0, max = 50210591.79, value = c(0, 1000000)))
   } else {
    return (sliderInput("moneyRange", label = h3("Adjust Slider to Select Money Range"), min = 0, max = 34104655, value = c(0, 1000000)))
    }
  })
  
  source("./scripts/ElectionPieChart.R")
  
  output$description <- renderText({
    "This graph helps to visualize a correlation between politician's spending money and their percentage of either losing or winning that 
    election.  Note that if you scroll to a money range where no pie chart displays, that means we have no data on politicians spending
    that amount for their campaign."
  })
  
  output$electionChart <- renderPlotly({
    if (input$govBranch == "Senate") {
      legislature.branch <- read.csv("data/SenateElection.csv", stringsAsFactors=FALSE)
    } else {
      legislature.branch <- read.csv("data/HouseElection.csv", stringsAsFactors=FALSE)
    }
    wanted.data.from.branch <- legislature.branch[,c(13,36)]
    colnames(wanted.data.from.branch)[2] <- "total"
    return(BuildPieChart(wanted.data.from.branch, input$moneyRange[1], input$moneyRange[2]))
  })
  
  output$about <- renderUI({
    HTML("<strong>Matthew Li, Liuyang Fu, Nikhil Goel, Dean Barlan</strong></br></br>
         <p>For our project, we decided to use a combination of api calls and csv files, both of which came from FollowTheMoney.org.
         FollowTheMoney.org is a nonprofit site whose purpose is to <q><i>promotes an accountable democracy by compiling comprehensive 
         campaign-donor, lobbyist, and other information from the government disclosure agencies nationwide.</i></q>  Their data centers 
          around the flow of over 50 billion dollars through the US government to many endpoints like our politicians.</p>
         <p>With this expansive amount of data, we decided to focus around three target questions, which we divided up among the three
          tabs:</p>
         <ol>
          <li><strong>Who is each politician getting their money from while they are in office?</strong>  For this question, we allow you
          to type in any politician and be able to see using either a bar chart, pie chartthe top 100 contributions for that politician.</li>
          <li><strong>From where is each politician receiving their money from?</strong>  Although this sounds similar to question 1, this question
          focuses more geographically and the location of the donations.  You will be able to see on the map, where each politician is most popular 
          and receive the most donations.</li>
          <li><strong>Is there a correlation between the amount of money a candidate spends campaigning and their chances of winning the election?</strong>
            More money is flooding system currently than ever before and so we would like to see if that helps their chances of winning.  Also with Obama's
          use of Big Data and analytics in his second campaign, we would like to see if other politicians were able to do more with less.</li>
         </ol>
         <p>We created this app with the average American citizen as our target audience in hopes of allowing them to feel more involved in our political
         system.  By breaking down, cleaning, and translating the data into a more readable format, people will be able to see clearly the data and 
         draw their own insights and conclusions.</p>")
  })
  
#######################################MAPPING EVENTS#############################################
# # # # # # # # # # # # # # # # # # # Matthew Li, AF3 # # # # # # # # # # # # # # # # # # # # # #
  
  # Reactive data value. Provides all served from either Presidential, Senate, or House of representative data
  # frames stored in the global variable fields. 
  # Determines if the csv files are completely loaded, if not, loading dataframe is produced and returned.
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
      # Produce a loading data frame for other input fields to be filled.
      loading <- data.frame(c("Loading..."), c("Loading..."), c("Loading..."), c("Loading..."), c("Loading..."))
      colnames(loading) <- c("Election_Year", "General_Party", "Candidate", "State", "City")
      return(loading)
    }
  })
  
  # Reactive data value. Provides the filtered version of data.full() that is filtered. Filters by year and party 
  # then returns the dataframe
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
  
  # ObserveEvent for select user input for election. Initially set to loading. When csv files are completely filled,
  # input field is repopulated with optiuons for Presidential, Senate, and House of Representatives. On observed event,
  # updates the fields for the year, party, and candidiate inputs.
  observeEvent(input$election, {
    if(input$election == 'Loading...') {
      updateSelectInput(session, "election", choices = c("Presidential", "Senate", "House of Representatives"), selected = "Presidential")
    }
    updateSelectInput(session, "year", choices = c("All", unique(data.full()$Election_Year)))
    updateSelectInput(session, "party", choices = c("All", unique(data.full()$General_Party)))
    updateSelectizeInput(session, "candidate", choices = c("All", unique(candidates()$Candidate)), server = TRUE)
  })
  
  # ObserveEvent for select user input for year. Shows years avaliable given the data.full() data. On observed event,
  # updates the fields for the candidate input by recalling the reactive data candidates()
  observeEvent(input$year, {
    if(input$election != 'Loading...') {
      updateSelectizeInput(session, "candidate", choices = c("All", unique(candidates()$Candidate)), server = TRUE)
    }
  })
  
  # ObserveEvent for select user input for election Shows years avaliable given the data.full() data. On observed event,
  # updates the fields for the candidate input by recalling the reactive data candidates()
  observeEvent(input$party, {
    if(input$election != 'Loading...') {
      updateSelectizeInput(session, "candidate", choices = c("All", unique(candidates()$Candidate)), server = TRUE)
    }
  })
  
  # reactive variable for increasing the number of tabs to be shown. Stops at the max_plot value (10)
  num_tabs <- eventReactive(input$new, {
    if(n_tabs < max_plots) {
      n_tabs <<- n_tabs + 1
    }
    return(n_tabs)
  })
  
  # ObserveEvent for opening a new tab. Observes for change in num_tabs() and renders the title text for
  # the new tab. Additionally, opens the new tab so user is shown the new tab on press.
  observeEvent(num_tabs(), {
    updateTabsetPanel(session, "tabs", selected = paste0("Map ", n_tabs))
    output[[paste0("title",n_tabs)]] <- renderText("New Plot")
    output[[paste0("query",n_tabs)]] <- renderText("")
  })
  
  # RenderUI output. Renders the number of tabs that the user produced. The tabs are placed inside a tabsetPanel
  # and each include a h2 title, h4 query description, map output (leaflet), and data output.
  output$goTab <- renderUI({
    tab_output_list <- lapply(1:num_tabs(), function(i){
      tabname <- paste("Map ", i, sep="")
      tabPanel(tabname,
               h2(textOutput(paste0("title",i))),
               h4(textOutput(paste0("query",i))),
               leafletOutput(paste0("plot",i)),
               hr(),
               h2("Raw Data"),
               dataTableOutput(paste0("table",i))) 
    })
    args = c(tab_output_list, list(id = "tabs"))
    do.call(tabsetPanel, args)
  })
  
  # ObserveEvent for the update button. Update button determines which tab is currently in view
  # and updates based on the user's input selected.
  observeEvent(input$update, {
    #Determine tab to update
    tab <- input$tabs
    i <- substring(tab, nchar(tab))
    
    # Update tab title
    output[[paste0("title",i)]] <- renderText({
      isolate({
        return(input$candidate)        
      })
    })
    
    # Update query description
    output[[paste0("query", i)]] <- renderText({
      isolate({
        return(paste0("Election: ", input$election ,", Year: ", input$year, ", Party: ", input$party))
      })
    })
    
    # Update map plot
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
    
    # Update datatable rendered.
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
  })
  
  # For loop required to initially render all output plots and tables. 
  for (i in 1:max_plots) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep="")
      tabname <- paste("table", my_i, sep="")
      
      output[[tabname]] <- renderDataTable(NULL)
      
      output[[plotname]] <- renderLeaflet({
          leaflet() %>% addTiles() %>%
            setView(-96, 37.8, 4) %>%
            addProviderTiles(providers$CartoDB.DarkMatter) 
      })
    })
  }
})