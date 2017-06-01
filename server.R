# Group AF3
# Info 201 Section AF
# Server.R file which contains the event handlers for various
# inputs to manipulate the data and the UI's appearance. 

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
source('./scripts/WinPlotMaker.R')

# read csv file and store global variable
out.senate <- as.data.frame(read.csv("./data/mapping/Senate_City.csv", stringsAsFactors = FALSE))
out.house <- as.data.frame(read.csv("./data/mapping/House_City.csv", stringsAsFactors = FALSE))
out.pres <- as.data.frame(read.csv("./data/mapping/Pres_City.csv", stringsAsFactors = FALSE))
city.locations <- as.data.frame(read.csv("./data/mapping/city_location.csv", stringsAsFactors = FALSE))
city.locations <- city.locations %>% group_by(State, City, County) %>% summarise(lat = min(Latitude), lon = min(Longitude))



pres.gen <- as.data.frame(read.csv("./data/PresidentialElection.csv"))
senate.gen <- as.data.frame(read.csv("./data/SenateElection.csv"))
house.gen <- as.data.frame(read.csv("./data/HouseElection.csv"))
  
  
n_tabs <- 0
max_plots <- 10



# Build shinyServer
shinyServer(function(input, output, session) {
  
  # Washington Candidates contribution part (Liuyang Fu)
  # the observeEvent listens the submit button and updates the name
  # of candidate 
  # it reads input name and calls GetContributor function and 
  # call 3 funtiosn with dataframe returned by GetContributor
  observeEvent(input$update.can, {
    
    # Isolation required since triggered by various selectInputs which are referenced. Isolation
    # eliminates unneccessary calling.
    isolate({
      canname <- input$canname

      con.to.candidate <- GetContributor(canname)

      # Renders the bar plot
      output$bar.con <- renderPlotly({
        return(BuildBarchart(con.to.candidate, input$colorvar, canname))
      })
      
      # Renders the map.
      output$map.con <- renderPlotly({
        return(BuildMap(con.to.candidate))
      })
      
      # Renders the pie chart
      output$pie.con <- renderPlotly({
        industry.candidate <- con.to.candidate %>% group_by(industry) %>% summarise(total = sum(total))
        industry.candidate <- industry.candidate %>% mutate(name = industry, percent = total / sum(total)) 
        return(BuildPie(industry.candidate))
      })
    })
  })
  
 
  # Checks which branch the user wants by seeing what radio value is clicked on, and then adjusts the max values of the sliders
  output$ui <- renderUI({
    if (input$govBranch == "Senate") {
      return (sliderInput("moneyRange", label = h3("Adjust Slider to Select Money Range"), min = 0, max = 80122596.41, value = c(0, 1000000)))
    } else if(input$govBranch == "House of Representatives"){
      return (sliderInput("moneyRange", label = h3("Adjust Slider to Select Money Range"), min = 0, max = 36104655.12, value = c(0, 1000000)))
    } else {
      return (sliderInput("moneyRange", label = h3("Adjust Slider to Select Money Range"), min = 0, max = 818383160.31, value = c(0, 1000000)))
    }
  })
  
  # Sources in the script to build Pie chart
  source("./scripts/ElectionPieChart.R")
  
  # Creates text within the side panel to describe what this tab does
 
  
  # Checks which gov branch the user wants and then builds the pie chart within all the specified input values
  output$electionChart <- renderPlotly({
    if (input$govBranch == "Senate") {
      legislature.branch <- senate.gen
    } else if(input$govBranch == "House of Representatives") {
      legislature.branch <- house.gen
    } else {
      legislature.branch <- pres.gen
    }
    wanted.data.from.branch <- legislature.branch[,c(13,36)]
    colnames(wanted.data.from.branch)[2] <- "total"
    return(BuildPieChart(wanted.data.from.branch, input$moneyRange[1], input$moneyRange[2]))
  })
  
  #Adds the text to the about tab using HTML tags
  output$about <- renderUI({
    HTML("<strong>Matthew Li, Liuyang Fu, Nikhil Goel, Dean Barlan</strong></br></br>
         <p>For our project, we decided to use a combination of API calls and CSV files, to produce visual representations of the
          data.  All data was sourced from FollowTheMoney.org.
         FollowTheMoney.org is a nonprofit site whose purpose is to <q><i>promotes an accountable democracy by compiling comprehensive 
         campaign-donor, lobbyist, and other information from the government disclosure agencies nationwide.</i></q>  Their data centers 
          around the flow of over 50 billion dollars through the US government to many endpoints like our politicians.</p>
         <p>With this expansive amount of data, we decided to focus around three target questions, which we divided up among the three
          tabs:</p>
         <ol>
          <li><strong>Who is funding each politician?</strong> Under the 'Contributions to Candidates' tab, we allow a user to query the FollowTheMoney.org
          database which returns the top 100 contributors for the particular politician. Using the returned data from the API, we construct visual representations
          of the data to display the top contributors by their industry, name, and other qualities.</li>
          <li><strong>Where are Politicians recieving funds Geographically?</strong> For this section, we wrangled over 100 million rows of data. To reduce
          load time, we prewrangled the data and exported it to a CSV file. The data stored includes the total contributions from each city to each political candidate.
          Using this data, we can map the total amounts onto a map from which we can then explore.</li>
          <li><strong>Does Money really influence Politics?</strong> More money is flooding system currently than ever before and so we would like to see if that helps
          their chances of winning. Explored in the final tab, we explore how varying ranges of contributions changes the win/loss ratio. Additionally,
          in the sub-tab of this section, one can explore the outcomes of specific elections based on contributions.</li>
         </ol>
         <p>We created this app with the average American citizen as our target audience in hopes of allowing them to feel more involved in our political
         system. By breaking down, cleaning, and translating the data into a more readable format, people will be able to see clearly the data and 
         draw their own insights and conclusions.</p>
         <p>You may view our code for this project at our github repository <a href='https://github.com/liuyaf/info201-af3-final-project'>here</a></p>")
  })
#######################################Contribution Scatter Plot#################################
  
# # # # # # # # # # # # # # # # # # # Matthew Li, AF3 # # # # # # # # # # # # # # # # # # # # # #
  
  # Reactive data value. Provides all served from either Presidential, Senate, or House of representative data
  # frames stored in the global variable fields. 
  # Determines if the csv files are completely loaded, if not, loading dataframe is produced and returned.
  data.type <- reactive({
    if(input$election2 != 'Loading...') {
      if(input$election2 == 'Presidential') {
        return(pres.gen)
      } else if(input$election2 == 'Senate') {
        return(senate.gen)
      } else {
        return(house.gen)
      }
    } else {
      # Produce a loading data frame for other input fields to be filled.
      loading <- data.frame(c("Loading..."), c("Loading..."), c("Loading..."), c("Loading..."), c("Loading..."))
      colnames(loading) <- c("Election_Year", "Election_Jurisdiction", "Candidate", "State", "City")
      return(loading)
    }
  })
  
  # Reactive data value. Provides a filtered version of the data.type value which only contains the
  # years avaliable given a specific state.
  year <- reactive({
    out.filtered <- data.type()
    out.filtered <- out.filtered %>% filter_(paste0('Election_Jurisdiction == "', input$state, '"'))
    return(out.filtered)
  })
  
  # ObserveEvent for select user input for election. Initially set to loading. When csv files are completely filled,
  # input field is repopulated with optiuons for Presidential, Senate, and House of Representatives. On observed event,
  # updates the fields for the year, party, and candidiate inputs.
  observeEvent(input$election2, {
    if(input$election2 == 'Loading...') {
      updateSelectInput(session, "election2", choices = c("Presidential", "Senate", "House of Representatives"), selected = "Presidential")
    }
    updateSelectInput(session, "state", choices = unique(data.type()$Election_Jurisdiction))
    updateSelectInput(session, "year2", choices = unique(year()$Election_Year))
  })
  
  # ObserveEvent for select user input for selectInput for year. Changes the selectInput's choices when 
  # a change in the year variable occurs.
  observeEvent(year(), {
    updateSelectInput(session, "year2", choices = c(unique(year()$Election_Year)))
  })
  
  # Renders the plot. Utilizes the BuildElectionResult function located in the WinPlotMaker.R file.
  output$winPlot <- renderPlotly({
    if(input$election2 != 'Loading...') {
      data.out <- data.type()
      if(input$election2 != 'Presidential') {
        data.out <- data.out %>% filter_(paste0('Election_Jurisdiction == "', input$state, '"'))
      }
      data.out <- data.out %>% filter_(paste0('Election_Year == "', input$year2,'"'))
      BuildElectionResult(data.out)
    }
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