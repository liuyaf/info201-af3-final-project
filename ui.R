# Matthew Li
# Info 201 Section AF
# Assignment 8
# ui.R file for building user interface appearance for data visualization of 
# the Cereal data.
library(leaflet)
library(shiny)
library(plotly)
library(httr)
library(leaflet)
library(jsonlite)

shinyUI(navbarPage("Political Bidding", 
                   # Tab Panel will show a grouped representation of the cereals based on input from
                   # the user via the 2 selectInput widgets, graph's marker size is based on frequency
                   tabPanel("2016 Election",
                            
                            sidebarPanel(
                              sliderInput("n", "Number of plots", value=1, min=1, max=5),
                              selectInput("election", "Election Type:", c("Loading...")),
                              selectInput("year", "Year:", c("Loading...")),
                              selectInput("party", "General Party:", c("Loading...")),
                              selectizeInput(
                                'candidate', 'Candidate', choices = c("Loading..."),
                                multiple = FALSE
                              ),
                              uiOutput("go_buttons")
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Plot", uiOutput("plots")),
                                uiOutput("tabs")
                              )
                            )
                            # end mainPanel
                   ), #end tabPanel 1
                   
                   # Tab Panel shows the raw data that the plots are being formed by.
                   tabPanel("Contribution to Candidate",
                            
                            # titlepanel
                            titlePanel('Top 100 Contribution to Specific Candidate (WA)'),
                            
                            # creates sidebar to input candidate's name
                            sidebarLayout(
                              sidebarPanel(
                                
                                # Input box to collect candidate's name
                                textInput("canname", label = h3("Candidate Name"), value = "MURRAY, PATTY"),
                                
                                # radio butttons that can group barchart by industry
                                radioButtons("colorvar", label = h3("Color Barchart by Industry"),
                                             choices = list("Yes" = '~industry', "No" = 'NULL'), 
                                             selected = 'NULL')
                              ),
                              
                              # Use plotlyOutput to show the scatter plot
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Barchart", plotlyOutput("bar.con")), 
                                  tabPanel("Map", plotlyOutput("map.con")), 
                                  tabPanel("Piechart", plotlyOutput("pie.con"))
                                )
                                
                              )
                            )
                            
                   ), # end tabPanel
                   tabPanel("Candidate Winning Percentage",
                            titlePanel("Candidate Winning Percentage Based Off Spending"),
                            
                            sidebarPanel(
                              textOutput("description"),
                              # Radio buttons for either the user to see if they want to see data from the House or the Senate
                              radioButtons("govBranch", label = h3("Select which Branch of Legislation you want to see:"), 
                                           choices = c("House of Representatives", "Senate"), selected = "House of Representatives"),
                              uiOutput("ui")
                              #The slider for the user to select the range of money that they want to see
                            ),
                            
                            mainPanel(
                              plotlyOutput("electionChart")
                            )
                            
                   ), # end tabPanel
                   
                   tabPanel("About",
                            titlePanel("About"),
                            mainPanel(
                              textOutput("about"), width = 12
                            )
                    )
                   
                   ###########################Tabset Stuff for Mapping#########################
                   # Important! : JavaScript functionality to add the Tabs
                
                   
                   
                   
)) #end shinyUI