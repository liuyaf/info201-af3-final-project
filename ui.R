# Matthew Li
# Info 201 Section AF
# Assignment 8
# ui.R file for building user interface appearance for data visualization of 
# the Cereal data.

library(shiny)
library(plotly)
library(httr)

shinyUI(navbarPage("Political Bidding",
                   # Tab Panel will show a grouped representation of the cereals based on input from
                   # the user via the 2 selectInput widgets, graph's marker size is based on frequency
                   tabPanel("2016 Election",
                            
                            sidebarPanel(
                              selectInput("election", "Election Type:", c("Loading...")),
                              selectInput("year", "Year:", c("Loading...")),
                              selectInput("party", "General Party:", c("Loading...")),
                              selectizeInput(
                                'candidate', 'Candidate', choices = c("Loading..."),
                                multiple = FALSE
                              )
                            ),
                            
                            mainPanel(
                              leafletOutput("map")
                            )
                            # end mainPanel
                   ), #end tabPanel 1
                   
                   # Tab Panel shows the raw data that the plots are being formed by.
                   tabPanel("Contribution to Candidate",
                            
                            # titlepanel
                            titlePanel('Top 100 Contribution to Specific Candidate'),
                            
                            # creates sidebar to input candidate's name
                            sidebarLayout(
                              sidebarPanel(
                                
                                # Input box to collect candidate's name
                                textInput("canname", label = h3("Candidate Name (Defualt MURRAY, PATTY"), value = "MURRAY, PATTY"),
                                
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
                              # Radio buttons for either the user to see if they want to see data from the House or the Senate
                              radioButtons("govBranch", label = h3("Select which Branch of Legislation you want to see:"), 
                                           choices = c("House of Representatives", "Senate"), selected = "House of Representatives"),
                              uiOutput("ui"),
                              #The slider for the user to select the range of money that they want to see
                              #sliderInput("moneyRange", label = h3("Adjust Slider to Select Money Range"), min = 0, max = 80122596, value = c(0, 1000000)),
                              width = 12
                            ),
                            
                            mainPanel(
                              plotlyOutput("electionChart")
                            )
                            
                   ) # end tabPanel
                   
)) #end shinyUI