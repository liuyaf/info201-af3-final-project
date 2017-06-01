# Matthew Li
# Info 201 Section AF
# Assignment 8
# ui.R file for building user interface appearance for data visualization of 
# FollowTheMoney.org datasets. Produces various tabs and their contents within.

library(leaflet)
library(shiny)
library(plotly)
library(httr)
library(leaflet)
library(jsonlite)

# read the candidate name for selectizeInput
candidate.barchart <- read.csv('./data/Candidate_2016.csv', stringsAsFactors = FALSE)

shinyUI(navbarPage("Political Bidding", 
                   # Tab Panel will show a user interface for producing 
                   tabPanel("Money Mapping", 
                            
                            # Sidebar Panel holds the user selection inputs. Initial values of 
                            # the widgets are set to "Loading..." and updated by server.R once 
                            # csv files are completely read.
                            sidebarPanel(
                              selectInput("election", "Election Type:", c("Loading...")),
                              selectInput("year", "Year:", c("Loading...")),
                              selectInput("party", "General Party:", c("Loading...")),
                              selectizeInput(
                                'candidate', 'Candidate', choices = c("Loading..."),
                                multiple = FALSE
                              ),
                              actionButton("update", "Update Plot"),
                              actionButton("new", "Create New Plot"),
                              width = 4
                            ),
                            # MainPanel contains uiOutput, uiOutput "goTab" is rendered by the server
                            # and produces the tab output.
                            mainPanel(
                              uiOutput("goTab")
                            ) # end fluidRow 
                      ),
                   
                   # Tab Panel shows the raw data that the plots are being formed by.
                   tabPanel("Contributions to Candidates",
                            
                            # titlepanel
                            titlePanel('Top 100 Contribution to Specific Candidate'),
                            
                            # creates sidebar to input candidate's name
                            sidebarLayout(
                              sidebarPanel(
                                
                                # selectize input to allow user to input candidate's name
                                selectizeInput(
                                  'canname', label = h3('Candidate Name'), choices = candidate.barchart$Candidate
                                ),

                                
                                # radio butttons that can group barchart by industry
                                radioButtons("colorvar", label = h3("Group Barchart by Industry"),
                                             choices = list("Yes" = '~industry', "No" = 'NULL'), 
                                             selected = 'NULL'),
                                actionButton('update.can', 'Submit')
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
                   tabPanel("Monetary Influence",
                            
                            tabsetPanel(
                              tabPanel("tab1",
                                titlePanel("Candidate Winning Percentage Based Off Spending"),
                                
                                sidebarPanel(
                                  textOutput("description"),
                                  # Radio buttons for either the user to see if they want to see data from the House or the Senate
                                  radioButtons("govBranch", label = h3("Select which Legislative Branch you want to see:"), 
                                               choices = c("House of Representatives", "Senate"), selected = "House of Representatives"),
                                  sliderInput("moneyRange", label = h3("Adjust Slider to Select Money Range"), min = 0, max = 34104655, value = c(0, 1000000))
                                  #The slider for the user to select the range of money that they want to see
                                ),
                                
                                
                                mainPanel(
                                  plotlyOutput("electionChart")
                                )
                              ),
                              tabPanel("tab2",
                                sidebarPanel(
                                  selectInput("election2", "Election Type:", c("Loading...")),
                                  selectInput("state", "Election Jurisdiction:", c("Loading...")),
                                  selectInput("year2", "Year:", c("Loading..."))
                                ),
                                mainPanel(
                                  plotlyOutput("winPlot", width = "100%", height = "100%")
                                )
                              )
                            )
                            
                   ), # end tabPanel
                   
                   tabPanel("About",
                            titlePanel("About"),
                            mainPanel(
                              uiOutput("about"), width = 12
                            )
                    )
)) #end shinyUI