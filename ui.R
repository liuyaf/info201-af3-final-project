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

# read the candidate name for selectizeInput
wa.candidate <- read.csv('./data/2016_WA_Candidates.csv')

shinyUI(navbarPage("Political Bidding", 
                   # Tab Panel will show a user interface for producing 
                   tabPanel("2016 Election", 
                            
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
                   tabPanel("Contribution to Candidate",
                            
                            # titlepanel
                            titlePanel('Top 100 Contribution to Specific Candidate (WA)'),
                            
                            # creates sidebar to input candidate's name
                            sidebarLayout(
                              sidebarPanel(
                                
                                # Input box to collect candidate's name
                                selectizeInput(
                                  'canname', label = h3('WA Candidate Name'), choices = wa.candidate$Candidate
                                ),
                                # textInput("canname", label = h3("Candidate Name"), value = "MURRAY, PATTY"),
                                
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

)) #end shinyUI