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
                            titlePanel("Mapping Money"),
                            sidebarLayout(
                              sidebarPanel(
                                h3("Explore which parts of the country are donating money to specific politicians."),
                                h5("To use, create new plot."),
                                selectInput("election", "Election Type:", c("Loading...")),
                                selectInput("year", "Year:", c("Loading...")),
                                selectInput("party", "General Party:", c("Loading...")),
                                selectizeInput(
                                  'candidate', 'Candidate', choices = c("Loading..."),
                                  multiple = FALSE
                                ),
                                actionButton("update", "Update Plot"),
                                actionButton("new", "Create New Plot")
                              ),
                              # MainPanel contains uiOutput, uiOutput "goTab" is rendered by the server
                              # and produces the tab output.
                              mainPanel(
                                uiOutput("goTab")
                              )
                          ) # end sidebarlayout 
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
                              tabPanel("General Exploration",
                                titlePanel("Candidate Winning Percentage Based Off Spending"),
                                sidebarLayout(
                                  sidebarPanel(
                                    h5("This graph helps to visualize a correlation between politician's spending money and their percentage of either losing or winning that 
      election.  Note that if you scroll to a money range where no pie chart displays, that means we have no data on politicians spending
                                       that amount for their campaign."),
                                    # Radio buttons for either the user to see if they want to see data from the House or the Senate
                                    radioButtons("govBranch", label = h3("Select Election Type:"), 
                                                 choices = c("House of Representatives", "Senate", "Presidential"), selected = "House of Representatives"),
                                    uiOutput("ui")
                                    #The slider for the user to select the range of money that they want to see
                                  ),
                                  
                                  
                                  mainPanel(
                                    plotlyOutput("electionChart")
                                  )
                                )
                              ),
                              #Create tab panel for "Case-Study Exploration"
                              tabPanel("Case-Study Exploration",
                                titlePanel("Case-Study Exploration"),
                                sidebarLayout(
                                  #Sidebar control unit
                                  sidebarPanel(
                                    h3("Explore specific candidate's total contributions and their election results."),
                                    selectInput("election2", "Election Type:", c("Loading...")),
                                    selectInput("state", "Election Jurisdiction:", c("Loading...")),
                                    selectInput("year2", "Year:", c("Loading..."))
                                  ),
                                  mainPanel(
                                    #Output
                                    plotlyOutput("winPlot", width = '100%', height = 'auto')
                                  )
                                )
                              )
                            )
                            
                   ), # end tabPanel
                   
                   tabPanel("About",
                            titlePanel("About"),
                            uiOutput("about"), width = 12
                    )
)) #end shinyUI