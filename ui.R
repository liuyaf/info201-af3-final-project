# Matthew Li
# Info 201 Section AF
# Assignment 8
# ui.R file for building user interface appearance for data visualization of 
# the Cereal data.

library(shiny)
library(plotly)

shinyUI(navbarPage("Political Bidding",
                   # Tab Panel will show a grouped representation of the cereals based on input from
                   # the user via the 2 selectInput widgets, graph's marker size is based on frequency
                   tabPanel("2016 Election",
                            sidebarPanel(
                              
                            ), # end sidebar
                            
                            # Main panel of the Plot tab shows the outputted plot given the user's inputs 
                            mainPanel(
                              
                            ) # end mainPanel
                   ),  # end tabPanel
                   
                   # Tab Panel shows the raw data that the plots are being formed by.
                   tabPanel("Washington State"
                            
                   ) # end tabPanel

                   
)) #end shinyUI