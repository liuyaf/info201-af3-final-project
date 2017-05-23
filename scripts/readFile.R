# load library
library(knitr)
library(httr)
library(dplyr)
library(jsonlite)

# read candidate file and wrangle the data
can.wa.16.raw <- read.csv('./data/2016_WA_Candidates.csv', stringsAsFactors = FALSE)

can.wa.16 <- can.wa.16.raw %>% select(Candidate_Entity.id, Candidate, Election_Status, Status_of_Candidate,
                                      Specific_Party, General_Party, Office_Sought, Num_of_Records, Total_dollar)
write.csv(can.wa.16, file = 'data/filtered_WA_can_16.csv', row.names = FALSE)

# stores global variable
base.uri <- 'http://api.followthemoney.org/'
api.key <- 'b6349b9565e5e9bc024b94923636fae5'
year <- 2016
mode.json <- 'json'

# the function takes name and returns contributor info in dataframe

GetContributor <- function(can.name) {
  # get candidate id
  can.df <- can.wa.16 %>% filter(Candidate == can.name)
  can.id <- as.character(can.df$Candidate_Entity.id)
  
  # query info through api
  query.params <- list(y = year, 'f-fc' = '1,2,3', 'c-t-eid' = can.id, 
                       gro = 'd-eid', APIKey = api.key, mode = mode.json)

  response <- GET(base.uri, query = query.params)
  
  # Use fromJSON to parse
  response.content <- content(response, "text")
  body.data <- fromJSON(response.content)
  
  return(body.data)
}

b <- GetContributor('MURRAY, PATTY')

a <- read.csv('./data/USZipCodes.csv', stringsAsFactors = FALSE)
