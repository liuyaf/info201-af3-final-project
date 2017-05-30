# load library
library(knitr)
library(httr)
library(dplyr)
library(jsonlite)

# read candidate file and wrangle the data
can.wa.16.raw <- read.csv('./data/2016_WA_Candidates.csv', stringsAsFactors = FALSE)

can.wa.16 <- can.wa.16.raw %>% select(Candidate_Entity.id, Candidate, Election_Status, Status_of_Candidate,
                                      Specific_Party, General_Party, Office_Sought, Num_of_Records, Total_dollar)


zip <- read.csv('./data/zip_codes_states.csv', stringsAsFactors = FALSE)
zip <- zip %>% select(zip_code,latitude,longitude)

# stores global variable
base.uri <- 'http://api.followthemoney.org/'

# each apikey has limited amout of search time, in case it's not working
# we here to provide some extra ones
api.key <- 'b6349b9565e5e9bc024b94923636fae5'
# extra0: babd0de84e727110ff37faed81c9da27'
# extra1: 80a85901cac92dfe0d3d9752152f15f9
# extra2: 0bf78baa9ca34b47d96c966cff0804a1

year <- 2016
mode.json <- 'json'


# the function takes name and returns a dataframe containging
# first 100 contributors to selected candidate
# dataframe contains name of oragization, zip code, # of records and total number
GetContributor <- function(can.name) {
  # get candidate id
  can.df <- can.wa.16 %>% filter(Candidate == can.name)
  can.id <- as.character(can.df$Candidate_Entity.id)
  
  # query info through api
  query.params <- list(y = year, 'f-fc' = '1,2,3', 'c-t-eid' = can.id, 
                       gro = 'd-eid,d-ad-zip,d-cci', APIKey = api.key, mode = mode.json)
  
  response <- GET(base.uri, query = query.params)
  
  # Use fromJSON to parse
  response.content <- content(response, "text")
  body.data <- fromJSON(response.content)
  
  # wrangle the dataframe 
  df <- flatten(body.data$records)
  df <- df %>% select(Contributor.Contributor, Zip.id,Broad_Sector.Broad_Sector, `#_of_Records.#_of_Records`,`Total_$.Total_$`)
  colnames(df) <- c('name', 'zip', 'industry','records', 'total')
  df$zip <- as.integer(df$zip)
  df <- left_join(df, zip, by = c('zip' = 'zip_code'))
  df$total <- as.numeric(df$total)
  return(df)
}

a <- GetContributor('MURRAY, PATTY')

# the function takes a candidate name as variable and returns a dataframe
# containing industry of all contribution to the candidate and the total number of contribution
GetIndustryPercent <- function(can.name) {
  # get candidate id
  can.df <- can.wa.16 %>% filter(Candidate == can.name)
  can.id <- as.character(can.df$Candidate_Entity.id)
  
  # the indursty data have 2 pages
  # query first page
  query.params <- list(y = year, 'c-t-eid' = can.id, gro = 'd-cci', APIKey = api.key, mode = mode.json)
  response <- GET(base.uri, query = query.params)
  
  # Use fromJSON to parse information
  response.content <- content(response, 'text')
  body.data <- fromJSON(response.content)
  df1 <- flatten(body.data$records)
  
  # query 2nd page
  query.params$p <- 1
  response <- GET(base.uri, query = query.params)
  
  # parse response
  response.content <- content(response, 'text')
  body.data <- fromJSON(response.content)
  df2 <- flatten(body.data$records)
  
  # bind and wrangle the dataframe
  df <- bind_rows(df1,df2)
  
  df <- df %>% select(Broad_Sector.Broad_Sector, `#_of_Records.#_of_Records`,`Total_$.Total_$`)
  colnames(df) <- c('industry', 'records', 'total')
  df$records <- as.integer(df$records)
  df$total <- as.numeric(df$total)
  total.don <- sum(df$total)
  df <- df %>% group_by(industry) %>% 
    summarise(total_record = sum(records), total = sum(total), percent = total / total.don)
  return(df)
}

b <- GetIndustryPercent('MURRAY, PATTY')

