# load library
library(knitr)
library(httr)
library(dplyr)
library(jsonlite)

# read candidate file and wrangle the data

can.16 <- read.csv('./data/Candidate_2016.csv', stringsAsFactors = FALSE)


zip <- read.csv('./data/mapping/zipcode_lat_lon.csv', stringsAsFactors = FALSE)
zip <- zip %>% select(Zip_Code,Latitude,Longitude)


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
  can.df <- can.16 %>% filter(Candidate == can.name)
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
  df <- left_join(df, zip, by = c('zip' = 'Zip_Code'))
  df$total <- as.numeric(df$total)
  
  return(df)

  
}

