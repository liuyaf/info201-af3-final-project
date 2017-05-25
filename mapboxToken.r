# Function makes a post call to 

library("httr")
library("jsonlite")

search.query <- list(grant_type = "client_credentials", client_id = "oUBxMAlVRNvt92rrkhyqCw", 
          client_secret = "pk.eyJ1IjoibWF0dGhld2tsaTk3IiwiYSI6ImNqMzE5M254MjAwMHYzMm1ydG43MnV6cm8ifQ.lMMwzXJokvtW4hispPYEIg")

response <- POST("https://api.yelp.com/oauth2/token", query = search.query)
response
body <- fromJSON(content(response, "text"))

access_token <- body$access_token

#USE access.code for headers
access.code <- paste0("bearer ", access_token)