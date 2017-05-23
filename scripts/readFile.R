# load library
library(dplyr)

can.wa.16.raw <- read.csv('./data/2016_WA_Candidates.csv', stringsAsFactors = FALSE)

can.wa.16 <- can.wa.16.raw %>% select(Candidate.id, Candidate, Election_Status, Status_of_Candidate,
                                      Specific_Party, General_Party, Office_Sought, Num_of_Records, Total_dollar)
write.csv(can.wa.16, file = 'data/filtered_WA_can_16.csv', row.names = FALSE)


