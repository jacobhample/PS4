# Jacob Hample
# Professor Montgomery
# Applied Statistical Programming
# February 25, 2016

## Problem Set 4 ##

## Importing Data
library(rvest)
wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

# Imports tables from URL
temp <- wikiURL %>% 
  read_html %>%
  html_nodes("table")

# Isolates the primary table from URL
election.data <- html_table(temp[[2]], fill = TRUE, trim = TRUE)


## Cleaning Data
# Changes column names
colnames(election.data) <- c("Election #", 
                             "Year", 
                             "Electoral College Winner", 
                             "Electoral College Winner Party", 
                             "Popular Vote %", 
                             "Popular Vote Margin %", 
                             "Popular Vote", 
                             "Popular Vote Margin", 
                             "Electoral College Runner-Up", 
                             "Electoral College Runner-Up Party", 
                             "Turnout")

# Removes redundant first two rows
election.data <- election.data[-c(1,2), ]

# Gets rid of strange duplicate formatting in Popular Vote Margin % column
election.data$`Popular Vote Margin %`[1] <- substr(election.data$`Popular Vote Margin %`[1], 7, 13)
for (i in 2:4) {
  election.data$`Popular Vote Margin %`[i] <- substr(election.data$`Popular Vote Margin %`[i], 7, 12)
}
for (i in 5:29) {
  election.data$`Popular Vote Margin %`[i] <- substr(election.data$`Popular Vote Margin %`[i], 7, 11)
}

# Removes % symbol in Popular Vote Margin % column
for (i in 1:length(election.data$`Popular Vote Margin %`)) {
  election.data$`Popular Vote Margin %`[i] <- gsub("%", "", election.data$`Popular Vote Margin %`[i])
}

# Removes negative symbols from first four rows of Popular Vote Margin % column
election.data$`Popular Vote Margin %`[1] <- substr(election.data$`Popular Vote Margin %`[1], 2, 6)
for (i in 2:4) {
  election.data$`Popular Vote Margin %`[i] <- substr(election.data$`Popular Vote Margin %`[i], 2, 5)
}

# Changes Popular Vote Margin % column to a numeric
election.data$`Popular Vote Margin %` <- as.numeric(election.data$`Popular Vote Margin %`)

# Restores negative values of Popular Vote Margin column
for (i in 1:4) {
  election.data$`Popular Vote Margin %`[i] <- -1 * election.data$`Popular Vote Margin %`[i]
}

