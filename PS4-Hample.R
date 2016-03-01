# Jacob Hample
# Professor Montgomery
# Applied Statistical Programming
# February 25, 2016

## Problem Set 4 ##

library(rvest)
library(plyr)

## Importng Data
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
election.data <- election.data[-c(1, 2), ]

# Removes strange duplicate formatting in Popular Vote Margin % column
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

# Removes % symbol in Turnout column
for (i in 1:length(election.data$Turnout)) {
  election.data$Turnout[i] <- gsub("%", "", election.data$Turnout[i])
}

# Removes negative symbols from first four rows of Popular Vote Margin % column
election.data$`Popular Vote Margin %`[1] <- substr(election.data$`Popular Vote Margin %`[1], 2, 6)
for (i in 2:4) {
  election.data$`Popular Vote Margin %`[i] <- substr(election.data$`Popular Vote Margin %`[i], 2, 5)
}

# Changes Popular Vote Margin % column to a numeric
election.data$`Popular Vote Margin %` <- as.numeric(election.data$`Popular Vote Margin %`)

# Changes Turnout column to a numeric
election.data$Turnout <- as.numeric(election.data$Turnout)

# Restores negative values of Popular Vote Margin column
for (i in 1:4) {
  election.data$`Popular Vote Margin %`[i] <- -1 * election.data$`Popular Vote Margin %`[i]
}

# Sorts data by year
election.data <- arrange(election.data, election.data$Year)


## Subsetting Data for Plots
# Subsets Democratic Electoral College winners
dem.rows <- grep("Dem.", election.data$`Electoral College Winner Party`)
dem.data <- election.data[dem.rows, ]

# Subsets Republican Electoral College winners
rep.rows <- grep("Rep.", election.data$`Electoral College Winner Party`)
rep.data <- election.data[rep.rows, ]

# Subsets other party Electoral College winners
dr.rows <- grep("D.-R.", election.data$`Electoral College Winner Party`)
whig.rows <- grep("Whig", election.data$`Electoral College Winner Party`)
other.rows <- c(dr.rows, whig.rows)
other.data <- election.data[other.rows, ]


## Plotting Trends
# Sets working directory
setwd("/Users/Jacob/Google Drive/Senior Year/Spring 2016/Statistical Programming/Problem Sets/PS4")

# Plot 1
# Creates empty plot
plot(NULL, NULL,
     xlab = "Year", 
     ylab = "Popular Vote Margin (%)", 
     main = "Popular Vote Margin by Year",
     xlim = c(1820,2020),
     ylim = c(-15, 40))

# Adds line chart of popular vote margin by year
lines(election.data$Year, election.data$`Popular Vote Margin %`, lwd = 2)

# Plots Democratic data points
points(dem.data$Year, dem.data$`Popular Vote Margin %`, pch = 17, col = "BLUE")

# Plots Republican data points
points(rep.data$Year, rep.data$`Popular Vote Margin %`, pch = 15, col = "RED")

# Plots other Party data points
points(other.data$Year, other.data$`Popular Vote Margin %`, pch = 16, col = "PURPLE")

# Adds dotted line at 0%
abline(0, 0, lty = 2)

# Creates Legend
legend("topleft",
       legend = c("Democratic Winners", "Republican Winners", "Other Party Winners"), 
       pch = c(17, 15, 16),
       col = c("BLUE", "RED", "PURPLE"), cex = 0.9)

# Plot 2
# Creates empty plot
plot(NULL, NULL,
     xlab = "Year", 
     ylab = "Voter Turnout (%)", 
     main = "Voter Turnout by Year",
     xlim = c(1820,2020),
     ylim = c(25, 90))

# Adds line chart of voter turnout by year
lines(election.data$Year, election.data$Turnout, lwd = 2)

# Plots Democratic data points
points(dem.data$Year, dem.data$Turnout, pch = 17, col = "BLUE")

# Plots Republican data points
points(rep.data$Year, rep.data$Turnout, pch = 15, col = "RED")

#Plots other party data points
points(other.data$Year, other.data$Turnout, pch = 16, col = "PURPLE")

# Creates Legend
legend("topright",
       legend = c("Democratic Winners", "Republican Winners", "Other Party Winners"), 
       pch = c(17, 15, 16),
       col = c("BLUE", "RED", "PURPLE"), cex = 0.9)


