library(Lahman)
data(Master)
head(Master)
dim(Master)
nameID <- Master[grepl("Ichiro", Master$nameGiven),1]

data(Batting)
data <- Batting[grepl("suzukic01", Batting$playerID), ]
data

