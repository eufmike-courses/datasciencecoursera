# This is a simple application presenting batting performance of the iconic MLB
# player, Suzuki Ichiro, through 2001 to 2014.

library(Lahman)
library(ggplot2)
library(gridExtra)
library(dplyr)

# extract playerID from "Master" database
data(Master)
nameID <- Master[grepl("Ichiro", Master$nameGiven),1]

data(Batting)
data <- Batting[grepl("suzukic01", Batting$playerID), ]
data

data1 <- data
data1$batting.average <- data$H/data$AB
str(data1)

# p1 <- ggplot(data1, aes(x = yearID, y = H)) +
#         geom_line() + geom_point()
#
# p2 <- ggplot(data1, aes(x = yearID, y = batting.average)) +
#         geom_line() + geom_point()
#
#grid.arrange(p1, p2, ncol = 2)

Batting.2 <- tbl_df(Batting)
Batting.2 <- Batting.2 %>% mutate(., batting.average = H/AB)
head(Batting.2)

# life batting ranking

Batting.life <- Batting.2 %>%
                group_by(., playerID) %>%
                summarise(., avg = mean(batting.average, na.rm = T),
                                total.AB = sum(AB, na.rm = T)) %>%
                arrange(., desc(avg))

Batting.life

p <- ggplot(Batting.life, aes(x = total.AB, y = avg)) + geom_point()
p
