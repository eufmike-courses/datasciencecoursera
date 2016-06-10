library(Lahman)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(rCharts)

# import data from 1871 to 2015
Master <- read.csv("./data/2015/Master.csv")
Batting <- read.csv("./data/2015/Batting.csv")

# combine data year of birth with batting data sheet
Master.year <- Master[, c("playerID", "birthYear", "nameFirst", "nameLast", "nameGiven")]
Batting.year <- merge(Batting, Master.year)
Batting.2 <- tbl_df(Batting.year)

# create two columns: batting average and age
Batting.2 <- Batting.2 %>% 
        mutate(., battingaverage = H/AB) %>%
        mutate(., age = yearID - birthYear) %>%
        mutate(., nameFull = paste(nameGiven, nameLast)) 

summary(Batting.2)
dim(Batting.2)

# remove NA value in year and batting average
Batting.2 <- Batting.2[!is.na(Batting.2$battingaverage), ]
Batting.2 <- Batting.2[!is.na(Batting.2$birthYear), ]
summary(Batting.2)
dim(Batting.2)

# check why the age data
hist(Batting.2$age)
table(Batting.2$age)

# remove the error of negative age
Batting.2$age[Batting.2$age <0]
which(Batting.2$age < 0)
Batting.2[which(Batting.2$age < 0), c("nameFirst", "nameLast", "nameGiven", "yearID", "birthYear")]
Batting.2$birthYear[grepl("William F.", Batting.2$nameGiven) & 
                            grepl("Johnson", Batting.2$nameLast)] <- 1862

# check the negative age are removed
hist(Batting.2$age)
table(Batting.2$age)

# merge name to nameFull
Batting.2 <- Batting.2 %>% 
                mutate(., age = yearID - birthYear) 
head(Batting.2)

# Batting average whole life
Batting.life <- Batting.2 %>%
        group_by(., playerID) %>%
        summarise(., avg = mean(battingaverage, na.rm = T),
                  total.AB = sum(AB, na.rm = T)) %>%
        filter(., total.AB >= 500) %>%
        arrange(., desc(avg))

head(Batting.life)
Batting.new <- Batting.2[Batting.2$playerID %in% Batting.life$playerID, ]

Batting.new[Batting.new$age == max(Batting.new$age), c("playerID", "yearID","H", "battingaverage", "age")]

p1 <- ggplot(Batting.new, aes(x = age, y = battingaverage)) + geom_point()
p1

# Generate cumulative hit data
Batting.3 <- Batting
for (i in unique(Batting.3$playerID)){
        Batting.3$H_cum[Batting.3$playerID == i] <- cumsum(Batting.3$H[Batting.3$playerID == i])
        
}

p3 <- ggplot(Batting.3, aes(x = yearID, y = H_cum)) + geom_point()
p3

