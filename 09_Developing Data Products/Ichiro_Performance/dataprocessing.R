library(Lahman)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(rCharts)

# # Create a complied batting dataset with the name of player, the birth year
# # (for age calculation) and the cumulative hit record.
# 
# # import data from 1871 to 2015
# Master <- read.csv("./data/2015/Master.csv")
# Batting <- read.csv("./data/2015/Batting.csv")
# 
# # combine data year of birth with batting data sheet
# Master.year <- Master[, c("playerID", "birthYear", "nameFirst", "nameLast", "nameGiven")]
# Batting.year <- merge(Batting, Master.year)
# Batting.2 <- tbl_df(Batting.year)
# Batting.2 <- Batting.2 %>% arrange(., yearID)
# 
# # Generate cumulative hit data
# for (i in unique(Batting.2$playerID)){
#         Batting.2$H_cum[Batting.2$playerID == i] <- cumsum(Batting.2$H[Batting.2$playerID == i])
#         Batting.2$firstyear[Batting.2$playerID == i] <- min(Batting.2$yearID[Batting.2$playerID == i])
# 
# }
# write.csv(Batting.2, "./data/Batting_Master.csv")



Batting.data <- read.csv("./data/Batting_Master.csv")

# create two columns: batting average and age
Batting.2 <- Batting.data %>% 
        mutate(., battingaverage = H/AB) %>%
        mutate(., age = yearID - birthYear) %>%
        mutate(., year.in.MLB = yearID - firstyear + 1 ) %>%
        mutate(., nameFull = paste(nameGiven, nameLast)) 

hit.perfomance <- Batting.2 %>%
                group_by(., playerID) %>%
                summarise(., total_hit = sum(H, na.rm = T)) %>%
                filter(., total_hit > 2500)

Batting.all.total <- Batting.2[Batting.2$playerID %in% hit.perfomance$playerID, ]


battingplot <- function(data, playerID, x, y){
                Batting.playerID <- data[data$playerID == playerID, ]
                p <- ggplot(data, aes_string(x = x, y = y, group = "nameFull")) + 
                        geom_line() +
                        geom_point() +  
                        geom_point(data = Batting.playerID, 
                                   aes_string(x = x, y = y), color = "red") + 
                        geom_line(data = Batting.playerID, 
                                  aes_string(x = x, y = y), color = "red")
                return(p)
                }

p1 <- battingplot(Batting.all.total, "suzukic01", "yearID", "H_cum")
p1
p2 <- battingplot(Batting.all.total, "suzukic01", "age", "H_cum")
p2
p3 <- battingplot(Batting.all.total, "suzukic01", "year.in.MLB", "H_cum")
p3
