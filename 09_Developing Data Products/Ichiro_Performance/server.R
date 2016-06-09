# This is a simple app for exploring batting record of MLB from 1871 to 2014.

library(shiny)
library(Lahman)
library(ggplot2)
library(gridExtra)
library(dplyr)

data(Master)
data(Batting)
Master.year <- Master[, c("playerID", "birthYear")]
Batting.year <- merge(Batting, Master.year)
Batting.2 <- tbl_df(Batting.year)

shinyServer(function(input, output) {
        output$year <- renderPrint({input$year})
        output$age <- renderPrint({input$age})
        
        output$newplot <- renderPlot({
                ## input year including start and end
                input.year <- input$year 
                input.age <- input$age
                ## generate a data set in the rage of preset year
                Batting.2 <- Batting.2 %>% 
                        mutate(., batting.average = H/AB) %>%
                        mutate(., age = yearID - birthYear) %>%
                        filter(., yearID >= input.year[1]) %>%
                        filter(., yearID <= input.year[2]) %>%
                        filter(., age >= input.age[1]) %>%
                        filter(., age <= input.age[2])
                
                Batting.life <- Batting.2 %>%
                        group_by(., playerID) %>%
                        summarise(., avg = mean(batting.average, na.rm = T),
                                  total.AB = sum(AB, na.rm = T)) %>%
                        arrange(., desc(avg))
                        
                        p1 <- ggplot(Batting.life, aes(x = total.AB, y = avg)) + geom_point()
                        p2 <- ggplot(Batting.2, aes(x = age, y = H)) + geom_point()
                        grid.arrange(p1, p2, nrow = 2)
        })

  }
  )
