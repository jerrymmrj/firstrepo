library(shiny)
library(ggplot2)
library(tidyverse)
library(shinythemes)
library(shinydashboard)
library(curl)
library(jsonlite)


ui <- fluidPage(theme = shinytheme("united"),
                titlePanel("Bitcoin price"),
                
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Myapp",
                  tabPanel("Part1",
                           sidebarPanel(
                             selectInput("var", 
                                         label = "Choose the varaible to display",
                                         choices = "",
                                         selected = ""),
                             sliderInput("range", "Time interval:",min =as.Date("2014-04-01"), 
                                         max = as.Date("2018-05-02"), value =c(as.Date("2018-03-02"),as.Date("2018-05-02")))),
                           mainPanel(
                             tabsetPanel(type = "tabs",
                                         tabPanel("interval", plotOutput("plot_11"),h4(' In this project, I have used skills that I learnt from this class including tidyverse,ggplot,shinny app.The graoh shows my study of digital currency, the Bitcoin.In my study, I focus on the influence of government regulation on Bitcoin price. I consider that government regulation would influence the price of Bitcoin. I use the API from Alphavantage which is an api about information of Bitcoin price in CNY, the Chinese RMB. To confirm my opinion, my example is about South Korean???s regulation in 2018. The South Korean announce in January that they have closed two of the biggest digital currency exchange and they would prohibit the trade of digital currency in their county in the future. As we can observe from my graph which has price of bitcoin on the Y axis and data on the X axis. The open price of Bitcoin breaks its prior bottom and it got a new low. Therefore I confirm that the regulation of government has influence on the price of Bitcoins.'))
                             ))
                  )
                )
)

