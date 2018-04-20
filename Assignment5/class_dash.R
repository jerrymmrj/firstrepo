#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(knitr)
library(rmarkdown)
library(tidyverse)
library(zoo)
library(foreign)
library(readxl)
library(stringr)
data <- NULL
time <- seq(1988,2017)
time <- time[-26]
n <- length(time)
for (i in 1:n){
  filename<-paste("/Users/billyxiao/schoolworks/spring2018/ma415/MA415/assignment5/weatherdata/46035h",time[i],".txt",sep = "")
  tex<-read.table(filename,header = TRUE, fill = TRUE)
  if(!is.null(data)){
    names(tex)<-names(data)
  }
  if(length(tex[1,])==16|length(tex[1,])==17){
    sub_tex<-tex[,c(1,2,3,4,13,14)]
  }
  if(length(tex[1,])==18){
    sub_tex<-tex[,c(1,2,3,4,14,15)]
  }
  names(sub_tex)<-c('YY','MM','DD','hh','ATMP','WTMP')
  data<-rbind(data,sub_tex)
}
data <- subset(data,data$ATMP!=999&data$WTMP!=999)
daily <- data %>%
  group_by(MM,YY,DD)%>%
  summarise(ATMP=sample(ATMP,size = 1),WTMP=sample(WTMP,size = 1))
daily[daily$YY<1999,]$YY<-daily[daily$YY<1999,]$YY+1900
daily$time<-paste(daily$YY,"-",daily$MM,"-",daily$DD,sep = "")
daily$time<-as.Date(daily$time,format = "%Y-%m-%d")

#View(daily)

year1<-daily[daily$YY==1988,]
year2<-daily[daily$YY==2017,]
sample1<-sample(year1$ATMP,size=100)
sample2<-sample(year2$ATMP,size=100)
sample3<-sample(year1$WTMP,size=100)
sample4<-sample(year2$WTMP,size=100)
test1<-t.test(sample1,sample2)
test2<-t.test(sample3,sample4)

#analysis
year <- function(x) as.POSIXlt(x)$year + 1900
#qplot(time,ATMP, data = daily, geom = "line",main = "The daily tempreature",colour=year(time))
#qplot(time,WTMP, data = daily, geom = "line",main = "The sea tempreature",colour=year(time))


veg1 <- read_xlsx("/Users/billyxiao/schoolworks/spring2018/ma415/MA415/assignment5/veg1.xlsx")


##Data Cleaning & tidy
a <- apply(veg1, 2, n_distinct)
c <- names(a[a>1])

veg2 <- select(veg1, c)
apply(veg2, 2, n_distinct)

veg.tidy <- veg2 %>%
  dplyr::rename(Area = `Geo Level`, State = `State ANSI`,
                Data = `Data Item`, Category = `Domain Category`) %>%
  separate(Category, into = c("Label", "Type"), sep=",") %>%
  separate(Data, into=c("A","Class Desc"),sep=" - ") %>%
  separate(`Class Desc`, into=c("Class Desc","Production Practice","Unit Desc"),sep=",") %>%
  separate(`Production Practice`,into=c("Production Practice","Utilization Practice","Statistic Category"),sep=" / ") %>%
  separate(Domain,into=c("Domain","B"),sep=", ") %>%
  dplyr::rename(Type=`B`,Chemical=`Type`) %>%
  separate(Chemical, into=c("C","Active Ingredient or Action Taken"),sep=": ") %>%
  separate(`Active Ingredient or Action Taken`, into=c("D","Active ingredient or Action Taken","E"),sep=c(1,-2)) %>%
  separate(`Active ingredient or Action Taken`, into=c("Active ingredient or Action Taken","EPA Pesticide Chemical Code"),sep="=") %>%
  separate(Area,into=c("Area","G"),sep=" : ") %>%
  select(-A,-Label,-C,-D,-E,-G)


##Restricted use chemical

veg4 <- veg.tidy %>%
  filter(Domain=="RESTRICTED USE CHEMICAL") %>%
  select(Commodity, Domain:`EPA Pesticide Chemical Code`) %>%
  unique()


##toxicity measurement
toxicity <- tibble(
  `Toxicity Measurements(mg/kg)` =
    c(20, 5620, 20, 11,
      869, 54, 5000, 82,
      869, 3129, 458, 450,
      14, 12, 50, 50,
      430, 1563, 86,380,
      54, 5000, 3129, 458,
      450, 144, 12, 50,
      50, 430, 1563, 86,
      300, 60, 1.9, 72.1,
      82, 869, 150, 300,
      4640, 56, 16, 73,
      1.9, 56, 73, 121
    )
)
veg4 <- veg4 %>%
  bind_cols(toxicity)

bro<-filter(veg4,Commodity=="BROCCOLI")
cau<-filter(veg4,Commodity=="CAULIFLOWER")
graph1<-ggplot(data=bro,mapping=aes(x=`Active ingredient or Action Taken`,
                                    y=`Toxicity Measurements(mg/kg)`))+labs(title = "Broccoli")+
  geom_bar(stat = "identity")+coord_flip()
graph2<-ggplot(data=cau,mapping=aes(x=`Active ingredient or Action Taken`,
                                    y=`Toxicity Measurements(mg/kg)`))+labs(title = "Cauliflower")+
  geom_bar(stat = "identity")+coord_flip()
#bro1 <- bro[c(3:7),]

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "project5part2"),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Temperature", tabName = "temperature", icon = icon("dashboard"),
             menuSubItem('DailyTemperature',
                         tabName = 'dailyTemperature',
                         icon = icon('line-chart')),
             menuSubItem('SeaTemperature',
                         tabName = 'seaTemperature',
                         icon = icon('line-chart')),
             menuSubItem('Analysis1',tabName = 'analysis1')
             ),
    menuItem("Chemical", icon = icon("th"), tabName = "chemical",
             menuSubItem('Brocolli',tabName = 'brocolli',icon = icon('bar-chart')),
             menuSubItem('Cauliflower',tabName = 'cauliflower', icon = icon('bar-chart')),
             menuSubItem('Analysis2',tabName = 'analysis2'))
  )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dailyTemperature",
    fluidRow(
      h2("daily temperature plot"),
      box(
        sliderInput("range", "time range of Daily Temperature:",
                                         min = 1,
                                          max = 8280,
                                          value = c(1,8280))
    ),
    # box(sliderInput("range2","time range of Sea temperature",
    #                 min=1,
    #                 max=8280,
    #                 value = c(1,8280))),
    
    box(
      plotOutput("distPlot",click = "plot1_click"),
      column(width = 12,
             h4("Points near click"),
             verbatimTextOutput("click_info1")
      )
      
    ))),#end tabitem1
    tabItem(tabName = 'seaTemperature',
            fluidRow(
              h2('sea temperature plot'),
              box(sliderInput("range2","time range of Sea temperature",
                              min=1,
                              max=8280,
                              value = c(1,8280))),
              box(plotOutput("distPlotSeaTemp",click = "plot2_click"),
                  column(width = 12,
                         h4("Points near click"),
                         verbatimTextOutput("click_info2")
                  ))
            )),#end tabitem2
    tabItem(tabName = 'analysis1',
            fluidRow(
              h2('analysis of plots and t tests'),
              box(textOutput("ttestOut1")),
              box(textOutput("ttestOut2")),
              box(
                p("In this case, we randomly select 100 samples from 1988 and 2017 to do the 
                  t-test for identifying the change of daily and sea tempreture over around 30 years.
                  The test1 is about daily tempreture,as we can see,the p-value of both test is 
                  smaller than 1% which means that the significance the difference is statistically 
                  significant.Therefore, the mean tempreature changes over 30 years. The sampling 
                  does light effect to the evaluation of temperature change since the 
                  tempreature is different each days or hours. Analyze them by random selecting 
                  creates some difference but the general trend will not be changed."))
            ))

  ,
  
  
  tabItem(tabName = "brocolli",
          fluidRow(
            h2('broccoli treatment toxicity'),
            #box(tableOutput('tableOut'))
            box(sliderInput("range3","broccoli range of displaying treaments",
                            min=1,
                            max=28,
                            value = c(1,28))),
            box(plotOutput("brocolliplot")
                )
          )),
  tabItem(tabName = 'cauliflower',
          fluidRow(
            h2('cauliflower treatment toxicity'),
            box(sliderInput("range4","cauliflower range of displaying treaments",
                            min=1,
                            max=20,
                            value = c(1,20))),
            box(plotOutput("cauplot")
                )
          )),
  tabItem((tabName = 'analysis2'),
          fluidPage(
            h2('a?????')
          ))
 )
 ))
  
  


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      #x    <- faithful[, 2]
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
      # draw the histogram with the specified number of bins
      qplot(time,ATMP, data = daily, geom = "line",main = "The daily temperature",
            colour=year(time))+
        coord_cartesian(xlim=c(as.Date(input$range[1],origin="1988-01-01"),as.Date(input$range[2],origin="1988-01-01")))
      
   })
   
   output$distPlotSeaTemp <- renderPlot({
     qplot(x=time,y=WTMP, data = daily, geom = "line",main = "The sea temperature",
           colour=year(time))+
       coord_cartesian(xlim=c(as.Date(input$range2[1],origin="1988-01-01"),as.Date(input$range2[2],origin="1988-01-01")))
   }
   )
   
   output$ttestOut1 = renderPrint({
     return(test1)
   })

   output$ttestOut2 = renderPrint({
     return(test2)
   })
   
   output$click_info1 <- renderPrint({
     # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
     # were a base graphics plot, we'd need those.
     nearPoints(daily, input$plot1_click, addDist = TRUE)
     
   })
   output$click_info2 <- renderPrint({
     nearPoints(daily, input$plot2_click, addDist = TRUE)
     
   })
   # output$tableOut = renderTable(toxicity)
   output$brocolliplot <- renderPlot({
     
     bro1 <- bro[c(input$range3[1]:input$range3[2]),]
     ggplot(data=bro1,mapping=aes(x=`Active ingredient or Action Taken`,
                                         y=`Toxicity Measurements(mg/kg)`))+labs(title = "Broccoli")+
       geom_bar(stat = "identity")+coord_flip()
     #input$range3
   })
   
   output$cauplot <- renderPlot({
     cau1 <- cau[c(input$range4[1]:input$range4[2]),]
     ggplot(data=cau1,mapping=aes(x=`Active ingredient or Action Taken`,
                                 y=`Toxicity Measurements(mg/kg)`))+labs(title = "Cauliflower")+
       geom_bar(stat = "identity")+coord_flip()
   })

   
}

# Run the application 
shinyApp(ui = ui, server = server)
#?nearPoints()
