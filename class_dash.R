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
  `Toxicity Measurements` =
    c("20-150 mg/kg", "5620-8350 mg/kg", "20-150 mg/kg", "11 mg/kg",
      "869-1271 mg/kg", "54-70 mg/kg", "5000 mg/kg", "82-270 mg/kg",
      "869-1271 mg/kg", "3129 mg/kg", "458 mg/kg", "450 mg/kg",
      "144 mg/kg", "12-48 mg/kg", "50-281 mg/kg", "50 mg/kg",
      "430-4000 mg/kg", "1563 mg/kg", "86 mg/kg", "380-651 mg/kg",
      "54-70 mg/kg", "5000 mg/kg", "3129 mg/kg", "458 mg/kg",
      "450 mg/kg", "144 mg/kg", "12-48 mg/kg", "50-281 mg/kg",
      "50 mg/kg", "430-4000 mg/kg", "1563 mg/kg", "86 mg/kg",
      "300-400 mg/kg", "60-387 mg/kg", "1.9-12.5 mg/kg", "72.1 mg/kg",
      "82-270 mg/kg", "869-1271 mg/kg", "150-500 mg/kg", "300-400 mg/kg",
      "4640 mg/kg", "56-79 mg/kg", "16-21 mg/kg", "73-79 mg/kg",
      "1.9-12.5 mg/kg", "56-79 mg/kg", "73-79 mg/kg", "121 mg/kg"
    )
)

veg4 <- veg4 %>%
  bind_cols(toxicity)


library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "project5part2"),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Temperature", tabName = "temperature", icon = icon("dashboard")),
    menuItem("Chemical", icon = icon("th"), tabName = "chemical")
  )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "temperature",
    fluidRow(
      h2("plots"),
      box(
        sliderInput("range", "time range of Daily Temperature:",
                                         min = 1,
                                          max = 8280,
                                          value = c(1,8280))
    ),
    box(sliderInput("range2","time range of Sea temperature",
                    min=1,
                    max=8280,
                    value = c(1,8280))),
    
    box(
      plotOutput("distPlot")
      
    ),
    box(plotOutput("distPlotSeaTemp")),
    h2("t tests"),
    box(textOutput("ttestOut1")),
    box(textOutput("ttestOut2")),
    h2("analysis"),
    box(
    p("In this case, we randomly select 100 samples from 1988 and 2017 to do the 
t-test for identifying the change of daily and sea tempreture over around 30 years.
The test1 is about daily tempreture,as we can see,the p-value of both test is 
smaller than 1% which means that the significance the difference is statistically 
significant.Therefore, the mean tempreature changes over 30 years. The sampling 
      does light effect to the evaluation of temperature change since the 
      tempreature is different each days or hours. Analyze them by random selecting 
      creates some difference but the general trend will not be changed."))
  )
  ),
  
  
  tabItem(tabName = "chemical",
          fluidRow(
            box(tableOutput('tableOut'))
          ))
 )
 )
)
  
  


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
   
   output$tableOut = renderTable(toxicity)
}

# Run the application 
shinyApp(ui = ui, server = server)

