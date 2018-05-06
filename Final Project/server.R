url<-'https://www.alphavantage.co/query?function=DIGITAL_CURRENCY_DAILY&symbol=BTC&market=CNY&apikey=7UZIED5JPZSAQMPT'
data1 <- read_json(url,format="jsonl")

thedata<-lapply(data1$`Time Series (Digital Currency Daily)`,unlist)
thedata<-as.data.frame(thedata)
thedata<-t(thedata)
thedata<-thedata[,-c(2,4,6,8,10)]
col_names<-colnames(thedata)
col_names<-unlist(lapply(col_names,function(x){
  temp<-unlist(strsplit(x,"[.]"))
  temp[2]
}))
col_names<-gsub(" ","",col_names)
col_names<-gsub("[(]","_",col_names)
col_names<-gsub("[)]","",col_names)
time<-rownames(thedata)
time<-gsub("X","",time)
time<-as.Date(gsub("[.]","-",time),format="%Y-%m-%d")
mydata<-data.frame(thedata)
names(mydata)<-col_names
mydata$time<-time
mydata[,1:5]<-apply(mydata[,1:5],1,as.numeric)

server <- function(input, output,session) {
  
  
  updateSliderInput(session,"range", "Time interval:",min =time[length(time)], 
                    max =time[1], value = c(time[50],time[1]))
  
  updateSelectInput(session, "var", 
                    label = "Choose the varaible to display",
                    choices = col_names,
                    selected = "open_CNY")
  
  thedata<-reactive({
    date1<-input$range[1]
    date2<-input$range[2]
    select_col<-input$var
    temp<-subset(mydata,mydata$time>=date1&mydata$time<=date2)
    temp
  })
  
  output$plot_11<-renderPlot({
    if(!is.null(thedata())){
      ggplot(thedata(),aes_string(x='time',y=input$var))+geom_line()
    }
  })
}
