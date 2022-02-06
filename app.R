# Load packages ----
library(shiny)
library(tidyquant)
library(tidyverse)
library(rdrop2)
library(shinycssloaders)
library(DT)
library(XML)
library(fpp2)

# Dropbox auth ----
drop_acc(readRDS("droptoken.rds"))
drop_auth(rdstoken="droptoken.rds")

# Global variables ----
beta<-drop_read_csv("beta.csv")

#stocks<-drop_read_csv("stocks.csv") # Read stock data from Dropbox
stocks<-read.csv("stocks.csv")
stocks <- stocks[!apply(is.na(stocks) | stocks == "", 1, all),] # Clears all empty rows
names(stocks)<-c("Stock","Quantity","Price","Date") # Rename columns
write.csv(stocks,"stocks.csv",row.names=FALSE) # Write to local file


#wallet <- drop_read_csv("wallet.csv") # Read wallet data from Dropbox
wallet<-read.csv("wallet.csv")
names(wallet)<-"Wallet" # Rename wallet (just in case)
write.csv(wallet,"wallet.csv",row.names=FALSE) # Write to local file





# Set up functions ----

# Upload files to dropbox
 upload <- function() {
   drop_upload("wallet.csv")
   drop_upload("stocks.csv")
 }

# Download from Dropbox
 download <- function() {
   stocks<-drop_download("stocks.csv",overwrite = TRUE)
   wallet<-drop_read_csv("wallet.csv")
   
 }

# Reset portfolio and wallet
resetAll <- function() {
  stocks<-stocks[0,]
  wallet<-10000000
  names(wallet)<-"Wallet"
  write.csv(stocks,"stocks.csv",row.names=FALSE)
  write.csv(wallet,"wallet.csv",row.names=FALSE)
  upload()
}

# Gets position of stock 
getStockIndex<-function(stock){
  index<-stocks[-1]
  row.names(index)<-stocks$Stock
  stockindex<-which(rownames(index)==stock)
  return(stockindex)
}


# Purchase known quantity of stocks
buyStock <- function(stock, quantity, date=Sys.Date()) { # Add stock to portfolio by requesting a number of stocks, automatically at current price

  #download()
  stocks<-read.csv("stocks.csv")
  wallet<-read.csv("wallet.csv")
  
  # DOES NOT PROVIDE HISTORICAL DATA YOU NEED TO FIX THIS OTHERWISE THE PROJECT DOESNT WORK
  complete_info<-getQuote(stock,src="yahoo", from=date, to=date)
  complete_info$`Trade Time`<-as.character(as.POSIXct(complete_info$`Trade Time`))

  if(!missing(date)){
    old_price<-tq_get(stock,get="stock.prices",from=date-4,to=as.Date(date)+4)
    complete_info$Last<-old_price$close[1]
    complete_info$`Trade Time`<-old_price$date[1]
  }
  
  cost<-quantity*complete_info$Last
  # Transaction fees - 2.5% of each transaction
  trans_fee<-0.025*cost
  cost<-cost+trans_fee
  
  if (cost<=wallet) { # Check the user can afford the purchase
    if (any(stocks$Stock==stock)) { # Check if the stock already exists in the portfolio - if so, just increment quantity
      stocks$Quantity[getStockIndex(stock)]<-stocks$Quantity[getStockIndex(stock)]+quantity
      
    }
    else { # If not, generate new data and add to table
      new<-data.frame("Stock"=stock, "Quantity"=quantity,"Price"=complete_info$Last, "Date"=as.character(as.POSIXct(complete_info$`Trade Time`)))
      names(new)<-c("Stock","Quantity","Price","Date")
      stocks<-rbind(stocks,new)
    }
    names(stocks)<-c("Stock","Quantity","Price","Date")
    # Update all records with cost
    stocks<-stocks[order(stocks$Stock),]
    write.csv(stocks,"stocks.csv",row.names=FALSE)
    print(paste(quantity, "at", complete_info$Last))
    print(paste("cost of stock ", stock, ": ", cost))
    wallet<-wallet-cost
    write.csv(wallet,"wallet.csv",row.names=FALSE)
    
    stocks<-read.csv("stocks.csv")
    #upload()
    
  }
  else {
    stop("You don't have enough money for that.")
  }
}


# Buys stock with cash - i.e. find quantity of stocks such that quantity*price= cash then utilise buyStock above
buyStockCash <- function(stock,cash,date) {
  complete_info<-tq_get(stock,get="stock.prices",from=date-4,to=as.Date(date)+4)$close[1]
  print(paste("buySTockCash price at",complete_info))
  quantity<-cash/complete_info
  buyStock(stock,quantity,date)
}

# Sell stock at certain date
sellStock <- function(stock,quantity,date) {
  #download()
  stocks<-read.csv("stocks.csv")
  wallet<-read.csv("wallet.csv")
  index<-stocks[-1] # Offset dataframe so first column (stock names) is deleted
  row.names(index)<-stocks$Stock # Name the row names to stock names
  stockindex<-which(rownames(index)==stock) # Find the index of the selling stock
  if (quantity>stocks[stockindex,2]) {
    print("You don't have enough stocks for that!")
  }
  else {
    if (quantity==stocks[stockindex,2]){
      wallet<-wallet+(quantity*tq_get(stocks$Stock[stockindex], get="stock.prices",from=date-4,to=as.Date(date)+4)$close[1])*0.975
      stocks<-stocks[-c(stockindex),] # Remove entire row as we are selling all of the stock
    }
    else {
      print(paste("selling", quantity, "of stock", stock, "at date", date,"for",tq_get(stocks$Stock[stockindex], get="stock.prices",from=date,to=as.Date(date)+4)$close[1]))
      wallet<-wallet+(quantity*tq_get(stocks$Stock[stockindex], get="stock.prices",from=date-4,to=as.Date(date)+4)$close[1])*0.975 # transaction fee
      stocks$Quantity[stockindex]<-stocks$Quantity[stockindex]-quantity # Decrement quantity by amount sold
    }
    write.csv(stocks,"stocks.csv",row.names=FALSE)
    write.csv(wallet,"wallet.csv",row.names=FALSE)
    #upload()
  }
}

# Calcualte current portfolio value
portValue <- function(date=Sys.Date()) {
  current<-0
  stocks<-read.csv("stocks.csv")
  for (i in 1:nrow(stocks)){
    current<-current+(stocks$Quantity[i]*tq_get(stocks$Stock[i], get="stock.prices",from=as.Date(date)-4,to=as.Date(date)+4)$close[1])
  }
  return(current)
}
  
  
# Calculate portfolio cost
portCost <- function() {
  current<-0
  for (i in 1:nrow(stocks)){
    current<-current+(stocks$Quantity[i]*stocks$Price[i])
  }
  return(current)
}

# Calculate profit and show percentage over inital investment
profit <- function() {
  p<-portValue(input$date)-portCost()
  perc<-p/portCost()
  return(p)
}

# Calculate profit from individual stock
stockProfit <- function(stock,from=Sys.Date()) {
  profit<-round(
    getQuote(
      stock,src="yahoo",from=from,to=Sys.Date(),auto.assign=FALSE)$Last*stocks$Quantity-stocks$Price*stocks$Quantity,digits=2)
  return(profit)
}

# Calculate weights of stocks in portfolio
portWeights <- function(portfolio) {
  total<-0
  for (i in 1:nrow(stocks)) {
    total<-total+stocks$Quantity[i]*stocks$Price[i]
  }
    weight<-(stocks$Quantity*stocks$Price)/total
  weights<-data.frame(stocks$Stock,weight)
  return(weights)
}



# Scrape stock data from Yahoo Finance in order to get stock Beta
getStockBeta <- function(stock){

    x <- paste0("https://finance.yahoo.com/quote/", stock, "/key-statistics?p=", stock) %>% 
         read_html() %>% 
         html_table() %>% 
         map_df(bind_cols) %>% 
         t() %>%
         as_tibble()
     
    # Set first row as column names
    colnames(x) <- x[1,]
    # Remove first row
     x <- x[-1,]
     #Return numeric value of stocks 5Y monthly beta
   return(as.numeric(x$'Beta (5Y Monthly)'))
 }


# Calculate portfolio beta (weight of stock * stock beta for each stock)
portBeta <- function(portfolio) {
  beta<-0
  for (i in 1:nrow(stocks)) {
    beta<-beta+portWeights()[i,2]*getStockBeta(stocks$Stock[i])
  }
  return(beta)
}





# User interface ----
ui <- fluidPage(
  tabsetPanel(id="main",
  tabPanel("Stocks",value="stocks",
  titlePanel("UoN Portfolio Management"),
  h4(textOutput("wallet")),
  dateInput("date",label="Today's date",value=Sys.Date()),

  sidebarLayout(
      sidebarPanel(
      style="background: #e0e0e0;",
      h3("Examine a stock"),
      helpText("Information will be collected from Yahoo finance."),
      textInput("symb", "Stock", "AAPL"),

      dateRangeInput("dates",
                     "Date range",
                     start = "2013-01-01",
                     end = as.character(Sys.Date())),

      checkboxInput("log", "Plot y axis on log scale",
                    value = FALSE),
      br(),
      
      
      h3("Buy stocks"),
      helpText("Buy either a quantity of stocks or
               buy with a certain cash value."),
      textInput("stockinput",label=NULL,value=NULL,placeholder="Which stock?"),
      textOutput("stockprice"),
      br(),
      selectInput("select",NULL,choices=list("Buy quantity of stock"=1,"Buy cash value of stock"=2),selected=1),
      numericInput("quan",label=NULL,value=0,min=0),
      actionButton("buy",label="Buy",class="btn-success"),
      br(),
      
      
      
      h3("Sell stocks"),
      helpText("Sell stocks currently in your portfolio."),
      selectInput("sellstock",NULL,c(stocks[,1])),
      p(textOutput("sellquan")),
      numericInput("soldquan",label=NULL,value=0,min=0),
      p(textOutput("sellvalue")),
      actionButton("sell",label="Sell quantity",class="btn-warning"),actionButton("sellmax",label="Sell all",class="btn-danger")
    ),

    mainPanel(plotOutput("plot") %>% withSpinner(color="#adadad"),
              br(),
              br(),
              tabsetPanel(
                id="graph",
                tabPanel("Portfolio",br(),h3(textOutput("portvalue")),DT::dataTableOutput("table")     
                  
                  %>% withSpinner(color="#adadad")),
                tabPanel("Graph")
              ),
              br()
              
              )
  )
),
tabPanel("Beta",value="beta")
)
)








# Server logic
server <- function(input, output, session) {
  
  # Get current stock price
  stockPrice<-reactive({
    getQuote(input$stockinput,src="yahoo",from=input$date,to=input$date,auto.assign=FALSE)$Last
  })
  
  portfolioDatedValue<-reactive({
    portValue(input$date)
  })
  
  output$portvalue<-renderText({
    paste("Portfolio value: $",portValue(input$date))
  })
  
  # Output text displaying stock price
  output$stockprice<-renderText({
    req(input$stockinput)
    paste(input$stockinput, "is worth $",stockPrice())
  })
  
  
  # Generate P/L table
  output$table <- DT::renderDataTable({
    data<-cbind(stocks$Stock,
                stocks$Quantity,
                getQuote(stocks$Stock,src="yahoo",from=input$date,to=input$date,auto.assign=FALSE)$Last*stocks$Quantity,
                stockProfit(stocks$Stock),
                round(stockProfit(stocks$Stock)/portCost(),digits=2),
                stocks$Date)},
    colnames=c("Stock","Quantity","Value ($)","Profit ($)","Profit (%)","Date purchased"),
    style="bootstrap")
  

  
  # 
  # # Update contents of stock selection input
  # sellstocks<-reactiveValues(
  #   names=stocks$Stock
  # )
  # 
  # observe({
  #   updateSelectInput(session,"sellstock",NULL,names,names[1])
  # })
  # 
  # Output text containing the sale value of selected stocks
  output$sellvalue<-renderText({
    paste(input$soldquan, " stock(s) of ", input$sellstock, " are worth $",
          getQuote(input$sellstock,src="yahoo",from=Sys.Date(),to=Sys.Date(),auto.assign=FALSE)$Last*input$soldquan)
  })
  
  # Output text containing wallet balance
  output$wallet<-renderText({
    paste("Balance: $", round(wallet[1,1],digits=2))
  })
  
  # Output text displaying quantity of selected stocks
  output$sellquan<-renderText({
    paste("You currently own ", stocks[getStockIndex(input$sellstock),2], input$sellstock, " stock(s).")
  })
  
  # Reactive input for graphs
  dataInput <- reactive({
    getSymbols(input$symb, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
  # Display chart showing selected stock
  output$plot <- renderPlot({
  chartSeries(dataInput(),name="Stock Analyser", theme = chartTheme("white"), type = "line", log.scale = input$log, TA = c(addMACD()),plot=TRUE)
  })
  
  # Event when Buy button is clicked
  observeEvent(input$buy,{
    if(input$select==1){
      buyStock(input$stockinput,input$quan,input$date)
    }
    else {
      buyStockCash(input$stockinput,input$quan,input$date)
    }
  })
  
  # Event when Sell button is clicked
  observeEvent(input$sell,{
    sellStock(input$sellstock,input$soldquan)
  })
  
  # Event when Sell All button is clicked
  observeEvent(input$sellmax,{
    sellStock(input$sellstock,stocks[getStockIndex(input$sellstock),2])
  })
  
  
  # This is what doesn't work - trying to plot portfolio value over time, potentially even into the past

  portInput <- reactive({
    tickers<-stocks$Stock
    data<-new.env()
    getSymbols(tickers, src = 'yahoo', from = input$dates[1], to=input$dates[2], env = data)
  })
  output$portfolio <- renderPlot({
    tickers<-stocks$Stock
    data<-new.env()
    getSymbols(tickers, src = 'yahoo', from = input$dates[1], to=input$dates[2], env = data)
    chart_Series(data$AAPL)
  })
}

# Run the app
shinyApp(ui, server)
