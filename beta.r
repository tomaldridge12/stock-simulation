# Load libraries 
library(shiny)
library(tidyquant)
library(tidyverse)
library(rdrop2)
library(shinycssloaders)
library(DT)
library(XML)
library(fpp2)
library(rvest)
library(shinybusy)
library(timetk)

# Load functions from main ----
source("app.r")

# Functions ----

# Get covariance between filtered stocks up to a certain point
# Specify: stock= list of stock names (stocks$Stock) and date of purchase (specified in UI)
getCovariance<-function(stock,enddate) {
  price_data <- suppressWarnings(tq_get(stock,from = "2012-12-31",to = as.Date(enddate),get = "stock.prices")) ## Gathering the stock prices 
  
  log_ret_tidy <- price_data %>% ## Calculating the daily returns using logarithmic returns 
    group_by(symbol) %>%
    tq_transmute(select = adjusted,
                 mutate_fun = periodReturn,
                 period = 'daily',
                 col_rename = 'ret',
                 type = 'log')
  
  log_ret_xts <- log_ret_tidy %>% ## Converting the tidy format into a wide format time series 
    spread(symbol, value = ret) %>%
    tk_xts()
  
  good <- complete.cases(log_ret_xts) ## Cleans the data set, removing any NA values
  log_ret_xts <- log_ret_xts[good,]
  mean_ret <- colMeans(log_ret_xts) ## Mean daily returns for each asset 
  cov_mat <- cov(log_ret_xts) * 252 ## Covariance Matrix for all these stocks yearly
  newList<-list("log_ret_xts"=log_ret_xts,"mean_ret"=(round(mean_ret, 5)),"cov_mat"=round(cov_mat,4))
  return(newList)
}

# Using the covariance table generated above, find stocks with minimum covariance
minCovStocks<-function(stock,quantity,date){
  matrix<-getCovariance(stock,date)$cov_mat
  diag(matrix)<-100 # As we are finding min cov, set diagonal elements to large number so these are not selected (setting=NA/NuLL caused errors)
  filtered<-data.frame("Stock"=NULL,"Beta"=NULL)
  for (i in 1:quantity){ # Quan of stocks to select
    loc<-which(matrix==min(matrix),arr.ind = TRUE) # Find location of min cov
    names<-row.names(loc) # Find names of stocks with min cov
    new<-data.frame("Stock"=c(names[1],names[2]),"Beta"=c(beta[which(beta==names[1]),2],beta[which(beta==names[2]),2])) # Store this in df
    filtered<-rbind(filtered,new) 
    matrix[loc]<-100 # Set value to large value so it cant be counted again
    if (length(filtered)>=quantity) break
  }
  filtered<-filtered[!duplicated(filtered),] # Remove any duplicate data (each request pulls 2 stocks)
  return(filtered)
}

# Generate num_port randomly weighted portfolios in order to select the best portfolio
calculatePortWeights<-function(port,num_port,date){
  list<-getCovariance(port$Stock,date)
  
  tick<-port$Stock
  all_wts<-matrix(nrow=num_port,
                  ncol=length(tick))
  port_returns <- vector('numeric', length = num_port)
  port_risk <- vector('numeric', length = num_port)
  sharpe_ratio <- vector('numeric', length = num_port)
  
  for (i in 1:num_port){
    wts<-runif(length(tick))
    wts<-wts/sum(wts)
    all_wts[i,]<-wts
    
    port_ret<-sum(wts*list$mean_ret)
    port_ret<-((port_ret+1)^252)-1 # adjust for different interval lenghts
    port_returns[i]<-port_ret
    
    port_sd<-sqrt(t(wts) %*% (list$cov_mat %*% wts))
    port_risk[i]<-port_sd
    
    sr<-port_ret/port_sd
    sharpe_ratio[i]<-sr
  }
  portfolio_values<-tibble(Return=port_returns,
                           Risk=port_risk,
                           SharpeRatio=sharpe_ratio)
  all_wts<-tk_tbl(all_wts)
  colnames(all_wts)<-colnames(list$log_ret_xts)
  portfolio_values<-tk_tbl(cbind(all_wts,portfolio_values))
  return(portfolio_values)
}


# Forecast portfolio from current date to 6months time
forecastPortfolio<-function(portfolio,date){
  forecast<-data.frame("Stock"=NULL,"Low 80"=NULL,"Low 95"=NULL)
  for (i in 1:nrow(portfolio)){
    print(portfolio$Stock[i])
    stock<-getSymbols(portfolio$Stock[i],to=date,env=NULL)
    fc<-forecast::forecast(auto.arima(ts(stock[,6])),h=126)
    new<-data.frame("Stock"=portfolio$Stock[i],"Low 80"=fc$lower[126,1],"Low 95"=fc$lower[126,2])
    forecast<-rbind(forecast,new)
  }
  forecast<-forecast[order(forecast$Stock),] # organise forecast alphabetically just in case
  write.csv(forecast,"forecast.csv",row.names=FALSE)
  drop_upload("forecast.csv")
}


# Compare current prices of stocks to our forecast from 6 months ago, and remove them if they fall outside of the range
#save stocks after each iteration
compareForecast<-function(portfolio,date){
  fc<-read.csv("forecast.csv")
  for (i in 1:nrow(portfolio)){
    stocks<-read.csv("stocks.csv")
    price<-tq_get(portfolio[i,1],get="stock.prices",from=date-4,to=as.Date(date)+4)$close[1]
    print(paste(portfolio[i,1],"at",price,"with forecast",fc[i,3]))
    if (price<fc[i,3]){ # current price below forecasted range
      cb<-subset(beta,beta$Stock==portfolio[i,1])$Beta # get beta of current stock
      ns<-subset(beta,beta$Beta<=cb+0.01&beta$Beta>=cb-0.01) # find stocks with similar betas
      ns<-ns[grep(paste(stocks$Stock,collapse = "|"),ns$Stock,invert=TRUE),]
      covvalue<-vector('numeric',length=nrow(ns))# remove all stocks that are included in current portfolio from filtered stocks
      for (j in 1:nrow(ns)){
        cov_mat<-getCovariance(c(ns[j,1],portfolio$Stock),date)$cov_mat # generate covariance matrix for new stock with current portfolio
        diag(cov_mat)<-NA
        covvalue[i]<-sum(cov_mat[which(row.names(cov_mat)==ns[j,1]),],na.rm=TRUE) # sum row of cov matrix with new stock in
      }
      print(covvalue)
      add_stock<-ns[which.min(covvalue),] # stock to add is that with minimum covariance with rest of portfolio
      # remove old stock from portfolio and buy equal amount of new stock
      print(add_stock)
      quan<-price*portfolio[i,2]*0.975
      print(portfolio)
      print(paste("trying to sell",portfolio[i,2],"of",portfolio[i,1]))
      sellStock(portfolio[i,1],portfolio[i,2],date)
      buyStockCash(add_stock[1,1],quan,date)
      # update port_info
      info<-read.csv("port_info.csv")
      print(info)
      weight<-portWeights(portfolio)
      colnames(info)[which(colnames(info)==portfolio[i,1])]<-add_stock[1,1]
      print(info)
      info[1,add_stock[1,1]]<-weight[which(weight==portfolio[i,1]),2]
      print(info)# update weights inside port_info.csv
      write.csv(info,"port_info.csv",row.names=FALSE)
      drop_upload("port_info.csv")
      
    }
    else{
      print("Price within range")
    }
    }
}


# Descriptive outline of what the below function performs:
# When the adjust portfolio button is clicked, a new set of random portfolios are generated using the stocks currently in the portfolio.
# We then read in the values from the old portfolio, stored in port_info.csv as old_port. Depending on the type of portfolio we initially
# invested in, we will perform a different action. If we invested in minimum variance, then we will get the new minimum variance portfolio
# and check the level of risk. If the risk level is higher than before, we do not adjust. If it is lower, then we calculate the difference
# in the weights between the new portfolio and the old one. We then multiply this by the value of our portfolio to see how much we should buy
# or sell in order to balance the portfolio as required. We then calculate the cost of performing this action, and also the difference in 
# returns that we will make from switching to the new portfolio. If the cost of switching is higher than the increase in returns that we experience,
# the operation does not go ahead. If we will achieve greater returns as well as incurring a lower risk, then we will sell or buy stocks accordingly.
# Similar procedures are ran for max returns and max sharpe ratio.


adjustPortfolio<-function(portfolio,date){
  compareForecast(portfolio,date)
  adjust_port<-calculatePortWeights(portfolio,20000,date)
  print(paste("portfolio contains",portfolio$Stock))
  # calculate new random portfolios at date using stocks form current portfolio
  old_port<-read.csv("port_info.csv") # get info of current portfolio
  old_port.type<-old_port[1,1] # get type of portfolio invested in (max ret, min var, max sr, user)
  min_var <- adjust_port[which.min(adjust_port$Risk),]
  max_sr <- adjust_port[which.max(adjust_port$SharpeRatio),]
  max_ret <- adjust_port[which.max(adjust_port$Return),] # get new max ret, min var, max sr portfolios from new generated ones
  print(paste("current portfolio is",old_port.type))
  print(paste("new risk is ",min_var$Risk,"old risk is",old_port$Risk))
  print(paste("new ret is ",min_var$Return,"old ret is",old_port$Return))
  
  if (old_port.type=="MinVar"){
    print("found port type")
    if (min_var$Risk<=old_port$Risk){
      print("less risky")
      # as user invested in min risk, we dont want to change portfolio if new min risk is still higher risk than old
      # calculate difference in port weights
      old_port<-old_port[-1]# remove type of portfolio from old values
      total_returns<-(min_var$Return-old_port$Return)*portValue() # estimated change in returns due to new portfolio
      old_port<-old_port[1:(length(old_port)-3)] # remove risk, return and sharpe columns
      min_var<-min_var[1:(length(min_var)-3)]
      old_port<-old_port[,order(names(old_port))]# sort alphabetically, same as how calculatePortWeights returns min_var
      weight_diff<-old_port-min_var # calculate difference in weights between old and new
      sellorbuy<-portValue()*weight_diff # calculate value of stock to sell or buy by multiplying difference in weight by port value
      # calculate return of portfolio after 6 months, compare to new returns and see if its worth after transac fee
      total_cost<-sum(sellorbuy[1:nrow(portfolio)])*0.025 # add all of the amounts we're changing then multiply by transac fee to see total shift cost
     
      
      if (total_returns>=total_cost){ # only perform the exchange if the change in returns is greater than the cost of switching
        info<-data.frame("Type"="MinVar")
        print("its worth it")
        port_info<-cbind(info,min_var)
        write.csv(port_info,"port_info.csv",row.names=FALSE) # store information about portfolio invested in
        drop_upload("port_info.csv")
        for (i in 1:nrow(portfolio)){
          if (weight_diff[i]>0){ # >0 so we must buy more of this stock
            print(paste("trying to buy",sellorbuy[1,i],"of stock",portfolio[i,1]))
            buyStockCash(portfolio[i,1],sellorbuy[1,i],date)
          }
          else{ # <0 so we must sell stock
            print(paste("trying to sell",sellorbuy[1,i],"of stock",portfolio[i,1]))
            sellStock(portfolio[i,1],abs(sellorbuy[1,i])/tq_get(stocks$Stock[stockindex], get="stock.prices",from=date,to=as.Date(date)+4)$close[1],date)
          }
        }
      }
      
    }
    else {print("Risk is higher. No change")} # as risk is higher than before, dont change
  }
  if (old_port.type=="MaxRet"){
    if (max_ret$Return>old_port$Return){
      # calculate difference in port weights
      old_port<-old_port[-1]# remove type of portfolio from old values
      old_port<-old_port[1:(length(old_port)-3)]# remove risk, return and sharpe columns
      old_port<-old_port[,order(names(old_port))]# sort alphabetically, same as how calculatePortWeights returns min_var
      max_ret<-min_var[1:(length(max_ret)-3)] # calculate difference in weights between old and new
      sellorbuy<-portValue()*weight_diff # calculate amount of stock to sell or buy by multiplying difference in weight by port value
      # calculate return of portfolio after 6 months, compare to new returns and see if its worth after transac fee
      total_cost<-sum(sellorbuy[1:nrow(portfolio)])*0.025 # add all of the amounts we're changing then multiply by transac fee to see total shift cost
      total_returns<-(max_ret$Return-old_port$Return)*portValue() # estimated change in returns due to new portfolio
      
      if (total_returns>=total_cost){ # only perform the exchange if the change in returns is greater than the cost of switching
        info<-data.frame("Type"="MaxRet")
        port_info<-cbind(info,max_ret)
        write.csv(port_info,"port_info.csv",row.names=FALSE) # store information about portfolio invested in
        drop_upload("port_info.csv")
        for (i in 1:nrow(portfolio)){
          if (weight_diff>0){ # >0 so we must buy more of this stock
            buyStock(portfolio[i,1],sellorbuy[1,i],date)
          }
          else{ # <0 so we must sell stock
            sellStock(portfolio[i,1],abs(sellorbuy[1,i]),date)
          }
        }
    }
    else {print("Returns are lower. no change")}
    }
  }
  if (old_port.type=="MaxSR"){
    if (max_sr$SharpeRatio>old_port$SharpeRatio){
      # calculate difference in port weights
      old_port<-old_port[-1]# remove type of portfolio from old values
      old_port<-old_port[1:(length(old_port)-3)] # remove risk, return and sharpe columns
      old_port<-old_port[,order(names(old_port))]# sort alphabetically, same as how calculatePortWeights returns min_var
      max_sr<-min_var[1:(length(max_sr)-3)] # remove type of portfolio from old values
      weight_diff<-old_port-max_sr # calculate difference in weights between old and new
      sellorbuy<-portValue()*weight_diff # calculate amount of stock to sell or buy by multiplying difference in weight by port value
      # calculate return of portfolio after 6 months, compare to new returns and see if its worth after transac fee
      total_cost<-sum(sellorbuy[1:nrow(portfolio)])*0.025 # add all of the amounts we're changing then multiply by transac fee to see total shift cost
      total_returns<-(max_sr$Return-old_port$Return)*portValue() # estimated change in returns due to new portfolio
      
      if (total_returns>=total_cost){
        info<-data.frame("Type"="MaxSR")
        port_info<-cbind(info,max_sr)
        write.csv(port_info,"port_info.csv",row.names=FALSE) # store information about portfolio invested in
        drop_upload("port_info.csv")# only perform the exchange if the change in returns is greater than the cost of switching
        for (i in 1:nrow(portfolio)){
          if (weight_diff>0){ # >0 so we must buy more of this stock
            buyStock(portfolio[i,1],sellorbuy[1,i],date)
          }
          else{ # <0 so we must sell stock
            sellStock(portfolio[i,1],abs(sellorbuy[1,i]),date)
          }
        }
    }
    else{print("ratio is lower. No change")}
  }
  }
  
  
}


# Main ----
stocks<-read.csv("stocks.csv")
wallet<-read.csv("wallet.csv")
SP500<-tq_index("SP500")$symbol

# Generate list of betas for S&P500 if the beta file does not exist
if (!file.exists("beta.csv")){
  beta<-data.frame(matrix(ncol=2,nrow=0))
  colnames(beta)<-c("Stock","Beta")
  
  for (i in 1:length(SP500)) {
    new<-data.frame("Stock"=as.character(SP500[i]),"Beta"=getStockBeta(SP500[i]))
    beta<-rbind(beta,new)
    #beta <- beta[!apply(is.na(beta) | beta == "", 1, all),] # Clears all empty rows
  }
} else {
  beta<-read.csv("beta.csV")
}

beta_f<-beta




# UI ----
ui <- fluidPage(
  tags$head(tags$style(".rightAlign{float:right;}")),
  titlePanel("UoN Portfolio Management"),actionButton("reset","Reset All",class="btn-danger rightAlign"),actionButton("adjust","Adjust Portfolio",class="btn-warning rightAlign"),dateInput("date",label="Today's date",value=Sys.Date()),
  h4(textOutput("wallet")),
  
  sidebarLayout(
    sidebarPanel(
      style="background: #e0e0e0;",
      h3("Portfolio Selection"),
      helpText("Select the range of betas your portfolio will include."),
      
      sliderInput("betarange",
                     NULL,
                     min = 0.85,
                     max = 1.5,
                     value=c(0.9,1.1),
                     step=0.01),
      
      actionButton("filter","Filter",class="btn-info"),
      br(),
      
      h3("Filter Selected by Covariance"),
      helpText("Filter the selected stocks above by those with the least covariance with each other."),
      numericInput("quan",label="Quantity of stocks to select",value=2,min=2),
      actionButton("filter2","Filter",class="btn-info"),
      br(),
      numericInput("quan2",label="Number of portfolios to generate",value=5000,min=1),
      actionButton("generate","Generate portfolios",class="btn-warning"),
      br(),
      
      h3("Select Portfolio"),
      helpText("Select which portfolio you wish to invest in."),
      selectInput("portselect",NULL,choices=list("Minimum Variance"=1,"Maximum Sharpe Ratio"=2,"Maximum Returns"=3,"User Selected (Click on graph)"=4),selected=1),
      numericInput("investquan","Quantity to invest",value=5000000,min=0,max=wallet[1,1]),
      actionButton("invest","Invest",class="btn-success"),
      
      
      
      width=4),
    
    mainPanel(
      h4("Filtered Beta Stocks"),
      DT::dataTableOutput("betatable") %>% withSpinner(color="#adadad"),
      h4("Minimum Covariance Stocks"),
      DT::dataTableOutput("covtable") %>% withSpinner(color="#adadad"),
      h4("Efficient Frontier Plot"),
      plotOutput("plot",click="plot_click") %>% withSpinner(color="#adadad"),
      DT::dataTableOutput("point")
      # Ask which portfolio to use - minvar, sharpe or user
              
    )
  )
)







# Server logic ----
server <- function(input, output, session) {
  
  rs<-reactiveValues()
  cov<-reactiveValues()# set up class of reactive dataframes
  rs$beta<-beta # Assign values
  
  
  # Event when Filter betas is clicked
  observeEvent(input$filter,{
    beta_f<-subset(beta,beta$Beta>=input$betarange[1]&beta$Beta<=input$betarange[2])
    rs$beta<-beta_f
    rs$stocks<-beta_f$Stock
  })
  
  # Event when calculate covariances is clicked
  observeEvent(input$filter2,{
    show_modal_spinner(text="Please wait... generating covariances...")
    cov$table<-minCovStocks(rs$stocks,input$quan,input$date)
    remove_modal_spinner()
  })
  
  # Event when Reset Portfolio is clicked - resets all stocks in stock file, sets wallet value to 100000000
  observeEvent(input$reset,{
    show_modal_spinner(text="Please wait... resetting portfolio...")
    resetAll()
    remove_modal_spinner()
  })
  
  
  # Covariance matrix table
  output$covtable<-DT::renderDataTable({
    data<-cbind(cov$table)},
    colnames=c("Stock","Beta"),
    style="bootstrap")
  
  
  # Filtered beta table
  output$betatable <- DT::renderDataTable({
    data<-cbind(rs$beta)},
    colnames=c("Stock","Beta"),
    style="bootstrap",
    options=list(pageLength=5))
  
  # Adjust portfolio when Adjust button is clicked
  observeEvent(input$adjust,{
    show_modal_spinner(text="Adjusting portfolio... please wait...")
    stocks<-read.csv("stocks.csv")
    adjustPortfolio(stocks,input$date)
    stocks<-read.csv("stocks.csv")
    forecastPortfolio(stocks,input$date)
    remove_modal_spinner()
  })
  
  # Generate portfolio frontier plot when clicking on Generate button
  observeEvent(input$generate,{
  output$plot<-renderPlot({
    portfolio_values<-calculatePortWeights(cov$table,input$quan2,input$date)
    min_var <- portfolio_values[which.min(portfolio_values$Risk),]
    ## Highest sharpe ratio
    max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]
    portfolio_values %>%
      ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
      geom_point() +
      theme_classic() +
      scale_y_continuous(labels = scales::percent) +
      scale_x_continuous(labels = scales::percent) +
      labs(x = 'Annualized Risk',
           y = 'Annualized Returns',
           title = "Portfolio Optimization & Efficient Frontier") +
      geom_point(aes(x = Risk,
                     y = Return), data = min_var, color = 'red') +
      geom_point(aes(x = Risk,
                     y = Return), data = max_sr, color = 'red')
    
  })})
  
  # Output what portfolio corresponds to the clicked area on the graph
  output$point<-DT::renderDataTable({
    nearPoints(calculatePortWeights(cov$table,input$quan2,input$date),input$plot_click,threshold=15)
  },style="bootstrap")
  
  # INVEST THA CASH
  
  observeEvent(input$invest,{
    show_modal_spinner(text="Please wait... investing...")
    
    portfolio_values<-calculatePortWeights(cov$table,input$quan2,input$date)
    min_var <- portfolio_values[which.min(portfolio_values$Risk),]
    max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]
    max_ret <- portfolio_values[which.max(portfolio_values$Return),]
    
    n=length(portfolio_values)-3 # number of stocks in portfolio (3 dead rows at the end not needed)
  
    
    
    if (input$portselect==1){ # minvar portfolio
      info<-data.frame("Type"="MinVar")
      port_info<-cbind(info,min_var)
      write.csv(port_info,"port_info.csv",row.names=FALSE) # store information about portfolio invested in
      drop_upload("port_info.csv")
      for (i in 1:n){
        print(paste(input$investquan*min_var[1,i],"worth of",colnames(min_var)[i]))
        buyStockCash(colnames(min_var)[i],input$investquan*min_var[1,i],input$date)
      }
      
    }
    else if (input$portselect==2){ # max sharpe ratio
      info<-data.frame("Type"="MaxSR")
      port_info<-cbind(info,max_sr)
      write.csv(port_info,"port_info.csv",row.names=FALSE) # store information about portfolio invested in
      drop_upload("port_info.csv")
      for (i in 1:n){
        buyStockCash(colnames(max_sr)[i],input$investquan*max_sr[1,i],input$date)
      }
    }
    else if (input$portselect==3){ # max returns
      info<-data.frame("Type"="MaxRet")
      port_info<-cbind(info,max_ret)
      write.csv(port_info,"port_info.csv",row.names=FALSE) # store information about portfolio invested in
      drop_upload("port_info.csv")
      for (i in 1:n){
        buyStockCash(colnames(max_ret)[i],input$investquan*max_ret[1,i],input$date)
      }
    }
    else { # user selected port
      show_modal_spinner(text="Work in progress.")
      Sys.sleep(4)
      remove_modal_spinner()
    }
    stocks<-read.csv("stocks.csv")
    forecastPortfolio(stocks,input$date)
    remove_modal_spinner()
  })
  
  
}

# Run the app
shinyApp(ui, server)

  

