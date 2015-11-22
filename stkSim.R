#This code requires the following two packages
library(stockPortfolio)
library(googleVis)
library(data.table)
library(reshape2)


#######################################################################
#   INPUTS
#   Load Stock Data from UI
#######################################################################

#Stocks for Portfolio?
symbols <- c("MSFT","C","BAC")

#Returns to use: day, week, month (0,1,2)
freq = 0

if( freq == 0 ){
  freq = "day"
}else if( freq == 1){
  freq = "week"
}else {
  freq = "month"
}

#Historical returns going back how many years?
yrs <- 3


#######################################################################
#   Run script
#######################################################################

#getReturns calculates daily returns starting from Jan 1st 2010
#stores them into stockReturns
raw.all <- getReturns(ticker=symbols,freq=freq,start=Sys.Date()-365*yrs)
raw.full <- lapply(raw.all$full,data.table)


#######################################################################
#   Some Initial Outputs - VOLUME and STOCK PROCE
#######################################################################

#1. Get Data.Tables from output 's'
#2. Only keep the date and the specified 'var' (volume or close price)
#3. Reduce and merge to arrive at Volume and Close data.tables

get_var <- function(y){
  
  f <- function(x,var){
    dt <- data.table(raw.all$full[[x]])[,.(Date,get(var))]
    setnames(dt,"V2",x)
  }
  
  ll <- sapply(symbols, f, y, simplify = F)
  ll <- Reduce(function(...) merge(...,by = "Date"), ll)
  ll[,Date:=as.Date(Date)]
  setkeyv(ll, "Date")
}

res <- sapply(c("Volume","Close"), get_var, simplify = F)

#######################################################################
#   CALENDAR CHARTS - VOLUME 
#######################################################################

calopts <- "{yearLabel:{fontName: 'Times-Roman',
                        fontSize: 32, color: '#1A8763', bold: true},
                        cellSize: 10,
                        focusedCellColor: {stroke:'red'}}"

Cal <- lapply(symbols,function(x){ 
  output <- gvisCalendar(res$Volume[,.(Date,get(x))], 
                         datevar="Date", 
                         options=list(calendar = calopts))})

Cal[[3]]$html$caption=NULL
Cal[[3]]$html$footer="</body></html>"
plot(Cal[[3]])


#######################################################################
#  LINE CHART - OPEN/CLOSE & HIGH/LOW
#######################################################################

s <- sapply(symbols,function(x) 
  data.table(raw.all$full[[x]])[,.(Date, Low, Open, Close, High)],
  simplify = F)

msft <- s[[1]]
monthAve <- msft[,lapply(.SD,mean),by=month(Date)]
monthAve[,(names(monthAve)):=lapply(.SD,round,2)]

setkey(monthAve,month)
monthAve[,month:=month.name[month]]

line <- gvisLineChart(monthAve,
                      options=list(title="MSFT",
                                   titleTextStyle="{color:'red', 
                                   fontName:'Courier', 
                                   fontSize:16}",
                                   curveType="function",
                                   width=600,
                                   height=300))
line$html$caption = NULL
line$html$footer = NULL
plot(line)

#######################################################################
#   BUBBLE CHARTS
#######################################################################

dt.all <- rbindlist(lapply(1:length(symbols), function(x){
  dt <- data.table(raw.full[[x]],keep.rownames=T)
  
  #set column for ticker name
  dt[,Ticker:=names(raw.full[x])]
  
  #Measure of daily volatility 
  dt[,Volatility:=(High-Low)/Open]
  
  #Standardized monthly volume by ticker, relative measure
  dt[,Volume.Yr:=Volume/max(Volume),by=.(Ticker,year(Date))]
  
  #Standardized monthly volume by ticker, relative measure
  dt[,Volume:=Volume/1000000]
  
  #round only the numeric columns 
  nums = which(sapply(dt, is.numeric))
  dt[,nums:=lapply(.SD,round,3),.SDcols = nums,with=F]

}))


dt.returns <- melt(data.table(raw.all$R,keep.rownames = T),
                   id.vars="rn",
                   value.name="Return",
                   variable.name="Ticker")
dt.returns[,Return:=round(Return,4)]
setnames(dt.returns,"rn","Date")

#set keys to join tables
setkey(dt.all,Ticker,Date)
setkey(dt.returns,Ticker,Date)

#Join the data.tables ignoring NAs that occur when calculating returns
dt <- dt.returns[dt.all,nomatch=0]

#get daily means by month and year and plot yearly
dt.mon <- dt[,lapply(.SD,function(x)round(mean(x),3)),by=.(month.abb(Date),Ticker)]
dt.yr <- dt[,lapply(.SD,mean),by=.(year(Date),Ticker)]

Bubble <- gvisBubbleChart(dt.yr,
                          idvar="Ticker", 
                          xvar="Return",
                          yvar="Volatility",
                          colorvar="year",
                          sizevar="Volume.Yr",
                          options=list(gvis.editor="Edit",
                                       width=700,
                                       height=400,
                                       vAxis="{title:'Averaged Daily Volatility',
                                              format:'#.###%'}",
                                       hAxis="{title:'Averaged Daily Returns',
                                              format:'#.###%'}"))
plot(Bubble)

#######################################################################
#   PORTFOLIO ANALYSIS
#######################################################################

aveReturns <- stockReturns$ticker

#get historical return and risk of each stock 
stockProfile <- stockModel(stockReturns)

opPort <- optimalPort(stockProfile)

#portReturns, takes my model and my weights and calculates mu and sigma
portReturns <- portReturn(model,opPort$X)

mu <- portReturns$R
sig <- sqrt(portReturns$V)

n <- 50

#Simulate random stock price movement using mu and sigma
h <- 1/260
s0 <- 1
s <- matrix(0,261,n)
s[1,] <- s0

for(j in 2:261){
  s[j,] <- s[j-1,]*exp((mu-.5*sig^2)*h+sig*rnorm(n)*sqrt(h))
}

#Vector of days used to label my chart
days <- seq(Sys.Date(), by=1, len=261)

#create a dataframe with days as one column
colnames(s) <- symbols
simulated <- data.frame(days=days,s)

#store the dataframe I just created with info for the legend of my chart, and a size option
Line <- gvisLineChart(simulated,xvar = "days",
                      options=list(width=1200, height=600,vAxis="{format:'#,###%'}"))

# Display chart
#plot(Line)
htmlcode <- unlist(Line$html)

#Create Google Gadget
#cat(createGoogleGadget(Motion), file="motionchart.xml")