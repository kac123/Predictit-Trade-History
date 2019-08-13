# set directory and read trade history file
setwd("C:/Users")
trade = read.delim("TradeHistory.csv",sep=",",header = T,fill = T,stringsAsFactors = F)

# reverse order of data so earliest records are first
trade = trade[dim(trade)[1]:1,]

# function to convert the dollar formats into numeric
dollar = function(col) {
  col2 = as.numeric(gsub('[$,()]', '', col))  
  col2 = as.numeric(lapply(col2, function(x) if (is.na(x)){x=0} else {x=x} )) 
  sign = as.numeric(grepl('\\(',col) > 0) * -1
  sign = as.numeric(lapply(sign, function(x) if (x==0){x = 1} else if (is.na(x)){x=0} else {x=x} ))
  return (col2 * sign)
}

# reformat the dates
trade$date = strptime(trade$DateExecuted, format = "%m/%d/%Y %I:%M %p")

# use the dollar function to find profit, fees, and net profit
trade$profit = dollar(trade$ProfitLoss)
trade$fee2 = dollar(trade$Fees)
trade$net = trade$profit + trade$fee2

# cumulative sum of net profit
trade$cumu = cumsum(trade$net)

# create YTD subsetted data
YTD = trade[trade$date >= '2019-01-01 00:00:00' & !is.na(trade$date),]

# plot overall and YTD  profits
par(mfrow=c(1,2))
plot(trade$date,trade$cumu, type = "o", cex = .4, main = paste0("Total Net: ",sum(trade$net)) )
abline(h = sum(trade$net), col = "red")
plot(YTD$date,YTD$cumu, type = "o",cex = .4
     ,main = paste0("YTD Net: ",sum(YTD$net),"\nLatest Date: ",substr(max(YTD$date),1,10)) )
abline(h = sum(trade$net), col = "red")
