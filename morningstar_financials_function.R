#Function to scrape morningstar data
#Library 
# list.of.packages <- c("reshape2")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

library("reshape2")

scrape_morningstar = function(ticker){

stocks <- c(ticker)
urls <- sprintf("http://financials.morningstar.com/ajax/exportKR2CSV.html?&t=%s", stocks)
data = read.csv(urls,stringsAsFactors = FALSE, skip = 2)
# data <- lapply(urls, read.csv, header = TRUE, stringsAsFactors = FALSE, skip = 2)
# data = data[[1]]



#Financials row, 
#Key Ratios -> Profitability row, 
#Profitability row, 
#Key Ratios -> Growth row, 
#Key Ratios -> Cash Flow row, 
#Key Ratios -> Financial Health row, 
#Liquidity/Financial Health row

# financials_index = which(data[,1] == "Financials")
ratio_profit_index = which(data[,1] == "Key Ratios -> Profitability")
profit_index = which(data[,1] == "Profitability")
ratio_growth_index = which(data[,1] == "Key Ratios -> Growth")
cf_index = which(data[,1] == "Key Ratios -> Cash Flow")
fin_health_index = which(data[,1] == "Key Ratios -> Financial Health")
liq_fin_index = which(data[,1] == "Liquidity/Financial Health")
eff_index = which(data[,1] == "Key Ratios -> Efficiency Ratios")

data$fin_indicator_type = ""

data$fin_indicator_type[1:nrow(data)] = "Financials"
data$fin_indicator_type[ratio_profit_index:nrow(data)] = "ratio_profitability"
data$fin_indicator_type[profit_index:nrow(data)] = "profitability"
data$fin_indicator_type[ratio_growth_index:nrow(data)] = "ratio_growth"
data$fin_indicator_type[cf_index:nrow(data)] = "cash_flow"
data$fin_indicator_type[fin_health_index:nrow(data)] = "ratio_fin_health"
data$fin_indicator_type[liq_fin_index:nrow(data)] = "liq_fin_health"
data$fin_indicator_type[eff_index:nrow(data)] = "efficiency"

# names(data)[1:(ncol(data)-1)] = data[2,1:(ncol(data)-1)]
# names(data)[2:(ncol(data)-2)] = paste("X",names(data)[2:(ncol(data)-2)],sep = "")
names(data)[1] = "fin_indicator"
names(data)[(ncol(data)-1)] = "TTM_or_Latest Qtr"

#Assign growth fin indicators with year average
# Revenue %,Operating Income %,Net Income %,EPS %
#append revenue_perc
rev_index  = which(data[,1] == "Revenue %")
op_y_index  = which(data[,1] == "Operating Income %")
net_y_index  = which(data[,1] == "Net Income %")
eps_index  = which(data[,1] == "EPS %")

data$fin_indicator[(rev_index+1):(op_y_index-1)] = paste("rev_perc_",(data$fin_indicator[(rev_index+1):(op_y_index-1)]),sep = "")
data$fin_indicator[(op_y_index+1):(net_y_index-1)] = paste("op_y_perc_",(data$fin_indicator[(op_y_index+1):(net_y_index-1)]),sep = "")
data$fin_indicator[(net_y_index+1):(eps_index-1)] = paste("net_y_perc_",(data$fin_indicator[(net_y_index+1):(eps_index-1)]),sep = "")
data$fin_indicator[(eps_index+1):(cf_index-2)] = paste("eps_perc_",(data$fin_indicator[(eps_index+1):(cf_index-2)]),sep = "")


#Subset out blank space
data = subset(data,data$fin_indicator != "" & 
                              data$fin_indicator != "Financials"&
                              data$fin_indicator != "Key Ratios -> Profitability"&
                              data$fin_indicator != "Profitability"&
                              data$fin_indicator != "Key Ratios -> Growth"&
                              data$fin_indicator != "Key Ratios -> Cash Flow"&
                              data$fin_indicator != "Key Ratios -> Financial Health" & 
                              data$fin_indicator != "Liquidity/Financial Health" & 
                              data$fin_indicator != "Key Ratios -> Efficiency Ratios" &
                              data$fin_indicator != "Key Ratios -> Efficiency Ratios" &
                              data$fin_indicator != "Cash Flow Ratios" &
                              data$fin_indicator != "Balance Sheet Items (in %)" &
                              data$fin_indicator != "Revenue %" & 
                              data$fin_indicator != "Operating Income %" &
                              data$fin_indicator != "Net Income %" & 
                              data$fin_indicator != "EPS %" &
                              data$fin_indicator != "Efficiency" &
                              data$fin_indicator != "Margins % of Sales")

#Melt to long dataframe
# for(i in 2:(ncol(data)-1)){
#   data[,i] = as.numeric(data[,i])
# }
#efficient way to convert to as.numeric
data[,2:(ncol(data)-1)] <- sapply(data[,2:(ncol(data)-1)],as.numeric)



data_long = melt(data,id = c(which(names(data) == "fin_indicator"),
                             which(names(data) == "fin_indicator_type")))

data_long$ticker = ticker
  
return(data_long)

}


# fb = scrape_morningstar("FB")




