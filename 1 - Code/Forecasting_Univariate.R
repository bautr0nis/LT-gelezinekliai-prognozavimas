library(dplyr)
TIME <- Sys.time()
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")}
require(pacman)
if (!"forecastxgb" %in% rownames(installed.packages())) {
  pacman::p_load(devtools)
  devtools::install_github("ellisp/forecastxgb-r-package/pkg")
  #pacman::p_load(githubinstall)
  #githubinstall("forecastxgb-r-package",ask=F)
}
pacman::p_load(forecastxgb)
pacman::p_load(forecast, thief, xts)
pacman::p_load(prophet, forecastHybrid)

# wrapper around Prophet forecasting model
forecastProphet <- function(ts_temp=NULL,level=80,h=7,fan=FALSE,myfreq="month") {
  ts_obs <- as.Date(time(ts_temp))
  if (fan) level <- seq(from=51,to=99,by=3)
  fc_temp <- meanf(ts_temp,h=h,level=level)
  fc_temp$method <- "Prophet"
  fc_temp$level <- level  
  for (i in seq(1,length(level))) {
    log <- capture.output(my_model <- prophet(data.frame(ds=ts_obs,y=ts_temp),interval.width = level[i]/100, yearly.seasonality=T, weekly.seasonality=F, daily.seasonality=F))
    future <- make_future_dataframe(my_model, periods = fh, freq=myfreq)
    fc_prophet <- predict(my_model,future)
    myidx <- seq(length(fc_prophet$yhat)-fh+1,length(fc_prophet$yhat))
    fc_temp$mean[1:length(fc_temp$mean)] <- fc_prophet$yhat[myidx]
    fc_temp$fitted[1:length(fc_temp$fitted)] <- fc_prophet$yhat[1:(length(fc_prophet$yhat)-fh)]
    if (length(level) > 1) {
      fc_temp$lower[,i] <- fc_prophet$yhat_lower[myidx]
      fc_temp$upper[,i] <- fc_prophet$yhat_upper[myidx]
    } else {
      fc_temp$lower <- fc_prophet$yhat_lower[myidx]
      fc_temp$upper <- fc_prophet$yhat_upper[myidx]
    }
  }
  return(fc_temp)
}

data_fold <- './'
data_file <- 'Y1'
data_type <- 'month'

#setwd(dirname(sys.frame(1)$ofile)) # this line does not work in Mac (OS X)
my_data <- read.csv(paste(data_fold,data_file,'.csv',sep=""))


ylabel <- colnames(my_data)[3] # 'kroviniu_kiekis'
xlabel <- sprintf("Time (%s)",data_type)
data_freq <- switch(data_type,"week" = 52, "month" = 12, "quarter" = 4, 0)

# set forecasting horizon
fh <- switch(data_type,"week" = 28, "month" = 12, "quarter" = 8, 0)
fw <- 2 * fh # (data_freq * 2) # historical data to show

# set interval forecast limits
interval_forecast <- 'level=c(80,90)' # level=c(80,90) or fan=TRUE ?'

sdN <- 3 # y limits in standard deviations above/below the mean
dP <- 2 # digits after floating point (for errors in table/fig)

# function to estimate grid lines in forecast plot
tickSize <- function(range,minCount){
  logMaxTick <- log10(range/minCount)
  exponent <- floor(logMaxTick)
  mantissa <- 10^(logMaxTick-exponent)
  af <- c(1,2,5) # allowed factors
  mantissa <- af[findInterval(mantissa,af)]
  return(mantissa*10^exponent)
}

# load data with date labels, create train set
single_time_series <- my_data[,ylabel]
notNA <- !is.na(single_time_series)
staNA <- min(which(notNA == TRUE))
endNA <- max(which(notNA == TRUE))
single_time_series <- single_time_series[staNA:endNA]
data_init <- as.numeric(unlist(strsplit(as.character(my_data[staNA,1]), toupper(substr(data_type,1,1))))) # c(2004,1)#min(my_data$date) 
ts_full <- ts(single_time_series,start=data_init,frequency=data_freq)
ts_obs <- time(ts_full) # observations
ts_short <- window(ts_full,end=ts_obs[length(ts_obs)-fh])

# settings for Y and X axis
ylimits <- c(floor(floor(mean(ts_full)-3*sd(ts_full))/10)*10,ceiling(ceiling(mean(ts_full)+4*sd(ts_full))/10)*10)
ystep <- tickSize(range(ylimits)[2]-range(ylimits)[1],5) # y axis step
yticks <- seq(ylimits[1],ylimits[2],by=ystep)
xlimits <- c(ts_obs[1],ts_obs[length(ts_obs)])

# plot time series
plot(ts_full,ylab=ylabel)
tsdisplay(ts_full,main=ylabel)

# plot histogram
hist(ts_full,col=7,main=paste('Histogram of',ylabel)); par(new=TRUE);
M <- mean(ts_full); SD = sd(ts_full)
x <- seq(-4,4,length=200)*SD + M
plot(x,dnorm(x,M,SD),type="l",lty=2,axes=FALSE,xlab='',ylab='')

# additional plots
uYears <- unique(floor(time(ts_full)))
nYears <- length(uYears)
seasonplot(ts_full,year.labels=TRUE, year.labels.left=TRUE, ylab=ylabel, col=rainbow(nYears), pch=20)
monthplot(ts_full,ylab=ylabel,col.base=2)

# time-series decomposition
plot(stl(ts_full,s.window='periodic',robust=TRUE),main=paste('Decomposition by Loess of',ylabel))
#plot(decompose(ts_full, type="multiplicative"))
#plot(decompose(ts_full, type="additive"))

# define univariate time-series models
ts_models_full <- c(
  sprintf('fc_full <- meanf(ts_full,%s,h=%d)',interval_forecast,fh),  
  # sprintf('fc_full <- splinef(ts_full,%s,h=%d)',interval_forecast,fh),  
  # sprintf('fc_full <- naive(ts_full,%s,h=%d)',interval_forecast,fh),  
  sprintf('fc_full <- rwf(ts_full,drift=FALSE,%s,h=%d)',interval_forecast,fh),  
  sprintf('fc_full <- rwf(ts_full,drift=TRUE,%s,h=%d)',interval_forecast,fh),    
  sprintf('fc_full <- snaive(ts_full,%s,h=%d)',interval_forecast,fh),  
  sprintf('fc_full <- forecast(tslm(ts_full ~ trend),%s,h=%d)',interval_forecast,fh),
  sprintf('fc_full <- forecast(tslm(ts_full ~ trend + season),%s,h=%d)',interval_forecast,fh),
  sprintf('qtrend <- seq(length(ts_full))^2; qtrendExtra <- seq(length(ts_full)+1,length(ts_full)+%d)^2; fc_full <- forecast(tslm(ts_full ~ trend + qtrend),%s,data.frame(qtrend=qtrendExtra));',fh,interval_forecast),
  sprintf('qtrend <- seq(length(ts_full))^2; qtrendExtra <- seq(length(ts_full)+1,length(ts_full)+%d)^2; fc_full <- forecast(tslm(ts_full ~ trend + qtrend + season),%s,data.frame(qtrend=qtrendExtra));',fh,interval_forecast),
  sprintf('fc_full <- forecast(auto.arima(ts_full,seasonal=F),%s,h=%d)',interval_forecast,fh),
  sprintf('fc_full <- forecast(auto.arima(ts_full,seasonal=T),%s,h=%d)',interval_forecast,fh),
  sprintf('fc_full <- forecast(stlm(ts_full,method="arima",robust=T,s.window="per"),%s,h=%d)',interval_forecast,fh),
  sprintf('fc_full <- forecast(bats(ts_full),%s,h=%d)',interval_forecast,fh),
  sprintf('fc_full <- forecast(tbats(ts_full),%s,h=%d)',interval_forecast,fh),
  sprintf('fc_full <- forecast(StructTS(ts_full),%s,h=%d)',interval_forecast,fh),
  sprintf('fc_full <- thetaf(ts_full,%s,h=%d)',interval_forecast,fh),
  sprintf('fc_full <- forecast(HoltWinters(ts_full),%s,h=%d)',interval_forecast,fh),
  sprintf('fc_full <- forecast(ets(ts_full),%s,h=%d)',interval_forecast,fh),
  sprintf('fc_full <- forecast(stlm(ts_full,method="ets",robust=T,s.window="per"),%s,h=%d)',interval_forecast,fh),
  # sprintf('fc_full <- forecast(baggedETS(ts_full),%s,h=%d)',interval_forecast,fh),
  #sprintf('fc_full <- forecastProphet(ts_full,h=%d,myfreq=\"%s\",%s)',fh,data_type,interval_forecast),
  # sprintf('fc_full <- forecast(hybridModel(ts_full,s.args=list(robust=T,s.window="per"),verbose=F),%s,h=%d)',interval_forecast,fh),
  #sprintf('fc_full <- croston(ts_full,h=%d)',fh),   
  #sprintf('fc_full <- forecast(nnetar(ts_full),h=%d)',fh),
  # sprintf('fc_full <- forecast(xgbar(ts_full, trend_method="none", seas_method="fourier"),h=%d)',fh),
  # sprintf('fc_full <- forecast(xgbar(ts_full, trend_method="none", seas_method="decompose"),h=%d)',fh),
  #sprintf('fc_full <- forecast(xgbar(ts_full, trend_method="differencing", seas_method="fourier"),h=%d)',fh),
  #sprintf('fc_full <- forecast(xgbar(ts_full, trend_method="differencing", seas_method="decompose"),h=%d)',fh),
  # sprintf('fc_full <- thief(ts_full,comb="mse",usemodel="arima",h=%d)',fh),
  # sprintf('fc_full <- thief(ts_full,comb="mse",usemodel="ets",h=%d)',fh),
  sprintf('fc_full <- thief(ts_full,comb="mse",usemodel="theta",h=%d)',fh)
)
ts_models_short <- gsub("_full","_short",ts_models_full)
ts_models_method <- NULL
ts_models_line <- NULL
ts_models_forecast <- matrix(NA,length(ts_models_full),fh)
ts_short_RMSE <- NULL
ts_short_MAPE <- NULL
ts_short_MASE <- NULL

# run univariate time-series models
for (i in seq(length(ts_models_full))) {
  cat(sprintf('\n\n\n[%d] %s\n',i,ts_models_full[i]))
  # qtrend <- seq(length(ts_short))^2;
  # qtrendExtra <- seq(length(ts_short)+1,length(ts_short)+fh)^2    
  eval(parse(text=ts_models_short[i]));
  # qtrend <- seq(length(ts_full))^2;
  # qtrendExtra <- seq(length(ts_full)+1,length(ts_full)+fh)^2
  eval(parse(text=ts_models_full[i]));
  short_obs <- time(ts(c(fc_full$fitted[(length(fc_full$fitted)-fw+1):length(fc_full$fitted)],fc_full$mean),start=ts_obs[length(ts_obs)-fw+1],frequency=data_freq))  
  #print(fc_full$model)
  #print(fc_full$mean)
  #summary(fc_full) # uncomment to see interval forecast values
  ts_models_forecast[i,] <- fc_full$mean
  true_short <- window(ts_full,start=ts_obs[length(ts_obs)-fh+1])  
  acc <- accuracy(fc_short,true_short)
  cat('\n')
  print(acc[,c("MAE","RMSE","MAPE","MASE","Theil's U")])
  ts_short_RMSE[i] <- acc['Test set','RMSE']
  ts_short_MAPE[i] <- acc['Test set','MAPE']
  ts_short_MASE[i] <- acc['Test set','MASE']
  # create time-series plot with forecast and errors
  plot(fc_full,ylim=ylimits,xlim=c(short_obs[1],short_obs[length(short_obs)]),axes=FALSE,lwd=2,col=1,ann=FALSE,shadecols='oldstyle')
  lines(fc_full$fitted,col=4,lty=5,lwd=2)
  #ts_models_method[i] <- gsub('\\s+$','',fc_full$method)
  mm <- length(fc_short$method)
  if (mm > 1) {
    ts_models_method[i] <- paste(mm,"pack",sep="-")
  } else {
    #ts_models_method[i] <- strsplit(gsub('\\s+$','',fc_short$method),split='\\(')[[1]][1]
    ts_models_method[i] <- fc_short$method
    #ts_models_method[i] <- gsub(" model| method","",ts_models_method[i])
    #ts_models_method[i] <- gsub("Seasonal naive","Snaive",ts_models_method[i])
  }  
  ts_models_line[i] <- sub('fc_full <- ','',ts_models_full[i])  
  title(paste(sprintf('%d. ',i),ts_models_method[i],sprintf(paste('\nRMSE = %5.',dP,'f; MAPE = %5.',dP,'f; MASE = %5.',dP,'f',sep=""),acc['Test set','RMSE'],acc['Test set','MAPE'],acc['Test set','MASE']),sep=''),xlab=xlabel,ylab=ylabel)
  xticks <- unique(round(c(ts_obs,time(fc_full$mean))))
  axis(1,at=xticks,labels=TRUE)
  axis(2,yticks,las=1)
  abline(h=yticks, v=short_obs, col="gray", lty=3);
  rect(time(ts_short)[length(ts_short)]+1/data_freq/2, min(true_short)-ystep/2, time(fc_short$mean)[length(fc_short$mean)]+1/data_freq/2, max(true_short)+ystep/2, lty="dashed")    
  box()
}

# sort models by RMSE
idx <- order(ts_short_RMSE)
topModels <- data.frame(cbind(ts_models_method[idx],round(ts_short_RMSE[idx],digits=dP),round(ts_short_MAPE[idx],digits=dP),round(ts_short_MASE[idx],digits=dP),ts_models_line[idx],ts_models_forecast[idx,]))

# add date on forecast
if (data_freq==4) {
  timeLabel <- as.character(as.yearqtr(time(fc_full$mean)))
} else {
  timeLabel <- as.character(as.yearmon(time(fc_full$mean)))
}
colnames(topModels) <- c('Method name','RMSE','MAPE','MASE','Function runline',timeLabel)

# print model table
sep_line <- '================='
cat(sprintf('\n\n\n%s[ %d %ss ahead forecasting ]%s\n\nTOP-%d forecasting methods for \'%s\' time series\n\n',sep_line,fh,data_type,sep_line,length(ts_models_full),ylabel))
print(cbind(idx,topModels[,1:4]))
cat(' \n')

# save model table
# if resulting .csv doesn't open nicely - change write.csv into write.csv2
#write.csv(topModels,file=paste(data_file,'_',data_type,'_',fh,'_steps_forecast.csv',sep=""))
#print(Sys.time() - TIME)

