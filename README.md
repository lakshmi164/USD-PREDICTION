df = readxl::read_xlsx("C:/Users/DEVI PRADEEP/Downloads/R/USD_INR Data.xlsx")
df

str(df)

#extract the year from date
df$Year = format(df$Date, "%Y")
df

summary(df)

#calculate the average USD using aggregate.
yearly_data = aggregate(Price~Year, data = df, FUN = mean)
yearly_data

str(yearly_data)

#ensure year is treated as factor for plotting 
yearly_data$Year = as.factor(yearly_data$Year)

#plot the year wise avg USD and the trend
library(ggplot2)
ggplot(yearly_data,aes(x=Year,y=Price,group=1))+
  geom_line(color="blue",size=1)+
  geom_point(color="red",size=2)+
  geom_smooth(method="lm",se=FALSE,color="black",size=1,linetype="dashed")+
  labs(title="Yearly USD PRICE TREND",x="Year",y="Price")

#rolling average.
library(zoo)
df$Rolling_Avg = rollmean(df$Price, k=12, fill=NA)
df

#plot the rolling average
ggplot(df,aes(x=Date,y=Price))+
  geom_line(color="blue")+
  geom_line(aes(y=Rolling_Avg),color="green")+
  labs(title="Rolling Average USD Price trend",x="Date",y="price")

#rolling avg for first 50 days
df$Rolling_Avg_50 = rollmean(df$Price, k=50, fill=NA)
df

#plot the rolling average
ggplot(df,aes(x=Date,y=Price))+
  geom_line(color="blue")+
  geom_line(aes(y=Rolling_Avg_50),color="green")+
  labs(title="Rolling Average USD PRICE ",x="Date",y="price")

#rolling avg for first 100 days
df$Rolling_Avg_100 = rollmean(df$Price, k=100, fill=NA)
df

#plot the rolling average
ggplot(df,aes(x=Date,y=Price))+
  geom_line(color="blue")+
  geom_line(aes(y=Rolling_Avg_100),color="green")+
  labs(title="Rolling Average USD Price Trend",x="Date",y="price")

#monthly analysis
#extract the month from date
df$Month = format(df$Date, "%B")
df

#calculate the avg USD price for each month.
monthly_data = aggregate(Price~Month, data = df, FUN = mean)
monthly_data

#ensure the month is ordered correctly.
monthly_data$Month = factor(monthly_data$Month, levels = month.name)
monthly_data

#plot the month wise avg USD price
ggplot(monthly_data,aes(x=Month,y=Price,group=1))+
  geom_line(color="blue",size=1)+
  geom_point(color="red",size=2)+
  labs(title="Monthly USD Price Trend",x="Month",y="Price")

#forcast the range next 5 years
library(forecast)
ts_data=ts(df$Price,start=c(1979,1),frequency=12)
ts_data

arima_model = auto.arima(ts_data)
forcast_data = forecast(arima_model,h=60,level=c(90,95))

#plot the forcasted data
autoplot(forcast_data)+
  labs(title="USD Price Forecast",x="Year",y="Price")

#retreive the forecasted values 
forcasted_values = forcast_data$mean

#extract the forcasted value for the year 2030
forcasted_values_2030= forcasted_values[49:60]
forcasted_values_2030
forcasted_values_2030[12]
