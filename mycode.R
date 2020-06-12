library(eia)
library(httr)
library(tidyverse)
library(ggplot2)
library(dtw)
eia_set_key("key=API_KEY_HERE") #SEE LINK IN REPORT TO REQUEST API

###First Extraction methods, not used but mentioned in report
#MyURL2 <- paste0("http://api.eia.gov/series/?api_key=API_KEY_HERE&series_id=EBA.US48-ALL.D.H")
# U.S. Electric System Operating Data > Demand > U.S. > United States Lower 48 (US48)

#rawselect <- httr::GET(MyURL2)

#mycontent<- httr::content(rawselect, as = 'text')
#mycontent_FromJson<- jsonlite::fromJSON(mycontent)

#dplyr::glimpse(mycontent_FromJson)

#MyURL2data<- mycontent_FromJson$series$data


##### Overall, Electric demand of United States

USid<- paste0("EBA.US48-ALL.D.", c("H")) 
# U.S. Electric System Operating Data > Hourly Demand > U.S. > United States Lower 48 (US48)

d1<- eia_series(USid[1], start = 2015)

#Overall, Electric demand of US
unnest(d1, cols = data) %>% ggplot(aes(date, value)) +
  geom_jitter(aes(col= value)) + labs(y = d1$units, title = "United States (Lower 48) Electric Demand")



#### Overall, Electric demand of New York
NYid<- paste0("EBA.NY-ALL.D.", c("H")) 
# U.S. Electric System Operating Data > Hourly Demand > U.S. > United States Lower 48 (US48)

d2<- eia_series(NYid[1], start = 2015)


unnest(d2, cols = data) %>% ggplot(aes(date, value)) +
  geom_jitter(aes(col= value)) + labs(y = d2$units, title = "New York Electric Demand")



####Preparing Data for a side by side comparison
d3<-data.frame(d2$data)
D2020 <- d3 %>% filter(month== 1:5, year == 2020)
D2020 <- aggregate(value~week,data= D2020, sum)

D2019 <- d3 %>% filter(month== 1:5, year == 2019)
D2019 <- aggregate(value~week,data= D2019, sum)

#comparing Jan-May 2020 to Jan-May 2019 to check for significant differences
ggplot(D2020, aes(x=week,value)) + geom_line(color= "blue") + geom_line(data= D2019, colour= "green") + 
  labs(y= "Megawatthours", x= "Week", title = "New York Demand 2020 vs 2019 - (Months of January to May)",
       subtitle = "2020 is Blue 2019 is Green") 


####DWT Modeling
alignment<- dtw(D2020$value,D2019$value, keep=T)

alignment[["localCostMatrix"]]

dtwPlotThreeWay(alignment,xts = D2020$value, yts = D2019$value, xlab = "2020 Data", ylab = "2019 Data")



