' Let us start with cleaning our data. The data is not consistent for 
2016 onwards. We have taken out 2017, 2020 and N/A
This was followed by converting the Year of Release and User score
to a numeric value
'
library(ggplot2)
library(dplyr)
library(DT)
library(tidyr)
library(wesanderson)
library(plotly)

gamesales <- read.csv("C:\\Users\\estellahsin\\Desktop\\data\\vgsales.csv")
gamesales <- gamesales[!(gamesales$Year %in% c("N/A", "2017", "2020")),]
gamesales$Year_of_Release <- as.numeric(as.character(gamesales$Year))
gamesales$Rank <- as.numeric(as.character(gamesales$Rank))
gamesales$Global_Sales <- as.numeric(as.character(gamesales$Global_Sales))
'gamesales$User_Score <- as.numeric(as.character(gamesales$Rank))'
gamesales$Genre <- as.character(gamesales$Genre)
gamesales <- gamesales %>% gather(Region, Revenue, 7:10) 
gamesales$Region <- factor(gamesales$Region)

## STEP 1 : DATA EXPLORATION (Descriptive Analysis)

# Let us define some colours and themes for our charts
mycolors <- c("#771C19", "#AA3929", "#8E9CA3", "#556670", "#000000", "#E25033", "#F27314", "#F8A31B", "#E2C59F", "#B6C5CC")

theme_1 <- function() {
  
  return(theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.4), plot.title = element_text(size = 15, vjust = 2),axis.title.x = element_text(size = 12, vjust = -0.35)))
  
}

theme_2 <- function() {
  
  return(theme(axis.text.x = element_text(size = 10, vjust = 0.4), plot.title = element_text(size = 15, vjust = 2),axis.title.x = element_text(size = 12, vjust = -0.35)))
  
}



# 1.1 Breaking down the number of releases by year and revenue by year

ggplot(gamesales, aes(Year)) + 
  geom_bar(fill = "Red") +
  theme_1() +
  ggtitle("Video Game Releases by Year")

revenue_by_year <- gamesales %>% 
  group_by(Year) %>%
  summarize(Revenue = sum(Global_Sales))

ggplot(revenue_by_year, aes(Year, Revenue)) + 
  geom_bar(fill = "Blue", stat = "identity") +
  theme_1() +
  ggtitle("Video Game Revenue by Year")


# 1.2 Let us look at the sales broken down by Regions
'install.packages("plotly")
library("plotly")'
gamesales_2 <- read.csv("C:/Users/avira/Desktop/Harrisburg University/Anly Method - II/Project/videogamesales/vgsales.csv")
gamesales_2$Year_of_Release <- as.numeric(as.character(gamesales_2$Year))
Sales_NA <- gamesales_2%>% select(Year_of_Release,NA_Sales) %>%
  group_by(Year_of_Release)%>% 
  summarise(Sum_NA_Sales=sum(NA_Sales))
Sales_EU <- gamesales_2%>% select(Year_of_Release,EU_Sales) %>%
  group_by(Year_of_Release)%>% 
  summarise(Sum_EU_Sales=sum(EU_Sales))
Sales_JP <- gamesales_2%>% select(Year_of_Release,JP_Sales) %>%
  group_by(Year_of_Release)%>% 
  summarise(Sum_JP_Sales=sum(JP_Sales))
Sales_OH <- gamesales_2%>% select(Year_of_Release,Other_Sales) %>%
  group_by(Year_of_Release)%>% 
  summarise(Sum_OH_Sales=sum(Other_Sales))

Sales_evo <- Reduce(function(x,y) merge(x,y,all=TRUE,by="Year_of_Release"),list(Sales_NA,Sales_EU,Sales_JP,Sales_OH))

plot_ly(data=Sales_evo,x=~Year_of_Release)%>%
  add_trace(y=~Sum_NA_Sales,name="North America Sales",mode="lines",type = 'scatter') %>%
  add_trace(y=~Sum_EU_Sales,name="Europe Sales",mode="lines",type = 'scatter') %>%
  add_trace(y=~Sum_JP_Sales,name="Japan Sales",mode="lines",type = 'scatter') %>%
  add_trace(y=~Sum_OH_Sales,name="Other Sales",mode="lines",type = 'scatter') %>%
  layout(title = "Fig.3 Total Sales (in mn) by Year of Release",
         yaxis = list(title="Sales (in millions of units)"))

# 1.3 Top Publisher by Revenue each Year

top_publisher_year <- gamesales %>% 
  group_by(Year, Publisher) %>% 
  summarize(Revenue = sum(Global_Sales)) %>%
  top_n(1)

datatable(top_publisher_year)

ggplot(top_publisher_year, aes(Year, Revenue, fill = Publisher)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top Publisher by Revenue each Year") +
  theme_1() +
  theme(legend.position = "top")

# 1.4 Top Genre by Revenue each year
top_genre <- gamesales %>% 
  group_by(Year, Genre) %>% 
  summarize(Revenue = sum(Global_Sales)) %>%
  top_n(1)

datatable(top_genre)

ggplot(top_genre, aes(Year, Revenue, fill = Genre)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top Genre by Revenue each Year") +
  theme_1() +
  theme(legend.position = "top") +
  scale_fill_manual(values = mycolors)

#1.5 Top 10 publishers by 


length(unique(gamesales$Publisher))
#there are 577 Pubishers
by_publishers <- gamesales %>% group_by(Publisher) %>% summarize(Total = n()) %>% arrange(desc(Total)) %>% head(10)
by_publishers$Percentage <- by_publishers$Total/dim(gamesales)[1] * 100
by_publishers$Publisher <- factor(by_publishers$Publisher)

datatable(by_publishers, filter = "none")

ggplot(by_publishers, aes(reorder(Publisher, Total), Total, fill = Publisher)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Top 10 Publishers by Number of Releases") + 
  theme(legend.position = "none") + 
  xlab("Publisher") +
  theme_2() +
  coord_flip()

#EA IS THE TOP PUBLISHER


top_publishers <-gamesales %>% group_by(Publisher) %>% summarize(Revenue = sum(Global_Sales), 
                  Percentage = Revenue/sum(gamesales$Global_Sales) * 100) %>% arrange(desc(Revenue)) %>% head(10)

top_publishers$Publisher <- factor(top_publishers$Publisher)

datatable(top_publishers)

ggplot(top_publishers, aes(reorder(Publisher, Revenue), Revenue, fill = Publisher)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Top 10 Publishers by Revenue") + 
  theme(legend.position = "none") + 
  xlab("Publisher") +
  ylab("Revenue in millions") +
  theme_2() +
  coord_flip()

# There is change in the positions from the list by number of releases.
# Nintendo is Top 1 with almost 21% of the overall revenue
# EA being Top 2 with nearly half the revenue of the Nintendo.
# Nearly staggering 70% of the overall revenue is generated by the Top 10 publishers

# 1.6 Let us see how the top 10 publishers have grown bt year in terms of releases


top_publishers <- gamesales[gamesales$Publisher %in% by_publishers$Publisher,] %>% group_by(Publisher, Year) %>% summarize(Total= n())

top_publishers$Publisher <- factor(top_publishers$Publisher)

ggplot(top_publishers, aes(Year, Publisher, fill = Total)) + 
  geom_tile(color = "green") + 
  theme_2() +
  ggtitle("Top 10 Publishers Releases by Year") +
  xlab("Year") +
  theme(legend.position = "top")


# EA is one of the late comers !!
# Activision is in the market since 1980 and is considered a Veteran
# EA has highest number of releases between 2002 and 2011.
# THQ has not released any games from 2014


# 1.7 Lets check how top 10 publishers have grown over the years in terms of revenue

top_publishers <- gamesales[gamesales$Publisher %in% by_publishers$Publisher,] %>% 
  group_by(Publisher, Year) %>% 
  summarize(Revenue = sum(Global_Sales))

top_publishers$Publisher <- factor(top_publishers$Publisher)

ggplot(top_publishers, aes(Year, Publisher, fill = Revenue)) + 
  geom_tile(color = "red") + 
  theme_1() +
  ggtitle("Top 10 Publishers by Revenue") +
  xlab("Year") +
  theme(legend.position = "top")


# Nintendo had great year in 2006, it is a pretty clear outlier
# Nintendo, EA and Activision had good run in last 10-15 years in terms of revenue.





# lET run some regression 
#install.packages('moments')
#library('moments')
gamesales$Global_Sales_mn <-  log(gamesales$Global_Sales*1000000)
plot(density(log(gamesales$Global_Sales_mn)))
plot(hist(gamesales$Global_Sales_mn))
summary(gamesales$Global_Sales_mn)
model_1 <- aov(gamesales$Global_Sales_mn ~ gamesales$Platform + gamesales$Genre + gamesales$Publisher + gamesales$Region + gamesales$Rank)
summary(model_1)
qqnorm(model_1$residuals)
qqline(model_1$residuals)
tapply(gamesales$Global_Sales_mn , gamesales$Genre, mean)
# This tells us that the Genre, even though is significant, doesn't actually impact the Global Sales. All the genres almost have an equal chance
# of success
tapply(gamesales$Global_Sales_mn , gamesales$Region, mean)
tapply(gamesales$Global_Sales_mn , gamesales$Publisher, mean)
#On the Other hand, we can see how Nintendo is a preferred publisher wiht a strong relationship with the Global Sales

'model_2 <- aov(gamesales$Revenue ~ gamesales$Platform + gamesales$Genre + gamesales$Publisher + gamesales$Region + gamesales$Rank + gamesales$Global_Sales)'
'shapiro.test(model$residuals)'

scatterplot(gamesales$Global)
