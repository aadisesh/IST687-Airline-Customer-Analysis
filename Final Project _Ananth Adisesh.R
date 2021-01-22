################################################
# IST 387/687, Standard Homework Heading
#
# Student name: Ananth Adisesh
# Homework number: Final Project
# Date due: 4/15/2020

dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

# Set working directory 
# Change to the folder containing your homework data files
setwd("C:/Users/Ananth/Desktop/Syracuse University/687/Final Project - Ananth")
getwd()

install.packages("tm")
install.packages("wordcoud")
install.packages("tidytext")
install.packages("kernlab")
install.packages("ggplot2")
install.packages("jsonlite")
install.packages("tidyr")
install.packages("arules")
install.packages("arulesViz")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("Hmisc")
install.packages("corrplot")
install.packages("PerformanceAnalytics")
install.packages("pastecs")
install.packages("psych")
install.packages("readxl")

library(readxl)
library(psych)
library(pastecs)
library(RCurl)
library(jsonlite)
library(ggplot2)
library(tidyverse)
library(arules)
library(arulesViz)
library(tm)
library(wordcloud)
library(tidytext)
library(kernlab)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(tidyverse)
library(dplyr)

#Import the data file
df <- fromJSON("Spring2020-survey-02.json")
#View the data frame
View(df)
summary(df)
str(df)
dim(df)

missingcol <- apply(df,2, function(x)all(any(is.na(x)))) 
View(missingcol)

#########################################################Data Cleaning########################################################
df<-as.data.frame(do.call("cbind", df), stringsAsFactors = FALSE)
View(df)

trimws(df$Destination.City)
trimws(df$Origin.City)
trimws(df$Airline.Status)
trimws(df$Age)
trimws(df$Gender)
trimws(df$Price.Sensitivity)
trimws(df$Year.of.First.Flight)
trimws(df$Flights.Per.Year)
trimws(df$Loyalty)
trimws(df$olat)
trimws(df$Type.of.Travel)
trimws(df$Total.Freq.Flyer.Accts)
trimws(df$Shopping.Amount.at.Airport)
trimws(df$Eating.and.Drinking.at.Airport)
trimws(df$Class)
trimws(df$Day.of.Month)
trimws(df$Flight.date)
trimws(df$Partner.Code)
trimws(df$Partner.Name)
trimws(df$Origin.State)
trimws(df$Destination.State)
trimws(df$Scheduled.Departure.Hour)
trimws(df$Departure.Delay.in.Minutes)
trimws(df$Arrival.Delay.in.Minutes)
trimws(df$Flight.cancelled)
trimws(df$Flight.time.in.minutes)
trimws(df$Flight.Distance)
trimws(df$Likelihood.to.recommend)
trimws(df$olong)
trimws(df$dlong)
trimws(df$dlat)
trimws(df$freeText)

colnames(df)[colnames(df)=="Destination.City"]<-"DestinationCity"
colnames(df)[colnames(df)=="Origin.City"]<- "OriginCity"
colnames(df)[colnames(df)=="Airline.Status"]<- "AirlineStatus"
colnames(df)[colnames(df)=="Price.Sensitivity"]<- "PriceSensitivity"
colnames(df)[colnames(df)=="Year.of.First.Flight"]<- "YearofFirstFlight"
colnames(df)[colnames(df)=="Flights.Per.Year"]<- "FlightsPerYear"
colnames(df)[colnames(df)=="Type.of.Travel"]<- "TypeofTravel"
colnames(df)[colnames(df)=="Total.Freq.Flyer.Accts"]<- "TotalFreqFlyerAccts"
colnames(df)[colnames(df)=="Shopping.Amount.at.Airport"]<- "ShoppingAmountatAirport"
colnames(df)[colnames(df)=="Eating.and.Drinking.at.Airport"]<- "EatingAndDrinkingAtAirport"
colnames(df)[colnames(df)=="Day.of.Month"]<- "DayOfMonth"
colnames(df)[colnames(df)== "Likelihood.to.recommend"]<- "LikelihoodToRecommend"
colnames(df)[colnames(df)== "Flight.Distance"]<- "FlightDistance"
colnames(df)[colnames(df)== "Flight.time.in.minutes"]<- "FlighttimeInMinutes"
colnames(df)[colnames(df)== "Flight.cancelled"]<- "FlightCancelled"
colnames(df)[colnames(df)== "Arrival.Delay.in.Minutes"]<- "ArrivalDelayinMinutes"
colnames(df)[colnames(df)== "Departure.Delay.in.Minutes"]<- "DepartureDelayinMinutes"
colnames(df)[colnames(df)== "Scheduled.Departure.Hour"]<- "ScheduledDepartureHour"
colnames(df)[colnames(df)== "Destination.State"]<- "DestinationState"
colnames(df)[colnames(df)== "Origin.State"]<- "OriginState"
colnames(df)[colnames(df)== "Partner.Name"]<- "PartnerName"
colnames(df)[colnames(df)== "Partner.Code"]<- "PartnerCode"
colnames(df)[colnames(df)== "Flight.date"]<- "FlightDate"
colnames(df)[colnames(df)== "freeText"]<- "Comments"
df<- separate(df,col="DestinationCity", into=c("DestcityName", "DestAbbrevation"), sep=",")
df<- separate(df,col="OriginCity", into=c("OriginCityName", "OriginAbbrevation"), sep=",")

df$Comments<- as.character(df$Comments)
View(df$Comments)
df$Comments[is.na(df$Comments)]<-"No comments"
which(is.na(df$DestcityName))
which(is.na(df$DestAbbrevation))
which(is.na(df$OriginCityName))
which(is.na(df$OriginAbbrevation))
which(is.na(df$AirlineStatus))
which(is.na(df$Age))
which(is.na(df$Gender))
which(is.na(df$PriceSensitivity))
which(is.na(df$YearofFirstFlight))
which(is.na(df$FlightsPerYear))
which(is.na(df$Loyalty))
which(is.na(df$TypeofTravel))
which(is.na(df$TotalFreqFlyerAccts))
which(is.na(df$ShoppingAmountatAirport))
which(is.na(df$EatingAndDrinkingAtAirport))
which(is.na(df$Class))
which(is.na(df$DayOfMonth))
which(is.na(df$FlightDate))
which(is.na(df$PartnerCode))
which(is.na(df$PartnerName))
which(is.na(df$OriginState))
which(is.na(df$DestinationState))
which(is.na(df$ScheduledDepartureHour))
df[which(is.na(df$DepartureDelayinMinutes)),24] <- 0 #Contains NAs
df[which(is.na(df$ArrivalDelayinMinutes)),25] <- 0 #Contains NAs
which(is.na(df$FlightCancelled))
df[which(is.na(df$FlighttimeInMinutes)),27] <- 0 #Contains NAs
which(is.na(df$FlightDistance))
which(is.na(df$LikelihoodToRecommend))
which(is.na(df$olong))
which(is.na(df$olat))
which(is.na(df$dlong))
which(is.na(df$dlat))
which(is.na(df$Comments))
df$LikelihoodToRecommend <- as.integer(df$LikelihoodToRecommend)
df$olong <- as.numeric(df$olong)
df$olat <- as.numeric(df$olat)
df$DepartureDelayinMinutes <- as.integer(df$DepartureDelayinMinutes)
df$ArrivalDelayinMinutes <- as.integer(df$ArrivalDelayinMinutes)


unique(ProjectDf$LikelihoodToRecommend)#to check if the likehood is in integers only



#########################################################Converting values to numeric########################################################
df$Age <- as.numeric(as.character(df$Age))
df$PriceSensitivity <- as.numeric(as.character(df$PriceSensitivity))
dF$YearofFirstFlight  <- as.numeric(as.character(df$YearofFirstFlight))
df$FlightsPerYear <- as.numeric(as.character(df$FlightsPerYear))
df$Loyalty <- as.numeric(as.character(df$Loyalty))
df$TotalFreqFlyerAccts <- as.numeric(as.character(df$TotalFreqFlyerAccts))
df$ShoppingAmountatAirport <- as.numeric(as.character(df$ShoppingAmountatAirport))
df$EatingAndDrinkingAtAirport <- as.numeric(as.character(df$EatingAndDrinkingAtAirport))
df$DayOfMonth <- as.numeric(as.character(df$DayOfMonth))
df$ScheduledDepartureHour<- as.numeric(as.character(df$ScheduledDepartureHour))
df$DepartureDelayinMinutes <- as.numeric(as.character(df$DepartureDelayinMinutes))
df$ArrivalDelayinMinutes <- as.numeric(as.character(df$ArrivalDelayinMinutes))
df$FlighttimeInMinutes <- as.numeric(as.character(df$FlighttimeInMinutes))
df$FlightDistance <- as.numeric(as.character(df$FlightDistance))
df$LikelihoodToRecommend <- as.numeric(as.character(df$LikelihoodToRecommend))
df$olong <- as.numeric(as.character(df$olong))
df$olat <- as.numeric(as.character(df$olat))
df$dlong<- as.numeric(as.character(df$dlong))
df$dlat<- as.numeric(as.character(df$dlat))

#########################################################Removing the NA values########################################################
df[which(is.na(df$DepartureDelayinMinutes)),24] <- 0 
df[which(is.na(df$ArrivalDelayinMinutes)),25] <- 0 
which(is.na(df$FlightCancelled))
df[which(is.na(df$FlighttimeInMinutes)),27] <- 0 





#########################################################Data visualization########################################################

#Descriptive statistics, histogram and plot - 1
desc(df$Age)
summary(df$Age)
as.numeric(df$Age)
hist(as.numeric(df$Age), col = "blue")
mean(df$Age,na.rm = TRUE)
df$Age[is.na(df$Age)] <- mean(df$Age,na.rm = TRUE)
plot(density(as.numeric(df$Age)),main = "Age Skewness plot", xlab = "Age",col = "red")

#Descriptive statistics, histogram and plot - 2 
desc(df$EatingAndDrinkingAtAirport)
summary(df$EatingAndDrinkingAtAirport)
as.numeric(df$EatingAndDrinkingAtAirport)
hist(as.numeric(df$EatingAndDrinkingAtAirport), col = "blue")
mean(df$EatingAndDrinkingAtAirport,na.rm = TRUE)
df$EatingAndDrinkingAtAirport[is.na(df$EatingAndDrinkingAtAirport)] <- mean(df$EatingAndDrinkingAtAirport,na.rm = TRUE)
plot(density(as.numeric(df$EatingAndDrinkingAtAirport)),main = "Eating and Drinking at Airport Skewness plot", xlab = "EatingAndDrinkingAtAirport",col = "red")

#Descriptive statistics, histogram and plot - 3
desc(df$LikelihoodToRecommend)
summary(df$LikelihoodToRecommend)
as.numeric(df$LikelihoodToRecommend)
hist(as.numeric(df$LikelihoodToRecommend), col = "blue")
mean(df$LikelihoodToRecommend,na.rm = TRUE)
df$LikelihoodToRecommend[is.na(df$LikelihoodToRecommend)] <- mean(df$LikelihoodToRecommend,na.rm = TRUE)
plot(density(as.numeric(df$LikelihoodToRecommend)),main = "Likelihood To Recommend Skewness plot", xlab = "LikelihoodToRecommend",col = "red")

#Descriptive statistics, histogram and plot - 4
desc(df$ShoppingAmountatAirport)
summary(df$ShoppingAmountatAirport)
as.numeric(df$ShoppingAmountatAirport)
hist(as.numeric(df$ShoppingAmountatAirport), col = "blue")
mean(df$ShoppingAmountatAirport,na.rm = TRUE)
df$ShoppingAmountatAirport[is.na(df$ShoppingAmountatAirport)] <- mean(df$ShoppingAmountatAirport,na.rm = TRUE)
plot(density(as.numeric(df$ShoppingAmountatAirport)),main = "Shopping Amount At the Airport Skewness plot", xlab = "ShoppingAmountAtAirport",col = "red")

#Descriptive statistics, histogram and plot - 5
desc(df$TotalFreqFlyerAccts)
summary(df$TotalFreqFlyerAccts)
as.numeric(df$TotalFreqFlyerAccts)
hist(as.numeric(df$TotalFreqFlyerAccts), col = "blue")
mean(df$TotalFreqFlyerAccts,na.rm = TRUE)
df$TotalFreqFlyerAccts[is.na(df$TotalFreqFlyerAccts)] <- mean(df$TotalFreqFlyerAccts,na.rm = TRUE)
plot(density(as.numeric(df$TotalFreqFlyerAccts)),main = "Total Frequent Flyer Accounts Skewness plot", xlab = "TotalFreqFlyerAccts",col = "red")

#Descriptive statistics, histogram and plot - 6
desc(df$Loyalty)
summary(df$Loyalty)
as.numeric(df$Loyalty)
hist(as.numeric(df$Loyalty), col = "blue")
mean(df$Loyalty,na.rm = TRUE)
df$Loyalty[is.na(df$Loyalty)] <- mean(df$Loyalty,na.rm = TRUE)
plot(density(as.numeric(df$Loyalty)),main = "Customer Loyalty Skewness plot", xlab = "Loyalty",col = "red")

#Descriptive statistics, histogram and plot - 7
desc(df$PriceSensitivity)
summary(df$PriceSensitivity)
as.numeric(df$PriceSensitivity)
hist(as.numeric(df$PriceSensitivity), col = "blue")
mean(df$PriceSensitivity,na.rm = TRUE)
df$PriceSensitivity[is.na(df$PriceSensitivity)] <- mean(df$PriceSensitivity,na.rm = TRUE)
plot(density(as.numeric(df$PriceSensitivity)),main = "Price Sensitivity Skewness plot", xlab = "PriceSensitivity",col = "red")

#Descriptive statistics, histogram and plot - 8
desc(df$FlightsPerYear)
summary(df$FlightsPerYear)
as.numeric(df$FlightsPerYear)
hist(as.numeric(df$FlightsPerYear), col = "blue")
mean(df$FlightsPerYear,na.rm = TRUE)
df$FlightsPerYear[is.na(df$FlightsPerYear)] <- mean(df$FlightsPerYear,na.rm = TRUE)
plot(density(as.numeric(df$FlightsPerYear)),main = "Flights Per Year Skewness plot", xlab = "FlightsPerYear",col = "red")

#Descriptive statistics, histogram and plot - 9
desc(df$DayOfMonth)
summary(df$DayOfMonth)
as.numeric(df$DayOfMonth)
hist(as.numeric(df$DayOfMonth), col = "blue")
mean(df$DayOfMonth,na.rm = TRUE)
df$DayOfMonth[is.na(df$DayOfMonth)] <- mean(df$DayOfMonth,na.rm = TRUE)
plot(density(as.numeric(df$DayOfMonth)),main = "Day Of Month Skewness plot", xlab = "DayOfMonth",col = "red")

#Descriptive statistics, histogram and plot - 10
desc(df$ScheduledDepartureHour)
summary(df$ScheduledDepartureHour)
as.numeric(df$ScheduledDepartureHour)
hist(as.numeric(df$ScheduledDepartureHour), col = "blue")
mean(df$ScheduledDepartureHour,na.rm = TRUE)
df$ScheduledDepartureHour[is.na(df$ScheduledDepartureHour)] <- mean(df$ScheduledDepartureHour,na.rm = TRUE)
plot(density(as.numeric(df$ScheduledDepartureHour)),main = "Scheduled Departure Hour Skewness plot", xlab = "ScheduledDepartureHour",col = "red")



#########################################################Support Vector Machines########################################################

df$Emotion <- "Detractor"
df$Emotion[which(df$LikelihoodToRecommend > 7)] <- "Promoter"

df1 <- data.frame(df$Gender, df$DepartureDelayinMinutes, df$ArrivalDelayinMinutes, df$FlightCancelled, df$FlighttimeInMinutes, ltr = df$LikelihoodToRecommend)
df1$ltr <- as.integer(df1$ltr)
df1$Emotion <- "Detractor"
df1$Emotion[which(df1$ltr > 7)] <- "Promoter"

Index <- sample(1:dim(df1)[1])
twothird <- floor(2*dim(df1)[1]/3)
tds <- df1[Index[1:twothird],]
tds1 <- df1[Index[(twothird+1):dim(df1)[1]],]

svmOutput <- ksvm(Emotion ~.,data = df1, kernel = "rbfdot", kpar = "automatic", C=5, cross = 3, prob.model = TRUE)
svmOutput
svmPred <- predict(svmOutput, tds1)
table1 <- table(svmPred == tds1[,7])
table1
Result <- (table1[1]/(table1[1]+table1[2]))*100
Result


########################################################Association Rule Mining########################################################

df <- fromJSON("Spring2020-survey-02.json")

df$Destination.City<-as.factor(df$Destination.City)
df$Origin.City<-as.factor(df$Origin.City)
df$Airline.Status<-as.factor(df$Airline.Status)
df$Age<-as.factor(df$Age)
df$Gender<-as.factor(df$Gender)
df$Price.Sensitivity<-as.factor(df$Price.Sensitivity)
df$Year.of.First.Flight<-as.factor(df$Year.of.First.Flight)
df$Loyalty<-as.factor(df$Loyalty)
df$Flights.Per.Year <- discretize(df$Flights.Per.Year)
df$Likelihood.to.recommend <- discretize(df$Likelihood.to.recommend)
df$Type.of.Travel<-as.factor(df$Type.of.Travel)
df$Total.Freq.Flyer.Accts<-as.factor(df$Total.Freq.Flyer.Accts)
df$Shopping.Amount.at.Airport<-as.factor(df$Shopping.Amount.at.Airport)
df$Eating.and.Drinking.at.Airport<-as.factor(df$Eating.and.Drinking.at.Airport)
df$Class<-as.factor(df$Class)
df$Day.of.Month<-as.factor(df$Day.of.Month)
df$Flight.date<-as.factor(df$Flight.date)
df$Partner.Code<-as.factor(df$Partner.Code)
df$Partner.Name<-as.factor(df$Partner.Name)
df$Origin.State<-as.factor(df$Origin.State)
df$Destination.State<-as.factor(df$Destination.State)
df$Scheduled.Departure.Hour<-as.factor(df$Scheduled.Departure.Hour)
df$Departure.Delay.in.Minutes<-as.factor(df$Departure.Delay.in.Minutes)
df$Arrival.Delay.in.Minutes<-as.factor(df$Arrival.Delay.in.Minutes)
df$Flight.cancelled<-as.factor(df$Flight.cancelled)
df$Flight.time.in.minutes<-as.factor(df$Flight.time.in.minutes)
df$Flight.Distance<-as.factor(df$Flight.Distance)
df$Flight.time.in.minutes<-as.factor(df$Flight.time.in.minutes)
df$Likelihood.to.recommend<-as.factor(df$Likelihood.to.recommend)
df$olong<-as.factor(df$olong)
df$olat<-as.factor(df$olat)
df$dlong<-as.factor(df$dlat)
df$freeText<-as.factor(df$freeText)
df$dlat<-as.factor(df$dlat)
df<-as(df,"transactions")

View(df)

RS <- apriori(df,parameter = list(supp=0.008, conf=0.9),appearance = list(rhs=c("Likelihood.to.recommend=[8,10]"),default="lhs"),control = list(verbose=F))
RS <- sort(RS,decreasing = TRUE,by="lift")
summary(RS)
plot(RS,jitter=0)
inspect(RS)
RS1 <- subset(RS,subset= rhs %in% "Likelihood.to.recommend=[8,10]")
RS1
plot(RS1, method="paracoord", control=list(reorder=TRUE))

#########################################################Comments section text mining########################################################

posWords <- scan("positive-words.txt", character(0), sep = "\n")
negWords <- scan("negative-words.txt", character(0), sep = "\n")
posWords <- posWords[-1:-34]
negWords <- negWords[-1:-34]

df$CommentType <- ""
for(i in 10001:dim(df[1])){
  charVector <- df$Comments[i]
  words.vector <- VectorSource(charVector)
  words.corpus <- Corpus(words.vector)
  words.corpus <- tm_map(words.corpus,content_transformer(tolower))
  words.corpus <- tm_map(words.corpus, removePunctuation)
  words.corpus <- tm_map(words.corpus, removeNumbers)
  words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
  tdm <- TermDocumentMatrix(words.corpus)
  m <- as.matrix(tdm)
  wordCounts <- rowSums(m)
  matchedP <- match(names(wordCounts), posWords, nomatch = 0) 
  matchedN <- match(names(wordCounts), negWords, nomatch = 0) 
  if (sum(matchedN != 0) > sum(matchedP != 0)){
    df$CommentType[i] <- "Negative"
  }else {
    df$CommentType[i] <- "Positive"
  }
  if (sum(matchedN != 0) == sum(matchedP != 0)){
    df$CommentType[i] <- "Neutral"
  }
}
View(df)
table(df$CommentType)
positive_percentage <- (table(df$CommentType)[4]/sum(table(df$CommentType)))*100
positive_percentage
negative_percentage <- (table(df$CommentType)[2]/sum(table(df$CommentType)))*100
negative_percentage
neutral_percentage <- (table(df$CommentType)[3]/sum(table(df$CommentType)))*100
neutral_percentage
ggplot(filter(df,CommentType != ""), aes(x = CommentType)) + geom_bar(position = "dodge")


#########################################################Maps and plots########################################################

#Plot - 1
df$AgeGroup <- ifelse(df$Age<=30,'Young',ifelse(df$Age<=60,'Middle Age','Old'))
agegroupDf <- df %>% 
  group_by(AgeGroup,Gender, AirlineStatus) %>% 
  summarise(AgeGroupLTR=mean(LikelihoodToRecommend))
ggplot(data = agegroupDf) +
  geom_col(
    mapping = aes(x = AgeGroup, fill = Gender, y = AgeGroupLTR),
    position = "dodge"
  )


#Plot - 2
LTR1 <- df %>% 
  group_by(PartnerName) %>%
  summarise(airlineLTR=mean(LikelihoodToRecommend))
LTR1$airlineLTR <- sort(LTR1$airlineLTR, decreasing = TRUE)
LTRplot1 <- ggplot(LTR1) +
  geom_col(mapping = aes(x = PartnerName, y = airlineLTR)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "Partner Name", y="Mean LTR") 
LTRplot1

#Plot - 3
df1 <- df
df1 <- group_by(df1, OriginState) %>%
  summarise(meanLTR = mean(LikelihoodToRecommend))
df1$OriginState <- tolower(df1$OriginState)
df1$state <- df1$OriginState
df1$OriginState <- c()
us <- map_data('state')
us <- filter(us, region != "west virginia")
df1$lat <- unique(us$lat)
dummydf <- data.frame(state.name, stringsAsFactors = FALSE)
dummydf$state <- tolower(dummydf$state.name)
map_averages <- ggplot(filter(df1, meanLTR < 7), aes(map_id = state)) +
  geom_map(map=us, aes(fill = meanLTR), color = "white")+
  expand_limits(x=us$long, y = us$lat) +
  coord_map() + ggtitle("US States with lesser average LTR (<7)")
map_averages

#Plot - 4
df$Loyalty <- as.numeric(df$Loyalty)
loyaltydf <- df %>%
  group_by(PartnerName) %>%
  summarise(meanLty = mean(Loyalty))
loyaltydf$meanLty <- order(loyaltydf$meanLty, decreasing = TRUE)
loyaltydfPlot <- ggplot(loyaltydf) +
  geom_col(mapping = aes(x = PartnerName, y = meanLty)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "Partner Name", y="Mean Loyalty")
loyaltydfPlot

#Plot - 5
K = tapply(df$LikelihoodToRecommend, df$PartnerName, length)
totalentries <- data.frame(No_of_Entries = K, Airline_name = row.names(K))

barp1 <- ggplot(totalentries, aes(x=reorder(Airline_name,No_of_Entries), y=No_of_Entries))
barp1 <- barp1 + geom_col(color = "Black", fill = "black")
barp1 <- barp1 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
barp1 <- barp1 + ggtitle("Number of passengers for each airline")
barp1 <- barp1 + labs(x = "Airlines")
barp1 <- barp1 + labs(y = "Number of passengeres")
barp1

#Plot - 6
ggplot(filter(df,CommentType != ""), aes(x = "CommentType"))+geom_bar(position = "dodge")

#Plot - 7
travelLTR <- group_by(df, TypeofTravel) %>%
  summarise(meanLTR = mean(LikelihoodToRecommend))
ggplot(df)+
  stat_count(aes(x=TypeofTravel))
travelLTR$meanLTR <- sort(travelLTR$meanLTR, decreasing = TRUE)
travelplot <- ggplot(travelLTR) +
  geom_col(mapping = aes(x = TypeofTravel, y = meanLTR)) +
  labs(x = " Travel Type", y="Mean LTR")
travelplot

#########################################################Analysis using Correlation########################################################


df_numeric <- data.frame(df$Age, df$PriceSensitivity, df$YearofFirstFlight, df$FlightsPerYear, df$Loyalty, df$TotalFreqFlyerAccts, df$ShoppingAmountatAirport, df$EatingAndDrinkingAtAirport, df$DayOfMonth, df$ScheduledDepartureHour, df$DepartureDelayinMinutes, df$ArrivalDelayinMinutes, df$FlighttimeInMinutes, df$FlightDistance, df$LikelihoodToRecommend, stringsAsFactors = FALSE)
df_numeric$df.Age <- as.numeric(df_numeric$df.Age)
df_numeric$df.PriceSensitivity <- as.numeric(df_numeric$df.PriceSensitivity)
df_numeric$df.YearofFirstFlight <- as.numeric(df_numeric$df.YearofFirstFlight)
df_numeric$df.FlightsPerYear <- as.numeric(df_numeric$df.FlightsPerYear)
df_numeric$df.Loyalty <- as.numeric(df_numeric$df.Loyalty)
df_numeric$df.TotalFreqFlyerAccts <- as.numeric(df_numeric$df.TotalFreqFlyerAccts)
df_numeric$df.ShoppingAmountatAirport <- as.numeric(df_numeric$df.ShoppingAmountatAirport)
df_numeric$df.EatingAndDrinkingAtAirport <- as.numeric(df_numeric$df.EatingAndDrinkingAtAirport)
df_numeric$df.DayOfMonth <- as.numeric(df_numeric$df.DayOfMonth)
df_numeric$df.ScheduledDepartureHour <- as.numeric(df_numeric$df.ScheduledDepartureHour)
df_numeric$df.DepartureDelayinMinutes <- as.numeric(df_numeric$df.DepartureDelayinMinutes)
df_numeric$df.ArrivalDelayinMinutes <- as.numeric(df_numeric$df.ArrivalDelayinMinutes)
df_numeric$df.FlighttimeInMinutes <- as.numeric(df_numeric$df.FlighttimeInMinutes)
df_numeric$df.FlightDistance <- as.numeric(df_numeric$df.FlightDistance)
df_numeric$df.LikelihoodToRecommend <- as.numeric(df_numeric$df.LikelihoodToRecommend)

correlation <- cor(df_numeric, method = c("pearson", "kendall", "spearman"))
round(correlation, 2)

correlation2 <- rcorr(as.matrix(df_numeric))
correlation2$r
correlation2$P

corrplot(correlation2$r, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot(correlation2$r , type = "upper", order = "hclust", p.mat = correlation2$P, sig.level = 0.01, insig = "blank")

chart.Correlation(df_numeric, histogram = TRUE, pch = 19)

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = correlation, col = col, symm = TRUE)

################################################Linear Modelling#####################################################
Model_Age <- lm(formula = LikelihoodToRecommend ~ Age, data = df)
summary(Model_Age)
ggplot(df, aes(x = Age, y = LikelihoodToRecommend)) + geom_count() + stat_summary(aes(y = df$LikelihoodToRecommend, group = 1),fun.y = mean, color = "red", geom = "point", group = 1) + labs(x = "Age", y = "LikelihoodToRecommend")

Model_Distance <- lm(formula = LikelihoodToRecommend ~ FlightDistance, data = df)
summary(Model_Distance)
ggplot(df, aes(x = FlightDistance, y = LikelihoodToRecommend)) + geom_count() + stat_summary(aes(y = df$LikelihoodToRecommend, group = 1),fun.y = mean, color = "red", geom = "point", group = 1) + labs(x = "FlightDistance", y = "LikelihoodToRecommend")

Model_FlightTime <- lm(formula = LikelihoodToRecommend ~ FlighttimeInMinutes, data = df)
summary(Model_FlightTime)
ggplot(dff, aes(x = FlighttimeInMinutes, y = LikelihoodToRecommend)) + geom_count() + stat_summary(aes(y=df$LikelihoodToRecommend,group=1),fun.y=mean, color = "red", geom = "point", group = 1) + labs(x = "FlighttimeInMinutes", y = "LikelihoodToRecommend")

Model_CF <- lm(formula = LikelihoodToRecommend ~ FlightCancelled, data= df)
summary(ModelCF)
ggplot(df, aes(x=FlightCancelled, y= LikelihoodToRecommend)) +geom_count() + stat_summary(aes(y=df$LikelihoodToRecommend,group=1),fun.y=mean, color = "red", geom = "point", group = 1) + labs(x = "FlightCancelled", y = "LikelihoodToRecommend")

ModelAD <- lm(formula = LikelihoodToRecommend ~ ArrivalDelayinMinutes, data= df)
summary(ModelAD)
ggplot(df, aes(x=ArrivalDelayinMinutes, y= LikelihoodToRecommend)) +geom_count() + stat_summary(aes(y=df$LikelihoodToRecommend,group=1),fun.y=mean, color = "red", geom = "point", group = 1) + labs(x = "ArrivalDelayinMinutes", y = "LikelihoodToRecommend")

ModelDD <- lm(formula = LikelihoodToRecommend ~ DepartureDelayinMinutes, data= df)
summary(ModelDD)
ggplot(df, aes(x=DepartureDelayinMinutes, y= LikelihoodToRecommend)) +geom_count() + stat_summary(aes(y=d$LikelihoodToRecommend,group=1),fun.y=mean, color = "red", geom = "point", group = 1) + labs(x = "DepartureDelayinMinutes", y = "LikelihoodToRecommend")

Model_OriginState <-  lm(formula = LikelihoodToRecommend ~ OriginState, data= df)
summary(ModelOriginState)
ggplot(df, aes(x=OriginState, y= LikelihoodToRecommend)) +geom_count() + stat_summary(aes(y=df$LikelihoodToRecommend,group=1),fun.y=mean, color = "red", geom = "point", group = 1) + labs(x = "OriginState", y = "LikelihoodToRecommend")

Model_DestinationState <- lm(formula = LikelihoodToRecommend ~ DestinationState, data= df)
summary(Model_DestinationState)
ggplot(df, aes(x=DestinationState, y= LikelihoodToRecommend)) +geom_count() + stat_summary(aes(y=df$LikelihoodToRecommend,group=1),fun.y=mean, color = "red", geom = "point", group = 1) + labs(x = "DestinationState", y = "LikelihoodToRecommend")

Model_PartnerName <- lm(formula = LikelihoodToRecommend ~ PartnerName, data= df)
summary(Model_PartnerName)
ggplot(df, aes(x=PartnerName, y= LikelihoodToRecommend)) +geom_count() + stat_summary(aes(y=df$LikelihoodToRecommend,group=1),fun.y=mean, color = "red", geom = "point", group = 1) + labs(x = "PartnerName", y = "LikelihoodToRecommend")

ClassModel <- lm(formula = LikelihoodToRecommend ~ Class, data= df)
summary(ClassModel)
ggplot(df, aes(x=Class, y= LikelihoodToRecommend)) +geom_count() + stat_summary(aes(y=df$LikelihoodToRecommend,group=1),fun.y=mean, color = "red", geom = "point", group = 1) + labs(x = "Class", y = "LikelihoodToRecommend")

Model_TravelType <- lm(formula = LikelihoodToRecommend ~ TypeofTravel, data= df)
summary(Model_TravelType)
ggplot(df, aes(x=TypeofTravel, y= LikelihoodToRecommend)) +geom_count() + stat_summary(aes(y=df$LikelihoodToRecommend,group=1),fun.y=mean, color = "red", geom = "point", group = 1) + labs(x = "TypeofTravel", y = "LikelihoodToRecommend")

Model_Gender <- lm(formula = LikelihoodToRecommend ~ Gender, data= df)
summary(Model_Gender)
ggplot(df, aes(x=Gender, y= LikelihoodToRecommend)) +geom_count() + stat_summary(aes(y=df$LikelihoodToRecommend,group=1),fun.y=mean, color = "red", geom = "point", group = 1) + labs(x = "Gender", y = "LikelihoodToRecommend")

Model_AirlineStatus <- lm(formula = LikelihoodToRecommend ~ AirlineStatus, data= df)
summary(Model_AirlineStatus)
ggplot(df, aes(x=AirlineStatus, y= LikelihoodToRecommend)) +geom_count() + stat_summary(aes(y=df$LikelihoodToRecommend,group=1),fun.y=mean, color = "red", geom = "point", group = 1) + labs(x = "AirlineStatus", y = "LikelihoodToRecommend")
