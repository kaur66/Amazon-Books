# Pull amazon book sale data 
setwd("C:/Users/DROGON/Documents/MBA_Courses/RProgramming-MGMT590/Project")
getwd()
abs <- data.frame(read.csv(file = "ABS.csv", sep = ",", header = TRUE))
head(abs,5)


source("DataQualityReport.R")
# Check class of each variable
DataQualityReport(abs)

# change data types to numeric
abs$DailyUnitsSold <- as.numeric(abs$DailyUnitsSold)
abs$DatePublished <- as.Date(abs$DatePublished, format = "%m/%d/%Y")

# use the Date Publised variable to calculate the number of days since published
TodaysDate = Sys.Date()
abs$DaysPublished <- difftime(TodaysDate, abs$DatePublished, units = "day")
abs$DaysPublished
abs$DaysPublished <- as.numeric(abs$DaysPublished)

DataQualityReport(abs)

#create dummy variable for Format Variable
FormatDummy <- as.data.frame(model.matrix(~abs$Format-1))
FormatDummy


abs <- data.frame(abs,FormatDummy)
head(abs,10)

# Independent Input variable data.frame
X <- data.frame(abs$SalePrice
                ,abs$TotalReviews
                ,abs$AverageRating
                ,abs$Preorder
                ,abs$DaysPublished
                ,abs$Category
                ,abs$IndiePublisher
                ,abs$SmallMediumPublisher
                ,abs$AmazonPublsher
                ,abs$BigFivePublisher
                ,abs$UncategorizedSingle.AuthorPublisher
                ,abs$Pages
                ,FormatDummy)
head(X)


#Correlation

library("Hmisc")
XCor <- rcorr(as.matrix(X), type = "pearson")
XCor


#Plotting all independent non-binary variables against target variables to check for constant variance

Y <- data.frame(abs$DailyUnitsSold)

XY <- data.frame(Y,X)
head(XY, 10)
DataQualityReport(XY)

par(mfcol=c(2,3), fg="black", bg="white",col.lab="black")

plot(XY$abs.DailyUnitsSold ~ XY$abs.SalePrice, data = XY
     , type="p"  #points
     , col="blue")

plot(XY$abs.DailyUnitsSold ~ XY$abs.TotalReviews, data = XY
     , type="p"  #points
     , col="blue")

plot(XY$abs.DailyUnitsSold ~ XY$abs.AverageRating, data = XY
     , type="p"  #points
     , col="blue")

plot(XY$abs.DailyUnitsSold ~ XY$abs.DaysPublished, data = XY
     , type="p"  #points
     , col="blue")

plot(XY$abs.DailyUnitsSold ~ XY$abs.Category, data = XY
     , type="p"  #points
     , col="blue")

plot(XY$abs.DailyUnitsSold ~ XY$abs.Pages, data = XY
     , type="p"  #points
     , col="blue")

#Multiple Linear Regression - All variables

fit <- lm(abs.DailyUnitsSold ~ abs.SalePrice + abs.TotalReviews + abs.AverageRating + abs.Preorder + abs.DaysPublished + abs.Category + abs.IndiePublisher + abs.SmallMediumPublisher + abs.BigFivePublisher + abs.UncategorizedSingle.AuthorPublisher + abs.Pages + abs.FormatAudible + abs.FormatHardcover + abs.FormatKindle + abs.FormatMass.Market.Paperback + abs.FormatPaperback, data = XY, na.action = NULL)
summary(fit)

source("ReviewDiag.R")
reviewDiag(fit)

# Adjusted R Square Method Regression


library(leaps)
leaps = regsubsets(abs.DailyUnitsSold ~ abs.SalePrice 
                   + abs.TotalReviews 
                   + abs.AverageRating 
                   + abs.DaysPublished 
                   + abs.Category 
                   + abs.IndiePublisher 
                   + abs.SmallMediumPublisher 
                   + abs.BigFivePublisher 
                   + abs.UncategorizedSingle.AuthorPublisher 
                   + abs.FormatAudible 
                   + abs.FormatHardcover 
                   + abs.FormatKindle 
                   + abs.FormatMass.Market.Paperback 
                   + abs.FormatPaperback
                   , data = XY,  nbest = 1)
plot(leaps, scale = "adjr2")


fit1 <- lm(abs.DailyUnitsSold ~ abs.SalePrice 
           + abs.TotalReviews 
           + abs.AverageRating 
           + abs.IndiePublisher 
           + abs.SmallMediumPublisher 
           + abs.UncategorizedSingle.AuthorPublisher 
           + abs.FormatAudible 
           + abs.FormatKindle
           , data = XY)
summary(fit1)

#Functionality Test

source("ReviewDiag.R")
reviewDiag(fit1)




