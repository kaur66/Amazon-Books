# Pull amazon book sale data 
# setwd("C:/Users/DROGON/Documents/MBA_Courses/RProgramming-MGMT590/Project")
# getwd()
# abs <- data.frame(read.csv(file = "ABS.csv", sep = ",", header = TRUE))
# head(abs,5)

runRegression = function(){
  
  DataQualityReport(abs)
  
  abs$DailyUnitsSold <- as.numeric(abs$DailyUnitsSold)
  abs$DatePublished <- as.Date(abs$DatePublished, format = "%m/%d/%Y")
  
  DataQualityReport(abs)
  
  TodaysDate = Sys.Date()
  TodaysDate
  
  class(abs$DatePublished)
  class(TodaysDate)
  
  abs$DaysPublished <- difftime(TodaysDate, abs$DatePublished, units = "day")
  abs$DaysPublished
  
  
  DataQualityReport(abs)
  abs$DaysPublished <- as.numeric(abs$DaysPublished)
  
  DataQualityReport(abs)
  
  
  #create dummy variable for Format Variable
  FormatDummy <- as.data.frame(model.matrix(~abs$Format-1))
  FormatDummy
  
  
  abs <- data.frame(abs,FormatDummy)
  head(abs,10)
  
  # Independent variable data.frame
  X <- data.frame(abs$SalePrice,abs$TotalReviews,abs$AverageRating,abs$Preorder,abs$DaysPublished, abs$Category, abs$IndiePublisher, abs$SmallMediumPublisher, abs$AmazonPublsher, abs$BigFivePublisher,abs$UncategorizedSingle.AuthorPublisher,abs$Pages,FormatDummy)
  head(X)
  
  
  #Correlation
  
  library("Hmisc")
  XCor <- rcorr(as.matrix(X))
  XCor
  
  
  #Regression
  
  Y <- data.frame(abs$DailyUnitsSold)
  
  XY <- data.frame(Y,X)
  head(XY, 10)
  
  library(MASS)
  
  fit <- lm(abs.DailyUnitsSold ~ abs.SalePrice + abs.TotalReviews + abs.AverageRating + abs.IndiePublisher + abs.SmallMediumPublisher + abs.UncategorizedSingle.AuthorPublisher + abs.FormatAudible + abs.FormatKindle, data = XY)
  summary(fit)
  
  return(fit)
  
  
}



# ##################################################################################3
# step <- stepAIC(fit, direction = "both")
# step$anova
# step$coefficients
# step$residuals
# summary(step)




# # source("DataQualityReport.R")
# 
# #Plotting trial
# 
# XPlot <- data.frame(abs$SalePrice,abs$AverageRating,abs$Category, abs$IndiePublisher, abs$SmallMediumPublisher, abs$AmazonPublsher, abs$BigFivePublisher,FormatDummy)
# head(XPlot)
# 
# YXPlot <- data.frame(Y,XPlot)
# head(YXPlot, 10)
# 
# 
# 
# 
# for (i in 1:(ncol(YXPlot)-1)) {
#   
# plot(YXPlot$abs.DailyUnitsSold ~ YXPlot[,i],data = YXPlot
#    , type="p"  #points
#    , col="blue"
#    , xlab = colnames(YXPlot[,i])
#    , ylab = colnames(YXPlot$abs.DailyUnitsSold)
# )
#   i = i+1
# }


