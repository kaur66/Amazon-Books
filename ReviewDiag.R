# a function to visually check linear regression assumptions
reviewDiag <- function(lmfit) {
  # Diagnostic plots
  par(mfcol=c(2,3), fg="black", bg="white",col.lab="black")
  # cooks distance - check for influential points
  cook<-cooks.distance(lmfit) 
  library(faraway) # library needed for half-normalplot
  halfnorm(cook,3,ylab="Cooks distance", main="Influences", col="blue") 
  boxplot(cook, col="blue", ylab="Cooks distance"
          , main="Boxplot Cooks Distances")
  # constant variance
  plot(fitted(lmfit),residuals(lmfit),xlab="Fitted",ylab="Residuals", col="blue"
       , pch=19, type='p', main="Resid vs. Fitted") 
  abline(h=0) 
  plot(fitted(lmfit),abs(residuals(lmfit)),xlab="Fitted",ylab="Abs(Residuals)"
       , main="Abs(Resid) vs. Fitted", col="blue", pch=19)
  # normality
  qqnorm(residuals(lmfit),ylab="Residuals", pch=19, col="blue") 
  qqline(residuals(lmfit)) 
  hist(residuals(lmfit), col="blue", main="Historgram of Residuals")
}
# review assumptions
reviewDiag(lmfit)