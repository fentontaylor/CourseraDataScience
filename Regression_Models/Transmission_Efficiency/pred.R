pred <- function(trans, weight, qtrsec) {
      if(trans==0) {trans <- "Automatic"}
      else if(trans==1) {trans <- "Manual"}
      mydf <- data.frame(am=trans,wt=weight,qsec=qtrsec)
      basic <- lm(formula = mpg ~ am, data = mtcars)
      best <- lm(formula = mpg ~ wt + qsec + am, data = mtcars)
      int <- lm(formula = mpg ~ am:wt + am:qsec, data = mtcars)
      a <- predict(basic, mydf)
      b <- predict(best, mydf)
      c <- predict(int, mydf)
      p <- c(a,b,c)
      names(p) <- c("Basic", "Best", "Int")
      p
}