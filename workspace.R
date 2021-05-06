library(tercen)
library(plyr)
library(dplyr)

options("tercen.workflowId" = "119e20e1121a1ac50b16db38b0000a3d")
options("tercen.stepId"     = "c4810b2f-8eae-466f-937a-29438913fb63")

# linreg for exposure time scaling
operatorFunction <- function(aFrame){
  dataX = aFrame$x
  dataY = aFrame$y
  
  nPoints = nrow(aFrame)
  if (nPoints > 1){      
    aLm <- try(lm(dataY ~ dataX+0), silent = TRUE)
    if(!inherits(aLm, 'try-error')){
      slope = aLm$coefficients[[1]]
      intercept = 0
      ssY <- sum((dataY-mean(dataY))^2)
      yFit <- predict(aLm)
      R2 <- 1-(sum((dataY-yFit)^2) / ssY)
      Result = 1;
      
    } else {
      slope = NaN
      intercept = NaN
      R2 = NaN
      Result = 0;
    }
  } 
  if (nPoints == 1){
    slope = dataY/dataX
    intercept = 0;
    R2 = 1
    Result = 1
  }
  if (nPoints == 0){
    slope = NaN
    intercept = NaN
    R2 = NaN
    Result = 0;
  }
  return (c(rowSeq = aFrame$rowSeq[1], colSeq = aFrame$colSeq[1], slope=slope, intercept = intercept, R2 = R2, nPoints = nPoints, Result = Result))
}

calFractionPresent <- function(aFrame){
  data.frame(aFrame, fractionPresent = sum(aFrame$Result) / length(aFrame$Result))
}

do.ETS <- function(data) {
  df <- data.frame(rowSeq = data$.ri,
                   colSeq = data$.ci, 
                   x      = data$.x, 
                   y      = data$.y)
  result <- ddply(df, ~rowSeq + colSeq, .fun = operatorFunction)
  ddply(result, ~rowSeq, .fun = calFractionPresent) %>% 
    select(-c(rowSeq, colSeq))
}

ctx <- tercenCtx()

if(inherits(try(ctx$select(".x")), 'try-error')) stop("x axis is missing.")
if(inherits(try(ctx$select(".y")), 'try-error')) stop("y axis is missing.")

ctx %>% 
  select(.x, .y, .ri, .ci) %>% 
  group_by(.ri, .ci) %>%
  do(do.ETS(.)) %>%
  ctx$addNamespace() %>%
  ctx$save()
