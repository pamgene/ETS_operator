library(tercen)
library(dplyr)

do.ETS <- function(data){
  dataX = data$.x
  dataY = data$.y
  
  nPoints = nrow(data)
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
  data.frame(slope=slope, intercept = intercept, R2 = R2, nPoints = nPoints, Result = Result)
}

calFractionPresent <- function(data){
  data.frame(data, fractionPresent = sum(data$Result) / length(data$Result))
}

ctx <- tercenCtx()

if(inherits(try(ctx$select(".x")), 'try-error')) stop("x axis is missing.")
if(inherits(try(ctx$select(".y")), 'try-error')) stop("y axis is missing.")

ctx %>% 
  select(.x, .y, .ri, .ci) %>% 
  group_by(.ri, .ci) %>%
  do(do.ETS(.)) %>%
  ungroup() %>%
  group_by(.ri, .ci) %>%
  do(calFractionPresent(.)) %>%
  ctx$addNamespace() %>%
  ctx$save()