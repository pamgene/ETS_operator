library(tercen)
library(dplyr)

options("tercen.workflowId" = "282cdc3059e2f028b801fe3bf4005459")
options("tercen.stepId"     = "a1a1acb5-f7d3-4032-bcae-c1d54ac03f22")

do.ETS <- function(data){
  dataX = data$.x
  dataY = data$.y
  
  nPoints = nrow(data)
  if (nPoints > 1){      
    aLm <- try(lm(dataY ~ dataX+0), silent = TRUE)
    if(!inherits(aLm, 'try-error')){
      slope     <- aLm$coefficients[[1]]
      intercept <- 0
      ssY       <- sum((dataY-mean(dataY))^2)
      yFit      <- predict(aLm)
      R2        <- 1-(sum((dataY-yFit)^2) / ssY)
      Result    <- 1
    } else {
      slope  <- intercept <- R2 <- yFit <- NaN
      Result <- 0
    }
  } 
  if (nPoints == 1){
    slope     <- dataY/dataX
    intercept <- 0
    R2        <- 1
    Result    <- 1
    yFit      <- dataY
  }
  if (nPoints == 0){
    slope  <- intercept <- R2 <- yFit <- NaN
    Result <- 0
  }
  data.frame(slope=slope, intercept = intercept, R2 = R2, nPoints = as.double(nPoints), Result = Result, xFit = dataX, yFit = yFit)
}

calFractionPresent <- function(data){
  data.frame(data, fractionPresent = sum(data$Result) / length(data$Result))
}

ctx <- tercenCtx()

if(inherits(try(ctx$select(".x")), 'try-error')) stop("x axis is missing.")
if(inherits(try(ctx$select(".y")), 'try-error')) stop("y axis is missing.")

ETS_result <- ctx %>% 
  select(.x, .y, .ri, .ci) %>%
  group_by(.ri, .ci) %>%
  do(do.ETS(.)) 

sum.table <- ETS_result %>%
  select(-c(xFit, yFit)) %>%
  unique() %>% 
  ungroup() %>%
  group_by(.ri, .ci) %>%
  do(calFractionPresent(.)) %>%
  ctx$addNamespace() 

pred.table <- ETS_result %>%
  select(.ri, .ci, xFit, yFit) %>%
  ctx$addNamespace()

ctx$save(list(sum.table, pred.table))
