library(tercen)
library(dplyr)

options("tercen.workflowId" = "119e20e1121a1ac50b16db38b0000a3d")
options("tercen.stepId"     = "c4810b2f-8eae-466f-937a-29438913fb63")

do.ETS <- function(df){
  
  dataX = df$.x
  dataY = df$.y
  
  nPoints = nrow(df)
  
  if (nPoints > 1) {
    
    aLm <- try(lm(dataY ~ dataX + 0), silent = TRUE)
    
    if(!inherits(aLm, 'try-error')){
      
      slope <- aLm$coefficients[[1]]
      intercept <- 0
      ssY <- sum((dataY - mean(dataY)) ^ 2)
      yFit <- predict(aLm)
      R2 <- 1 - (sum((dataY - yFit) ^ 2) / ssY)
      Result = 1;
      
    } else {
      
      slope = NaN
      intercept = NaN
      R2 = NaN
      Result = 0;
      
    }
  }
  
  if (nPoints == 1) {
    
    slope = dataY / dataX
    intercept = 0;
    R2 = 1
    Result = 1
    
  }
  
  if (nPoints == 0) {
    
    slope = NaN
    intercept = NaN
    R2 = NaN
    Result = 0;
    
  }
  
  return(data.frame(
    .ri = df$.ri[1],
    .ci = df$.ci[1],
    slope = slope,
    intercept = intercept,
    R2 = R2,
    nPoints = nPoints,
    Result = Result
  ))
  
}

ctx <- tercenCtx()

if(inherits(try(ctx$select(".x")), 'try-error')) stop("x axis is missing.")
if(inherits(try(ctx$select(".y")), 'try-error')) stop("y axis is missing.")

ctx %>% 
  select(.x, .y, .ri, .ci) %>% 
  group_by(.ri, .ci) %>%
  do(do.ETS(.)) %>%
  print()
  # ctx$addNamespace() %>%
  # ctx$save()