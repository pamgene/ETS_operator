library(tercen)
library(plyr)
library(dplyr)
#library(pgMulticore)

options("tercen.workflowId" = "119e20e1121a1ac50b16db38b0000a3d")
options("tercen.stepId"     = "c4810b2f-8eae-466f-937a-29438913fb63")

# linreg for exposure time scaling
operatorFunction <- function(aFrame){
  isOut = aFrame$bOut
  dataX = aFrame$x
  dataY = aFrame$y
  
  nPoints = sum(!isOut)
  dataX = dataX[!isOut]
  dataY = dataY[!isOut]
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

curveFitOperatorFunction <- function(dataX , result, propList) {
  a <- result[1]
  b <- result[2]
  dataY <-lapply(dataX ,  function(x) { y = a*x + b})
  return (dataY)
}
calFractionPresent <- function(aFrame){
  aFrame = data.frame(aFrame, fractionPresent = sum(aFrame$Result)/length(aFrame$Result))
  return(aFrame)
}

getPropertyValue <- function(properties=properties, name=name, prop.is.numeric = FALSE) {
  for ( i in 1:length(properties) ) {
    if (properties[[i]][1] == name){
      val = properties[[i]][2]
      if (prop.is.numeric){
        return(as.numeric(val))
      } else {
        return(val)
      }
    }
  }
  stop(paste("property not found: ", name))
  return (NULL)
}

do.ETS <- function(data) {
  #aResult = ddply(aD, ~rowSeq + colSeq, .fun = operatorFunction, .progress=progress_win(title="Exposure Time Scaling"))
  # bMultiCore = getPropertyValue(properties, "Multicore processing") == "Yes"
  # if(bMultiCore){
  #   aResult = doMultiCore(~rowSeq + colSeq, data = aD, operatorFunction = operatorFunction, .export = "operatorFunction") 
  # } else {
  #   aResult = ddply(aD, ~rowSeq + colSeq, .fun = operatorFunction, .progress=progress_win(title="Exposure Time Scaling"))
  # }
  
  metaData    <- varMetadata(data)
  aXaxisLabel <- colnames(pData(data))[metaData$groupingType=='xAxis']  
  aD          <- data.frame(rowSeq = pData(data)$rowSeq,
                            colSeq = pData(data)$colSeq, 
                            x      = data[[aXaxisLabel]] , 
                            y      = data[['value']], 
                            bOut   = data[['IsOutlier']])
  
  aResult   <- ddply(aD, ~rowSeq + colSeq, .fun = operatorFunction, .progress=progress_win(title="Exposure Time Scaling"))
  aResult   <- ddply(aResult, ~rowSeq, .fun = calFractionPresent)
  aMetaData <- data.frame(labelDescription=c("rowSeq", "colSeq", "slope", "intercept", "R2", "nPoints", "Result", "fractionPresent"), 
                          groupingType=c("rowSeq","colSeq" , "QuantitationType" , "QuantitationType", "QuantitationType", "QuantitationType","QuantitationType", "Spot") )
  
  return(new("AnnotatedDataFrame", data=aResult, varMetadata=aMetaData))
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