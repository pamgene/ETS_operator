library(tercen)
library(dplyr)
library(dtplyr)

ctx <- tercenCtx()

if(!ctx$hasXAxis) stop("x axis is missing.")

dt_in <- ctx %>% 
  select(.x, .y, .ri, .ci) %>%
  data.table::as.data.table()

ETS_result <- dt_in[, 
          {
            x = .x
            y = .y
            
            nPoints = length(x)
            if (nPoints > 1) {
              slope     <- sum(x*y) / sum(x^2)
              intercept <- 0
              ssY       <- sum((y-mean(y))^2)
              yFit      <- x * slope
              R2        <- 1-(sum((y-yFit)^2) / ssY)
              Result    <- 1
              
            } 
            if (nPoints == 1) {
              slope     <- y/x
              intercept <- 0
              R2        <- 1
              Result    <- 1
              yFit      <- y
            }
            if (nPoints == 0) {
              slope  <- intercept <- R2 <- yFit <- NaN
              Result <- 0
            }
            list(
              slope = slope,
              intercept = intercept,
              R2 = R2,
              nPoints = as.double(nPoints),
              Result = Result,
              xFit = x,
              yFit = yFit
            )
          }, by=c(".ri", ".ci")
]

sum.table <- ETS_result %>%
  select(-c(xFit, yFit)) %>%
  unique() %>% 
  ungroup() %>%
  group_by(.ri, .ci) %>%
  mutate(fractionPresent = mean(Result)) %>%
  as_tibble() %>%
  ctx$addNamespace() 
  
pred.table <- ETS_result %>%
  select(.ri, .ci, xFit, yFit) %>%
  as_tibble() %>%
  ctx$addNamespace()

ctx$save(list(sum.table, pred.table))
