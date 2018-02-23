library(quantreg)
library(ciTools)
library(tidyverse)
cars

lmfit <- lm(dist ~ speed, data = cars)
rqfit <- rq(dist ~ speed, data = cars, tau = .7)

cars %>% 
  add_quantile(lmfit, p = .7) %>% 
  add_ci(rqfit) %>% 
  ggplot(aes(x = speed, y = dist)) +
  geom_point() +
  geom_line(aes(y = quantile0.7), colour = "blue") +
  geom_line(aes(y = pred), colour = "forestgreen")


#' So they tend to give different results. But which is more accurate? To determine that, we'll need to performa  simulation. 

library(skpr)

gen_data_set <- function(n = 20, beta = c(1, 1)){
  fullspace <- expand.grid(x1 = seq(0, 1, length.out = 21),
                           x2 = letters[1:2])
  xmat <- sample_n(fullspace, n)
  mm <- model.matrix(~x1 + x2, xmat)
  beta <- c(0, beta)
  y <- NULL
  y$y  <- rnorm(nrow(mm)) + mm %*% beta
  as_tibble(xmat) %>%
    bind_cols(y)
  
}

tquant <- .7
fullspace <- expand.grid(x1 = seq(0, 1, length.out = 21),
                         x2 = letters[1:2]) %>% 
  mutate(q7 = x1 + (x2 == "b") + qnorm(tquant))

tb <- gen_data_set()

lmfit <- lm(y ~ ., data = tb)
rqfit <- rq(y ~ ., data = tb, tau = tquant)

lmq <- fullspace %>% 
  add_quantile(lmfit, p = tquant)

lmq

#Next step is to compare the estimated quantile to the real one
