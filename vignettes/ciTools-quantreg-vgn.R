library(quantreg)
library(ciTools)
library(tidyverse)
library(modelr)

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

b = c(1, 1)
tb <- gen_data_set(beta = b)

lmfit <- lm(y ~ ., data = tb)
rqfit <- rq(y ~ ., data = tb, tau = tquant)

lmq <- fullspace %>% 
  add_quantile(lmfit, p = tquant) %>% 
  add_predictions(rqfit, var = "rq_pred") 


generate_preds <- function(tb, fullspace, tbquant){
  
  lmfit <- lm(y ~ ., data = tb)
  rqfit <- rq(y ~ ., data = tb, tau = tquant)
  
  fullspace %>% 
    add_quantile(lmfit, p = tquant) %>% 
    add_predictions(rqfit, var = "rq_pred") 

}

nsim = 1000
out <- NULL
for(sim in 1:nsim){
  
  out <- gen_data_set() %>% 
    generate_preds(fullspace, tbquant = .7) %>% 
    mutate(sim_number = sim) %>% 
    bind_rows(out)
  
}

out %>% 
  mutate(bias_rq = q7 - pred,
         bias_cit = q7 - quantile0.7,
         coverage_rq = pnorm(pred, x1 + (x2 == "b"), sd = 1),
         coverage_cit = pnorm(quantile0.7, x1 + (x2 == "b"), sd = 1)) %>% 
  group_by(sim_number) %>% 
  summarise(`Avg. Bias RQ` = mean(bias_rq),
            `Avg. Bias ciTools` = mean(bias_cit),
            `Avg. coverage RQ` = mean(coverage_rq),
            `Avg. coverage ciTools` = mean(coverage_cit)) %>% 
  gather(`Avg. Bias RQ`, `Avg. Bias ciTools`, 
         `Avg. coverage RQ`, `Avg. coverage ciTools`, 
         key = "summary", value = "value") %>%  
  group_by(summary) %>% 
  summarise(mean(value))
# 
# 
# #Bias measures how different, on average, our estimated quantile is from the true value
# calculate_bias <- function(qhat, trueq){
#   
#   mean(qhat - trueq)
#   
# }
# 
# 
# 
# #MSE is a composite measure of bias and variance for an estimator
# calculate_mse <- function(qhat, xmat, beta, tau){
#   
#   mean((qhat - qnorm(tau, xmat %*% beta, sd = 1))^2)
#   
# }
# 
# #Coverage is a measure of the actual probability covered by a quantile. Ideally, this should be equal to tau.
# calculate_coverage <- function(qhat, xmat, beta){
#   
#   pnorm(qhat, xmat %*% beta, sd = 1)
#   
# }
# 
# out %>% 
#   group_by(sim_number) %>% 
#   summarise(`Avg. Bias RQ` = calculate_bias(rq_pred, q7),
#             `Avg. Bias RQ` = calculate_bias(quantile0.7, q7),
#             `Avg. coverage RQ` = calculate_coverage(rq_pred, model.matrix(lmfit), b),
#             `Avg. coverage ciTools` = calculate_coverage(quantile0.7, model.matrix(lmfit), b)) 
# 
# #Next step is to compare the estimated quantile to the real one
