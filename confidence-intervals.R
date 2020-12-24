knitr::opts_chunk$set(echo = TRUE, eval = TRUE)

library(purrr)
library(ggplot2)
set.seed(7)
n <- 100 ## sample 100 people
sample <- data.frame(
  Age = rnorm(n,  mean = 30,  sd = 10)
)
sample$Wage <- 2 * sample$Age + rnorm(n, mean = 0, sd = 10)
head(sample)
n <- 100
sample2 <- data.frame(
  Age = rnorm(n, mean = 30, sd = 10)
)
sample2$Wage <- 2 * sample2$Age + rnorm(n, 0, 10) 
head(sample2)
Model <- lm(Wage ~ Age, data = sample) ## Sample 1
Model2 <- lm(Wage ~ Age, data = sample2) ## Sample 2
confint(Model)
confint(Model2)
get_ci <- function(id) {
  sample <- data.frame(
    Age = rnorm(n, mean = 30, sd = 10)
  )
  sample$Wage <- 2* sample$Age + rnorm(n, 0, 10)
  model <- lm(Wage ~ Age, data = sample)
  return(
    data.frame(
      lb = confint(model)[2,1],
      ub = confint(model)[2,2],
      id = id
    ))}
## map function call 100 times
set.seed(7)
ci <- map_df(1:100, get_ci)
ci
ggplot(ci, aes(y = id, color = (lb > 2 | ub < 2))) + 
  geom_linerange(aes(xmin = lb, xmax = ub)) +
  geom_vline(xintercept = 2, lty=2) +
  scale_color_manual(values = c("black", "#ff66ad")) +
  theme(legend.position = "none")