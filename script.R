library(knitr)
library(kableExtra)

## Part 1a.
library(ISLR)
hitters_cc <- Hitters[complete.cases(Hitters), ]

small <- lm(Salary ~ 1, data = hitters_cc)
larger <- lm(Salary ~ AtBat + Hits, data = hitters_cc)

rss_small <- summary(small)$sigma^2 * (nrow(hitters_cc) - 1)
rss_larger <- summary(larger)$sigma^2 * (nrow(hitters_cc) - 3)

f <- ((rss_small - rss_larger) / 2) / 
  (rss_larger / (nrow(hitters_cc) - 3))

Pr_f <- 1 - pf(f, 2, nrow(hitters_cc) - 3)

a <- anova(small,larger)
rownames(a) <- c("Model 1: Salary ~ 1", "Model 2: Salary ~ AtBat + Hits")

kable(a, row.names = TRUE) %>%
  kable_styling(font_size = 11, full_width = FALSE, bootstrap_options = "striped") %>%
  column_spec(1, bold = TRUE)



## Part 1b.

small <- lm(Salary ~ AtBat, data = hitters_cc)
larger <- lm(Salary ~ AtBat + Hits, data = hitters_cc)

a <- anova(small, larger)
rownames(a) <- c("Model 1: Salary ~ AtBat", "Model 2: Salary ~ AtBat + Hits")
kable(a, row.names = TRUE) %>%
  kable_styling(font_size = 11, full_width = FALSE, bootstrap_options = "striped") %>%
  column_spec(1, bold = TRUE)

summary(larger)


b2 <- broom::tidy(summary(larger))
kable(b2) %>%
  kable_styling(font_size = 11, full_width = FALSE, bootstrap_options = "striped") %>%
  column_spec(1, bold = TRUE)


## Part 1c.
library(purrr)
library(ggplot2)

# Sample 1
set.seed(7)
n <- 100 ## sample 100 people
sample <- data.frame(Age = rnorm(n,  mean = 30,  sd = 10))
sample$Wage <- 2 * sample$Age + rnorm(n, mean = 0, sd = 10)

# Sample 2
n <- 100
sample2 <- data.frame(Age = rnorm(n, mean = 30, sd = 10))
sample2$Wage <- 2 * sample2$Age + rnorm(n, 0, 10) 

Model1 <- lm(Wage ~ Age, data = sample) ## Sample 1
Model2 <- lm(Wage ~ Age, data = sample2) ## Sample 2

confint(Model1)
confint(Model2)

kable(confint(Model1), valign = TRUE) %>%
  kable_styling(font_size = 11, full_width = FALSE, bootstrap_options = c("striped"), position = "float_left") %>%
  add_header_above(c("Model 1" = 3), align = "right")

kable(confint(Model2), valign = TRUE) %>%
  kable_styling(font_size = 11, full_width = FALSE, bootstrap_options = c("striped"), position = "left") %>%
  add_header_above(c("Model 2" = 3), align = "right")


get_ci <- function(id) {
  sample <- data.frame(Age = rnorm(n, mean = 30, sd = 10))
  sample$Wage <- 2* sample$Age + rnorm(n, 0, 10)
  model <- lm(Wage ~ Age, data = sample)
  return(
    data.frame(
      lb = confint(model)[2,1],
      ub = confint(model)[2,2],
      id = id
    ))}
set.seed(7)
ci <- map_df(1:100, get_ci) ## map function call 100 times


ggplot(ci, aes(y = id, color = (lb > 2 | ub < 2))) + 
  geom_linerange(aes(xmin = lb, xmax = ub)) +
  geom_vline(xintercept = 2, lty=2) +
  scale_color_manual(values = c("black", "#ff66ad")) +
  theme(legend.position = "none")


##################################################

## Part 2a.
library(ggplot2)
library(broom)
library(kableExtra)
library(knitr)
library(Rcpp)
library(faraway) 
library(visdat)


library(faraway)
library(visdat)
data(mammalsleep)

vis_dat(mammalsleep)

df <- mammalsleep[(!is.na(mammalsleep$sleep) &
                     !is.na(mammalsleep$lifespan) &
                     !is.na(mammalsleep$gestation)),]

mod <- lm(sleep ~ body + brain + lifespan + gestation +
            predation + exposure + danger, 
          data = df)

check <- data.frame(
  e = resid(mod),
  fit = fitted(mod)
)

ggplot(check, aes(x = fit, y = e)) +
  geom_point() +
  geom_hline(yintercept = 0, 
             col = "plum") +
  labs(y = "Residuals", x = "Fitted Values") +
  theme_minimal()

ggplot(check, aes(sample = e)) +
  geom_qq() + 
  geom_qq_line(color="darkseagreen") +
  theme_minimal()

modLog <- lm(log(sleep) ~ body + brain + lifespan + 
               gestation + predation + exposure + 
               danger, data = df)


checkLog <- data.frame(
  e = resid(modLog),
  fit = fitted(modLog)
)

ggplot(checkLog, aes(x = fit, y = e)) +
  geom_point() +
  geom_hline(yintercept = 0, 
             col = "plum") +
  labs(y = "Residuals", x = "Fitted Values") +
  theme_minimal()

ggplot(checkLog, aes(sample = e)) +
  geom_qq() + 
  geom_qq_line(color="darkseagreen") +
  theme_minimal()


mod <- lm(sleep ~ body + brain + lifespan + gestation +
            predation + exposure + danger, 
          data = df)

mod_table <- tidy(mod, conf.int = TRUE)

knitr::kable(mod_table) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "condensed"), font_size = 11)

ggplot(mod_table[-1, ], aes(x = term, y = estimate,
                            ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() + 
  geom_hline(yintercept = 0, lty = 2, color = "aquamarine4") +
  coord_flip()


mod <- lm(sleep ~ body + brain + lifespan + gestation +
            predation + danger, data = df)

mod_table <- tidy(mod, conf.int = TRUE)

kable(mod_table, digits = 2, col.names = c("Term", "Estimate", "SE", "Statistic", "p-value", "Lower Bound", "Upper Bound")) %>%
  kable_styling(full_width = FALSE, font_size = 11, bootstrap_options = c("striped", "condensed")) %>%
  add_header_above(c(" " = 5, "95% Confidence Interval" = 2))


mod_table1 <- data.frame("Assumptions" = 
                           c("Linearity",
                             "Constant Variance",
                             "Normality",
                             "Correlated Error" ),
                         "Checking Assumptions" = 
                           c("Residuals vs. Fits Plot",
                             "Residuals vs. Fits Plot",
                             "Q-Q Plot",
                             "Structure of the data" ),
                         "Fixing Violations" = 
                           c("Transformations",
                             "Weighted Least Squares",
                             "Robust Regression",
                             "Generalized Least Squares" )
)

mod_table2 <- data.frame("Unusual Observations" = 
                           c("Leverage",
                             "Outliers",
                             "Influential Points"),
                         "Calculated" = 
                           c("$\\small\\underbrace{H_{ii} = h_i}_{\\text{diagonals of the hat matrix}}$",
                             "$\\underbrace{r_i = \\frac{e_i}{s_e\\sqrt{1 - h_i}}}_{\\text{standardized residual}}$",
                             "$\\underbrace{D_i = \\frac{1}{p}r_i^2\\frac{h_i}{1-{h_i}}}_{\\text{Cook's Distance}}$")
)

kable(mod_table1, col.names = c("Assumptions", " Checking Assumptions", "Fixing Violated")) %>%
  collapse_rows(1, valign = "bottom") %>%
  kable_styling(full_width = FALSE, font_size = 12, bootstrap_options = c("striped"), position = "float_left") %>%
  column_spec(1, color = "slategray")

kable(mod_table2, col.names = c("Unusual Observations", "Calculated As")) %>%
  kable_styling(full_width = FALSE, font_size = 12, bootstrap_options = c("striped"), position = "left") %>%
  column_spec(1, color = "slategray")


#####################################################
#####################################################
#####################################################


library(knitr)
library(kableExtra)

## Part 1a.
library(ISLR)
hitters_cc <- Hitters[complete.cases(Hitters), ]

small <- lm(Salary ~ 1, data = hitters_cc)
larger <- lm(Salary ~ AtBat + Hits, data = hitters_cc)

rss_small <- summary(small)$sigma^2 * (nrow(hitters_cc) - 1)
rss_larger <- summary(larger)$sigma^2 * (nrow(hitters_cc) - 3)

f <- ((rss_small - rss_larger) / 2) / 
  (rss_larger / (nrow(hitters_cc) - 3))

Pr_f <- 1 - pf(f, 2, nrow(hitters_cc) - 3)

a <- anova(small,larger)
rownames(a) <- c("Model 1: Salary ~ 1", "Model 2: Salary ~ AtBat + Hits")

kable(a, row.names = TRUE) %>%
  kable_styling(font_size = 11, full_width = FALSE, bootstrap_options = "striped") %>%
  column_spec(1, bold = TRUE)



## Part 1b.

small <- lm(Salary ~ AtBat, data = hitters_cc)
larger <- lm(Salary ~ AtBat + Hits, data = hitters_cc)

a <- anova(small, larger)
rownames(a) <- c("Model 1: Salary ~ AtBat", "Model 2: Salary ~ AtBat + Hits")
kable(a, row.names = TRUE) %>%
  kable_styling(font_size = 11, full_width = FALSE, bootstrap_options = "striped") %>%
  column_spec(1, bold = TRUE)

summary(larger)


b2 <- broom::tidy(summary(larger))
kable(b2) %>%
  kable_styling(font_size = 11, full_width = FALSE, bootstrap_options = "striped") %>%
  column_spec(1, bold = TRUE)


## Part 1c.
library(purrr)
library(ggplot2)

# Sample 1
set.seed(7)
n <- 100 ## sample 100 people
sample <- data.frame(Age = rnorm(n,  mean = 30,  sd = 10))
sample$Wage <- 2 * sample$Age + rnorm(n, mean = 0, sd = 10)

# Sample 2
n <- 100
sample2 <- data.frame(Age = rnorm(n, mean = 30, sd = 10))
sample2$Wage <- 2 * sample2$Age + rnorm(n, 0, 10) 

Model1 <- lm(Wage ~ Age, data = sample) ## Sample 1
Model2 <- lm(Wage ~ Age, data = sample2) ## Sample 2

confint(Model1)
confint(Model2)

kable(confint(Model1), valign = TRUE) %>%
  kable_styling(font_size = 11, full_width = FALSE, bootstrap_options = c("striped"), position = "float_left") %>%
  add_header_above(c("Model 1" = 3), align = "right")

kable(confint(Model2), valign = TRUE) %>%
  kable_styling(font_size = 11, full_width = FALSE, bootstrap_options = c("striped"), position = "left") %>%
  add_header_above(c("Model 2" = 3), align = "right")


get_ci <- function(id) {
  sample <- data.frame(Age = rnorm(n, mean = 30, sd = 10))
  sample$Wage <- 2* sample$Age + rnorm(n, 0, 10)
  model <- lm(Wage ~ Age, data = sample)
  return(
    data.frame(
      lb = confint(model)[2,1],
      ub = confint(model)[2,2],
      id = id
    ))}
set.seed(7)
ci <- map_df(1:100, get_ci) ## map function call 100 times


ggplot(ci, aes(y = id, color = (lb > 2 | ub < 2))) + 
  geom_linerange(aes(xmin = lb, xmax = ub)) +
  geom_vline(xintercept = 2, lty=2) +
  scale_color_manual(values = c("black", "#ff66ad")) +
  theme(legend.position = "none")


## Part 2a.
library(ggplot2)
library(broom)
library(kableExtra)
library(knitr)
library(Rcpp)
library(faraway) 
library(visdat)


library(faraway)
library(visdat)
data(mammalsleep)

vis_dat(mammalsleep)

df <- mammalsleep[(!is.na(mammalsleep$sleep) &
                     !is.na(mammalsleep$lifespan) &
                     !is.na(mammalsleep$gestation)),]

mod <- lm(sleep ~ body + brain + lifespan + gestation +
            predation + exposure + danger, 
          data = df)

check <- data.frame(
  e = resid(mod),
  fit = fitted(mod)
)

ggplot(check, aes(x = fit, y = e)) +
  geom_point() +
  geom_hline(yintercept = 0, 
             col = "plum") +
  labs(y = "Residuals", x = "Fitted Values") +
  theme_minimal()

ggplot(check, aes(sample = e)) +
  geom_qq() + 
  geom_qq_line(color="darkseagreen") +
  theme_minimal()

modLog <- lm(log(sleep) ~ body + brain + lifespan + 
               gestation + predation + exposure + 
               danger, data = df)


checkLog <- data.frame(
  e = resid(modLog),
  fit = fitted(modLog)
)

ggplot(checkLog, aes(x = fit, y = e)) +
  geom_point() +
  geom_hline(yintercept = 0, 
             col = "plum") +
  labs(y = "Residuals", x = "Fitted Values") +
  theme_minimal()

ggplot(checkLog, aes(sample = e)) +
  geom_qq() + 
  geom_qq_line(color="darkseagreen") +
  theme_minimal()


mod <- lm(sleep ~ body + brain + lifespan + gestation +
            predation + exposure + danger, 
          data = df)

mod_table <- tidy(mod, conf.int = TRUE)

knitr::kable(mod_table) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "condensed"), font_size = 11)

ggplot(mod_table[-1, ], aes(x = term, y = estimate,
                            ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() + 
  geom_hline(yintercept = 0, lty = 2, color = "aquamarine4") +
  coord_flip()


mod <- lm(sleep ~ body + brain + lifespan + gestation +
            predation + danger, data = df)

mod_table <- tidy(mod, conf.int = TRUE)

kable(mod_table, digits = 2, col.names = c("Term", "Estimate", "SE", "Statistic", "p-value", "Lower Bound", "Upper Bound")) %>%
  kable_styling(full_width = FALSE, font_size = 11, bootstrap_options = c("striped", "condensed")) %>%
  add_header_above(c(" " = 5, "95% Confidence Interval" = 2))


mod_table1 <- data.frame("Assumptions" = 
                           c("Linearity",
                             "Constant Variance",
                             "Normality",
                             "Correlated Error" ),
                         "Checking Assumptions" = 
                           c("Residuals vs. Fits Plot",
                             "Residuals vs. Fits Plot",
                             "Q-Q Plot",
                             "Structure of the data" ),
                         "Fixing Violations" = 
                           c("Transformations",
                             "Weighted Least Squares",
                             "Robust Regression",
                             "Generalized Least Squares" )
)

mod_table2 <- data.frame("Unusual Observations" = 
                           c("Leverage",
                             "Outliers",
                             "Influential Points"),
                         "Calculated" = 
                           c("$\\small\\underbrace{H_{ii} = h_i}_{\\text{diagonals of the hat matrix}}$",
                             "$\\underbrace{r_i = \\frac{e_i}{s_e\\sqrt{1 - h_i}}}_{\\text{standardized residual}}$",
                             "$\\underbrace{D_i = \\frac{1}{p}r_i^2\\frac{h_i}{1-{h_i}}}_{\\text{Cook's Distance}}$")
)

kable(mod_table1, col.names = c("Assumptions", " Checking Assumptions", "Fixing Violated")) %>%
  collapse_rows(1, valign = "bottom") %>%
  kable_styling(full_width = FALSE, font_size = 12, bootstrap_options = c("striped"), position = "float_left") %>%
  column_spec(1, color = "slategray")

kable(mod_table2, col.names = c("Unusual Observations", "Calculated As")) %>%
  kable_styling(full_width = FALSE, font_size = 12, bootstrap_options = c("striped"), position = "left") %>%
  column_spec(1, color = "slategray")

