library(ggplot2)
library(broom)
library(kableExtra)
library(knitr)
mod_table <- data.frame("Assumptions" = 
                          c("Linearity",
                            "Constant Variance",
                            "Normality",
                            "Correlated Errors" ),
                        "Checking Assumptions" = 
                          c("Residuals vs. Fits Plots",
                            "Residuals vs. Fits Plots",
                            "Q-Q Plot",
                            "Structure of the data" ),
                        "Fixing Violations" = 
                          c("Transformations",
                            "Weighted/Generalized Least Squares (WLS/GLS)",
                            "Robust Regression",
                            "Generalized Least Squares (GLS)" )
)

kable(mod_table, col.names = 
        c("Assumptions", "Checking Assumptions", "Fixing Violated Assumptions")) %>%
  kable_styling(full_width = FALSE, font_size = 14, bootstrap_options = "striped") %>%
  collapse_rows(1) %>%
  row_spec(0, color = "darkseagreen") %>%
  column_spec(1, color = "slategray")
mod_table <- data.frame("Unusual Observations" = 
                          c("Leverage",
                            "Outliers",
                            "Influential Points"),
                        "Calculated" = 
                          c("$\\underbrace{H_{ii} = h_i}_{\\text{diagonals of the hat matrix}}$",
                            "$\\underbrace{r_i = \\frac{e_i}{s_e\\sqrt{1 - h_i}}}_{\\text{standardized residual}}$",
                            "$\\underbrace{D_i = \\frac{1}{p}r_i^2\\frac{h_i}{1-{h_i}}}_{\\text{Cook's Distance}}$")
)

kable(mod_table, col.names = 
        c("Unusual Observations", "Calculated As")) %>%
  kable_styling(full_width = FALSE, font_size = 14, bootstrap_options = "striped") %>%
  collapse_rows(1) %>%
  row_spec(0, color = "darkseagreen") %>%
  column_spec(1, color = "slategray")
library(faraway) 
library(visdat)
data("mammalsleep")
?mammalsleep
vis_dat(mammalsleep)
df <- mammalsleep[(!is.na(mammalsleep$sleep) &
                     !is.na(mammalsleep$lifespan) &
                     !is.na(mammalsleep$gestation)),
]
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
modLog <- lm(log(sleep) ~ body + brain + lifespan + gestation +
               predation + exposure + danger, 
             data = df)
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
ggplot(mod_table[-1, ], aes(x = term, y = estimate,
                            ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() + 
  geom_hline(yintercept = 0, lty = 2, color = "aquamarine4") +
  coord_flip()
mod <- lm(sleep ~ body + brain + lifespan + gestation +
            predation + danger, 
          data = df)

mod_table <- tidy(mod, conf.int = TRUE)
kable(mod_table, digits = 2, 
      col.names = c("Term", "Estimate", "SE", "Statistic", "p-value", "Lower Bound", "Upper Bound")) %>%
  kable_styling(full_width = FALSE, 
                font_size = 13,
                bootstrap_options = "striped") %>%
  collapse_rows(1, latex_hline = 'major') %>%
  column_spec(1, color = "orchid") %>%
  row_spec(0, font_size=14, color = "darkslategray") %>%
  add_header_above(c(" " = 5, "95% Confidence Interval" = 2), 
                   color = c("black", "slategray"))