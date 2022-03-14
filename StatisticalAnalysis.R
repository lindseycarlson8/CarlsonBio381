# Statistical Analysis in R
# March 3 2022
# LSC 
# -----------------------------------

# Cause-and-Effect Relationships 

# measured variables x and y 
# x y (x and y do not affect each other H0)
# x -> y (x causes y) 
# y -> x (y casues x) 
# x <-> y (x and y both affect each other)
# x <- Z -> y ( x and y do not affect each other, but both are affected by variable Z)

# Types of Variables 

# discrete - distinct categories, such as species, sexes, treatment groups (character strings)
# continuous - measured with real numbers on a continuous scale, such as mass, length, concentration, volumne, frequency, relative freq (numeric)

# ------------------------------------

library(tidyverse)

#data frame construction for regression analysis
n <- 50 #number of observations (rows)

var_A <- runif(n) # random uniform (independent)
var_B <- runif(n) # random uniform (dependent)
var_C <- 5.5 + var_A*10 # a noisy linear relationship with var_A

ID <- 1:n

reg_data <- data.frame(ID, var_A,var_B, var_C)
head(reg_data)
str(reg_data)

#regression analysis in R
reg_model <- lm(var_B~var_A, data=reg_data)
print(reg_model)
str(reg_model)
head(reg_model$residuals)

#summary has the elements that we need 
summary(reg_model)

z <- unlist(summary(reg_model))

reg_stats <- list(intercept=z$coefficients1, 
                  slope=z$coefficients2,
                  intercept_p=z$coefficients7,
                  slope_p=z$coefficients8)
print(reg_stats)
reg_stats$r2
reg_stats[[5]]
reg_stats[5] # no this is just a list item

reg_plot <- ggplot(reg_data) + 
  aes(x=var_A, y=var_B) +
  geom_point() +
  stat_smooth(method=lm, se=0.99)  ## default se = 0.95

print(reg_plot)  
ggsave(filename="RegressionPlot.pdf",
       plot=reg_plot,
       device = "pdf")

# -------------------------------
# data frame construction for ANOVA 

n_group <- 3 #number of treatment groups
n_name <- c("Control", "Treatment1", "Treatment2") #names of treatment groups
n_size <- c(12, 17, 9) #sample sizes 
n_mean <-c(40, 41, 60) #mean responses
n_sd <- c(5, 5, 5) #standard deviation of each group 

ID <- 1: sum(n_size)

res_var <- c(rnorm(n=n_size[1],mean=n_mean[1], sd=n_sd[1]),
             rnorm(n=n_size[2],mean=n_mean[2], sd=n_sd[2]),
             rnorm(n=n_size[3],mean=n_mean[3], sd=n_sd[3]))

trt_group <- rep(n_name, n_size)
ano_data <- data.frame(ID, trt_group, res_var)
head(ano_data)

# analysis of variance in R
ano_model <- aov(res_var~trt_group, data=ano_data)
print(ano_model)
z <- summary(ano_model)
print(z)
flat_out <-unlist(z)
ano_stats <- list(f_ratio <- unlist(z)[7],
                  f_pval <- unlist(z)[9])
print(ano_stats)

# basic gg plot of anova data
ano_plot <- ggplot(ano_data) +
  aes(x=trt_group, y=res_var) +
  geom_boxplot()
print(ano_plot)
ggsave(filename="ANOVAPlot.pdf",
       plot=ano_plot,
       device = "pdf")
# -----------------------------------------
# Logistic Regression 
x_var <- sort(rgamma(n=200, shape=5, scale=5))
y_var <- sample(rep(c(1,0), each=100), prob=seq_len(200))
lreg_data <- data.frame(ID=1:200, xVar = x_var, yVar=y_var)
head(lreg_data)

# logistic regression analysis 
lreg_model <- glm(yVar ~ xVar, 
                  data=lreg_data,
                  family=binomial(link=logit))
summary(lreg_model)
summary(lreg_model)$coefficients

#logistic regression plot 
lreg_plot <- ggplot(lreg_data) +
  aes(x=xVar, y=yVar) +
  geom_point() +
  stat_smooth(method=glm, 
              method.args = list(family=binomial))
print(lreg_plot)

# --------------------------------------------

# contingency data are counts for different classifications 

vec_1 <- c(50, 66, 22)
vec_2 <- c(120, 22, 30)
data_matrix <- rbind(vec_1, vec_2)
rownames(data.matrix) = c