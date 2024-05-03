## Center for Research on Families Workshop
## Latent Basis Growth Curve Modeling
## Author: Ann Folker, M.S.
## February 23, 2024

##### SET WORKING DIRECTORY, LOAD NECESSARY PACKAGES, AND IMPORT DATA ####
setwd("/Users/annfolker/Documents/UMass Amherst/CRF/Latent Basis Seminar/Model Syntax")
  # set to file path where you have the data saved on your computer

dat <- read.csv("CRF Latent Basis Workshop Data_R.csv", header = T)
  # read in data

if(!require(psych)){install.packages("psych")}
if(!require(lavaan)){install.packages("lavaan")}
  # checks to see if you have a given package and installs it if you do NOT have the package already installed

library(psych)
library(lavaan)
  # load packages using library function once installed

##### RUNNING LINEAR MODEL #####
model_lin <- ' # Measurement model:
               i =~ 1*pub_11 + 1*pub_12 + 1*pub_13 + 1*pub_14 + 1*pub_15
               s =~ 0*pub_11 + 1*pub_12 + 2*pub_13 + 3*pub_14 + 4*pub_15 
               
              # Residual variances fixed to be equal:
               pub_11 ~~ z*pub_11
               pub_12 ~~ z*pub_12
               pub_13 ~~ z*pub_13
               pub_14 ~~ z*pub_14
               pub_15 ~~ z*pub_15 '
  # specify the model

fit_model_lin <- growth(model_lin, data = dat, missing = "ML")
  # fit linear model using 'growth' function in lavaan package
  # missing = ML imputes missing values

summary(fit_model_lin)
  # call summary of model to get parameter estimates

fitMeasures(fit_model_lin, c("aic", "bic"))
  # fit statistics

##### RUNNING LATENT BASIS MODEL #####
model_latent <- ' # Measurement model:
                  i =~ 1*pub_11 + 1*pub_12 + 1*pub_13 + 1*pub_14 + 1*pub_15
                  s =~ 0*pub_11 + a*pub_12 + b*pub_13 + c*pub_14 + 1*pub_15
                  
                  # Residual variances fixed to be equal:
                  pub_11 ~~ z*pub_11
                  pub_12 ~~ z*pub_12
                  pub_13 ~~ z*pub_13
                  pub_14 ~~ z*pub_14
                  pub_15 ~~ z*pub_15 '
  # specify the model
  # fix first and last growth factors to 0 and 1, then use letters to allow other paths to vary freely

fit_model_latent <- growth(model_latent, data = dat, missing = "ML")

summary(fit_model_latent)

fitMeasures(fit_model_latent, c("aic", "bic"))
  # fit statistics

##### RUNNING QUADRATIC MODEL #####
model_quad <- ' # Measurement model:
                i =~ 1*pub_11 + 1*pub_12 + 1*pub_13 + 1*pub_14 + 1*pub_15
                s =~ 0*pub_11 + 1*pub_12 + 2*pub_13 + 3*pub_14 + 4*pub_15
                q =~ 0*pub_11 + 1*pub_12 + 4*pub_13 + 9*pub_14 + 16*pub_15 
                
                # Residual variances fixed to be equal:
                  pub_11 ~~ z*pub_11
                  pub_12 ~~ z*pub_12
                  pub_13 ~~ z*pub_13
                  pub_14 ~~ z*pub_14
                  pub_15 ~~ z*pub_15 '

fit_model_quad <- growth(model_quad, data = dat, missing = "ML")

summary(fit_model_quad)

fitMeasures(fit_model_quad, c("aic", "bic"))

##### CREATING FIGURES #####
dat_long <- dat %>%
  pivot_longer(cols = 2:7,
               names_to = "time",
               values_to = "Puberty")
  # First, pivot data to long format

dat_long$time <- rep(c(1, 2, 3, 4, 5, 6),
                     length.out = length(dat_long$ID))

dat_long$age <- rep(c(11, 12, 13, 14, 15, 16),
                    length.out = length(dat_long$ID))
  # Create columns for time and age

# Simple plot of mean at each time
means=ddply(dat_long, .(time), summarize,  mean=mean(Puberty, na.rm=TRUE))
plot(means, ylim=c(1,4), 
     xlab = "Time",
     ylab = "Pubertal Status",
     main = "Plot of means for Pubertal Status",
     pch = 16)

# Plot of all individual profiles in one set with average line
dat_long %>% 
  ggplot(aes(age, Puberty, group = ID)) +
  geom_line(alpha = 0.1) +                      
  stat_smooth(aes(group=1),                     
              method = "lm", formula = y ~ x + I(x^2), size = 1.5,
              level = 0.95,
              color = "blue") +
  theme_classic() +
  labs(x = "Age", y = "Pubertal Status", title = "Pubertal Status")