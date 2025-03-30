library(ggplot2)
library(car)
library(readxl)

prostate <- read_excel("C:/Users/sharm/OneDrive/Desktop/MS/Semester - 2/Linear Regression and Time Series/Case_Study/Case_Study/Prostate_Cancer.xlsx")

ggplot(prostate, aes(x = CancerVol, y = PSA)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "PSA vs. Cancer Volume", x = "Cancer Volume (cc)", y = "PSA (ng/ml)")

summary(prostate$CancerVol)

model_initial <- lm(PSA ~ CancerVol, data = prostate)   # Initial Model
summary(model_initial)

par(mfrow = c(2, 2)) # Arrange 4 plots in a grid
plot(model_initial)

prostate$log_PSA <- log(prostate$PSA)                 # Applying Log to PSA
model_log <- lm(log_PSA ~ CancerVol, data = prostate) # Model after applying log transformation to PSA

summary(model_log)

par(mfrow = c(2, 2))
plot(model_log)

prostate_clean <- prostate[-c(95, 96, 97), ]          # Model after taking outliers

model_clean <- lm(log_PSA ~ CancerVol, data = prostate_clean) #Model after cleaning outliers and log transformation to PSA
summary(model_clean)

par(mfrow = c(2, 2))
plot(model_clean)

# Log transformation of Cancer Volume
prostate_clean$log_CancerVol <- log(prostate_clean$CancerVol)

# Fit a linear regression model with log-transformed Cancer Volume
model_log_cancervol <- lm(PSA ~ log_CancerVol, data = prostate_clean)

# Display the summary of the new model
summary(model_log_cancervol)

# Diagnostic plots for the new model
par(mfrow = c(2, 2))
plot(model_log_cancervol)

# Create a new variable for the square of Cancer Volume
prostate_clean$CancerVol_squared <- prostate_clean$CancerVol^2

# Fit a polynomial regression model (linear + quadratic term)
model_poly <- lm(PSA ~ CancerVol + CancerVol_squared, data = prostate_clean)

# Display the summary of the polynomial regression model
summary(model_poly)

# Diagnostic plots for the polynomial regression model
par(mfrow = c(2, 2))
plot(model_poly)


# Apply log transformation to both PSA and Cancer Volume   ----- Final Model
prostate_clean$log_PSA <- log(prostate_clean$PSA)
prostate_clean$log_CancerVol <- log(prostate_clean$CancerVol)

# Fit a linear regression model with log-transformed PSA and Cancer Volume
model_log_log <- lm(log_PSA ~ log_CancerVol, data = prostate_clean)

# Display the summary of the log-log model
summary(model_log_log)

# Diagnostic plots for the log-log model
par(mfrow = c(2, 2))
plot(model_log_log)

# Define new data point for prediction (log-transformed Cancer Volume)
log_cancer_vol_20 <- log(20)

# Predict log-transformed PSA level using the final log-log model
log_psa_pred_20 <- predict(model_log_log, newdata = data.frame(log_CancerVol = log_cancer_vol_20))

# Convert log-transformed prediction back to original scale (PSA level)
psa_pred_20 <- exp(log_psa_pred_20)

# Output predicted PSA level
psa_pred_20






