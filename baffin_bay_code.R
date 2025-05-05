# Libraries
library(rethinking)
library(corrplot)
library(ggplot2)
library(tidyr)
library(dplyr)
library(GGally)
library(readxl)
library(car)


###############################################################################
# Phase-1: Understanding data
###############################################################################
# Step-1: Loading the dataset
# Step-2: Visualizations of all variables in the dataset.
# Step-3: Feature selection
# Step-4: Variable Transformations (log + Standardization)
# Step-5: Final Visualizations to check for any inconsistencies
###############################################################################


# Step-1: Load the dataset
################################################################
Baffin <- read_excel("~/SIDDHARTHA/MASTER'S TAMUCC/COURSES/SPRING 2025/DASC 5304 Bayesian/Baffin_bay.xlsx")
#View(Baffin)
precis(Baffin)
#summary(Baffin)
Baffin_num <- Baffin[,-c(1,2)]
#View(Baffin_num)


# Step-2. Visualizations of all variables in the dataset.
# Boxplots
# Histogram
# Correlation plot
# Scatterplot
################################################################

dir.create("plots")
# Boxplots
long_data <- pivot_longer(Baffin_num, cols = where(is.numeric), names_to = "Variable", values_to = "Value")

box_plot <- ggplot(long_data, aes(x = "", y = Value)) +
  geom_boxplot(fill = "#1ff64d", color = "#1f4af6") +
  facet_wrap(~ Variable, scales = "free", ncol = 4) +
  theme_minimal() +
  labs(title = "Side by Side Boxplots of all numerical variables in the dataset", x = "", y = "Value")
ggsave("plots/boxplots.png", box_plot, width = 10, height = 6, dpi = 300)
################################################

# Histogram: Distiribution of salinity
sal_hist <- ggplot(Baffin, aes(x = Salinity)) +
  geom_histogram(binwidth = 2, fill = "#1283d7", color = "#0c0c0c") +
  theme_minimal() +
  labs(title = "Distribution of Salinity in Baffin Bay", x = "Salinity", y = "Count")
ggsave("plots/hist_salinity.png", sal_hist, width = 6, height = 4, dpi = 300)
################################################

# Histograms: all other variables
baffin_long <- pivot_longer(Baffin_num, cols = everything(), names_to = "Variable", values_to = "Value")

all_hist <- ggplot(baffin_long, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "#1855c7", color = "white") +
  facet_wrap(~ Variable, scales = "free", ncol = 4) +
  theme_minimal() +
  labs(title = "Histograms of All Numeric Variables in Baffin Bay", x = "Value", y = "Frequency")
ggsave("plots/hist_all_vars.png", all_hist, width = 10, height = 6, dpi = 300)
################################################

# Correlation plot
Cor_Baffin <- cor(Baffin_num)
Cor_Baffin
png("plots/correlation_plot.png", width = 1000, height = 800)
corrplot(Cor_Baffin, method = "number", type = "lower", bg = "#060606", add = FALSE, tl.pos = "lt", number.cex = 0.8)
corrplot(Cor_Baffin, method = "circle", type = "upper", bg = "#060606", add = TRUE,  tl.col = "blank")
dev.off()
################################################

# Scatterplot
png("plots/scatterplot_matrix.png", width = 1000, height = 800)
ggpairs(Baffin_num,
        title = "Scatterplot Matrix of Baffin Bay Variables",
        upper = list(continuous = wrap("cor", size = 3)),
        lower = list(continuous = wrap("points", alpha = 0.6)),
        diag = list(continuous = wrap("densityDiag")))
dev.off()
################################################


# Step-3. Feature Selection
# Using Variance Information Factor (vif) for checking multicollinearity.
# selecting and subsetting final variables
################################################################
lm_vif1 <- lm(Salinity ~ Conductivity + `DO_mg/L` + `DO%` + pH + Temp + DOC + TDN + TOC + TN + ammonium + 
                         nitrite + orthop + silicate + chl_whole + `chl_<20` + `chl_<3`, data = Baffin_num)
vif_values1 <- vif(lm_vif1)
print(vif_values1)

# removing DO_mg/L, DOC, and TDN due to their multicolliniearity with variables DO%, TOC and TN respectively.
# removing conductivity due to its high correaltion with salinity (0.96)
# removing chl_whole, `chl_<20`, and `chl_<3` because they are post treatment variables, 
#       chlorophyll is a direct effect of the salinity, less salinity --> freshwater --> neutrients --> algae --> chlorophyll
# removing the variables nitrite, orthop, and ammonium due to their abnormal distributions and outliers. also non relevance in predicting salinity

# final varibales vif
lm_vif2 <- lm(Salinity ~ `DO%` + pH + Temp + TOC + TN + silicate, data = Baffin_num)
vif_values2 <- vif(lm_vif2)
print(vif_values2)

# Subsetting selecting variables
Baffin_sel <- Baffin_num[ ,c("Salinity", "DO%", "pH", "Temp", "TOC", "TN", "silicate")]


# Step-4: Variable Transformations (log + Standardization)
################################################################
standardize <- function(x) {(x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)}

Baffin_trans <- Baffin_sel %>% mutate(log_TOC = log(TOC + 1), log_TN = log(TN + 1), log_silicate = log(silicate + 1)) %>% 
                               mutate(Salinity = standardize(Salinity),
                                      Temp = standardize(Temp),
                                      `DO%` = standardize(`DO%`),
                                      pH = standardize(pH),
                                      log_TOC = standardize(log_TOC),
                                      log_TN = standardize(log_TN),
                                      log_silicate = standardize(log_silicate)) %>% 
                               select(Salinity, Temp, `DO%`, pH, log_TOC, log_TN, log_silicate)
head(Baffin_trans)


# Step-5: Final Visualizations to check for any inconsistencies.
# Boxplots
# Histograms
# Correlation Plot
# Scatter plots
################################################################
# Transformed Boxplots
long_data2 <- pivot_longer(Baffin_trans, cols = where(is.numeric), names_to = "Variable", values_to = "Value")

ggplot(long_data2, aes(x = "", y = Value)) +
  geom_boxplot(fill = "#1ff64d", color = "#1f4af6") +
  facet_wrap(~ Variable, scales = "free", ncol = 4) + 
  theme_minimal() +
  labs(title = "Side by Side Boxplots of final transformed numerical variables in the dataset", x = "", y = "Value")
ggsave("plots/transformed_boxplots.png", box_plot, width = 10, height = 6, dpi = 300)
################################################


# Transformed Histograms
baffin_long2 <- pivot_longer(Baffin_trans, cols = everything(), names_to = "Variable", values_to = "Value")

ggplot(baffin_long2, aes(x = Value)) +
  geom_histogram(bins = 10, fill = "#1855c7", color = "white") +
  facet_wrap(~ Variable, scales = "free", ncol = 4) +
  theme_minimal() +
  labs(title = "Histograms of selected Transformed Numeric Variables in Baffin Bay",
       x = "Value", y = "Frequency")
ggsave("plots/selected_transformed_variables_histograms.png", all_hist, width = 10, height = 6, dpi = 300)
################################################


# Transformed Correaltion plot
Cor_Baffin_t <- cor(Baffin_trans)
Cor_Baffin_t
png("plots/transformed_correlation_plot.png", width = 1000, height = 800)
corrplot(Cor_Baffin_t, method = "number", type = "lower", bg = "#060606",add = FALSE, tl.pos = "lt", number.cex = 0.8)
corrplot(Cor_Baffin_t, method = "circle", type = "upper", bg = "#060606",add = TRUE,  tl.col = "blank")         
dev.off()
################################################


# Transformed Scatterplots
png("plots/transformed_scatterplot_matrix.png", width = 1000, height = 800)
ggpairs(Baffin_trans,
        title = "Scatterplot Matrix of Baffin Bay Variables",
        upper = list(continuous = wrap("cor", size = 3)),
        lower = list(continuous = wrap("points", alpha = 0.6)),
        diag = list(continuous = wrap("densityDiag")))
dev.off()
################################################

##############################################################################
# END OF PHASE-1
##############################################################################





###############################################################################
# Phase 2: Model Development and Comparision
###############################################################################
# Step-1: Prior Predictive check
# Step-2: Model-1: ulam() model with all selected variables.
# Step-3: Model-2: ulam() model with 'silicate' variable removed.
# Step-4: Model-3: ulam() model with 'silicate' and 'DO_prec' removed.
# Step-5: Comparision of all 3 models using WAIC and PSIS
###############################################################################


# Step-1: Prior Predictive check
###################################################################
# prior choice 1: alpha = normal(0,1); b_coeffi = normal(0,0.5); sigma = expo(1)
# prior choice 2: alpha = normal(0,1); b_coeffi = normal(0,1); sigma = expo(1)
# prior choice 3: alpha = normal(0,1.5); b_coeffi = normal(0,1); sigma = expo(1)
# prior choice 4: alpha = normal(0,2); b_coeffi = normal(0,1); sigma = expo(1)
# prior choice 5: alpha = normal(0,1.5); b_coeffi = normal(0,1); sigma = expo(1)
# prior choice 6: alpha = normal(0,1.5); b_coeffi = normal(0,0.5); sigma = expo(1)
# prior choice 7: alpha = normal(0,1.5); b_coeffi = normal(0,0.5); sigma = expo(2)

# Renaming the variable DO% to DO_perc to make it make it ulam() compatible
Baffin_trans1 <- Baffin_trans %>% rename(DO_perc = `DO%`)

model2_prior <- ulam(alist(Salinity ~ normal(mu, sigma),
                           mu <- alpha + b_temp*Temp + b_do*DO_perc + b_pH*pH +
                             b_TOC*log_TOC + b_TN*log_TN + b_silicate*log_silicate,
                           alpha ~ normal(0, 1.5),
                           b_temp ~ normal(0, 0.5),
                           b_do ~ normal(0, 0.5),
                           b_pH ~ normal(0, 0.5),
                           b_TOC ~ normal(0, 0.5),
                           b_TN ~ normal(0, 0.5),
                           b_silicate ~ normal(0, 0.5),
                           sigma ~ exponential(2)),
                     data = Baffin_trans1,
                     chains = 4,
                     cores = 4,
                     sample_prior = TRUE)

prior_sim <- extract.samples(model2_prior)
mu_prior <- prior_sim$alpha +
  prior_sim$b_temp * Baffin_trans1$Temp +
  prior_sim$b_do * Baffin_trans1$DO_perc +
  prior_sim$b_pH * Baffin_trans1$pH +
  prior_sim$b_TOC * Baffin_trans1$log_TOC +
  prior_sim$b_TN * Baffin_trans1$log_TN +
  prior_sim$b_silicate * Baffin_trans1$log_silicate

N <- nrow(Baffin_trans1)
y_prior_pred <- matrix(NA, nrow = length(prior_sim$sigma), ncol = N)
for (i in 1:length(prior_sim$sigma)) {
  y_prior_pred[i, ] <- rnorm(N, mu_prior[i, ], prior_sim$sigma[i])}

# Plotting prior predictive salinity values
prior_df <- data.frame(SimulatedSalinity = as.vector(y_prior_pred))

png("plots/prior_predictive_distribution.png", width = 1000, height = 800)
print(ggplot(prior_df, aes(x = SimulatedSalinity)) +
        geom_histogram(bins = 40, fill = "#0073C2FF", alpha = 0.6) +
        geom_vline(xintercept = mean(Baffin_trans1$Salinity), color = "red", linetype = "dashed") +
        labs(title = "Prior Predictive Distribution of Salinity",
             subtitle = "Blue: Simulated from prior | Red: Mean of real (standardized) salinity",
             x = "Simulated Salinity", y = "Frequency") +
        theme_minimal())
dev.off()

# Step-2: Model-1: ulam() model with all selected variables.
###################################################################
model1 <- ulam(alist(Salinity ~ normal(mu, sigma),
                     mu <- alpha + b_temp*Temp + b_do*DO_perc + b_pH*pH + 
                                   b_TOC*log_TOC + b_TN*log_TN + b_silicate*log_silicate,
                           alpha ~ normal(0, 1.5),
                           b_temp ~ normal(0, 0.5),
                           b_do ~ normal(0, 0.5),
                           b_pH ~ normal(0, 0.5),
                           b_TOC ~ normal(0, 0.5),
                           b_TN ~ normal(0, 0.5),
                           b_silicate ~ normal(0, 0.5),
                     sigma ~ exponential(2)),
               data = Baffin_trans1,
               chains = 4, 
               cores = 4,
               log_lik = TRUE)
precis(model1)


# Step-3: Model-2: ulam() model with 'silicate' variable removed.
###################################################################
model2 <- ulam(alist(Salinity ~ normal(mu, sigma),
                     mu <- alpha + b_temp*Temp + b_do*DO_perc + b_pH*pH + 
                                   b_TOC*log_TOC + b_TN*log_TN,
                           alpha ~ normal(0, 1.5),
                           b_temp ~ normal(0, 0.5),
                           b_do ~ normal(0, 0.5),
                           b_pH ~ normal(0, 0.5),
                           b_TOC ~ normal(0, 0.5),
                           b_TN ~ normal(0, 0.5),
                     sigma ~ exponential(2)),
               data = Baffin_trans1,
               chains = 4, 
               cores = 4,
               log_lik = TRUE)
precis(model2)


# Step-4: Model-3: ulam() model with 'silicate' and 'DO_prec' removed.
###################################################################
model3 <- ulam(alist(Salinity ~ normal(mu, sigma),
                     mu <- alpha + b_temp*Temp + b_pH*pH + b_TOC*log_TOC + b_TN*log_TN,
                           alpha ~ normal(0, 1.5),
                           b_temp ~ normal(0, 0.5),
                           b_pH ~ normal(0, 0.5),
                           b_TOC ~ normal(0, 0.5),
                           b_TN ~ normal(0, 0.5),
                     sigma ~ exponential(2)),
               data = Baffin_trans1,
               chains = 4, 
               cores = 4,
               log_lik = TRUE)
precis(model3)


# Step-5: Comparision of all 3 models using WAIC and PSIS.
###################################################################
compare(model1, model2, model3, func = PSIS)
compare(model1, model2, model3, func = WAIC)

# decided to go ahead with the model-2

##############################################################################
# END OF PHASE-2
##############################################################################




###############################################################################
# Phase 3: Model checks and Interpretation
###############################################################################
# Step-1: Simulating Posterior Predictive Distributions.
# Step-2: Reversing the Transformations for Interpretation.
###############################################################################


# Step-1: Simulating Posterior Predictive Distributions.
###################################################################
sim_sal <- sim(model2, data = Baffin_trans1)
mu_link <- link(model2, data = Baffin_trans1)
mu_mean <- apply(mu_link, 2, mean)
mu_PI <- apply(mu_link, 2, PI, prob = 0.89)
sal_PI <- apply(sim_sal, 2, PI, prob = 0.89)

plot_df <- data.frame(Observed = Baffin_trans1$Salinity, Predicted = mu_mean,
                      PI_low = sal_PI[1,], PI_high = sal_PI[2,])

ggplot(plot_df, aes(x = Observed, y = Predicted)) +
       geom_point(color = "darkblue", size = 2, alpha = 0.6) +
       geom_errorbar(aes(ymin = PI_low, ymax = PI_high), alpha = 0.2) +
       geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
       labs(title = "Posterior Predictive Check: Salinity",
            x = "Observed (Standardized)", y = "Predicted (Posterior Mean)") +
theme_minimal()


# Step-2: Reversing the Transformations for Interpretation.
###################################################################
# Raw means and SDs from Baffin_sel 
mean_temp <- mean(Baffin_sel$Temp, na.rm = TRUE)
sd_temp   <- sd(Baffin_sel$Temp, na.rm = TRUE)

mean_do <- mean(Baffin_sel$`DO%`, na.rm = TRUE)
sd_do   <- sd(Baffin_sel$`DO%`, na.rm = TRUE)

mean_pH <- mean(Baffin_sel$pH, na.rm = TRUE)
sd_pH   <- sd(Baffin_sel$pH, na.rm = TRUE)

mean_log_TOC <- mean(log(Baffin_sel$TOC + 1), na.rm = TRUE)
sd_log_TOC   <- sd(log(Baffin_sel$TOC + 1), na.rm = TRUE)

mean_log_TN <- mean(log(Baffin_sel$TN + 1), na.rm = TRUE)
sd_log_TN   <- sd(log(Baffin_sel$TN + 1), na.rm = TRUE)

mean_sal <- mean(Baffin_sel$Salinity, na.rm = TRUE)
sd_sal   <- sd(Baffin_sel$Salinity, na.rm = TRUE)

# Coefficients from model2 (standardized scale)
alpha_std <- 0.00
b_temp_std <- 0.55
b_do_std   <- 0.19
b_pH_std   <- -0.63
b_TOC_std  <- 0.62
b_TN_std   <- -0.20

# Reversing the coefficients 
b_temp <- b_temp_std * sd_sal / sd_temp
b_do   <- b_do_std   * sd_sal / sd_do
b_pH   <- b_pH_std   * sd_sal / sd_pH
b_TOC  <- b_TOC_std  * sd_sal / sd_log_TOC
b_TN   <- b_TN_std   * sd_sal / sd_log_TN

# Adjusted intercept
intercept <- mean_sal - (b_temp * mean_temp + b_do   * mean_do + b_pH   * mean_pH +
                         b_TOC  * mean_log_TOC + b_TN   * mean_log_TN)

# Printing Final Regression Equation.
cat(sprintf(
  "\n✅ Final Regression Equation (PSU):\nSalinity = %.3f + %.3f * Temp + %.3f * DO%% + %.3f * pH + %.3f * log(TOC + 1) + %.3f * log(TN + 1)\n\n",
  intercept, b_temp, b_do, b_pH, b_TOC, b_TN))

# Predicting Salinity using Realistic Input Values
temp <- 24.25
do <- 88
pH <- 8.2
toc <- 900
tn <- 93 

salinity_pred <- intercept + b_temp * temp + b_do   * do + b_pH   * pH +
                             b_TOC  * log(toc + 1) + b_TN   * log(tn + 1)
cat(sprintf("📌 Predicted Salinity (PSU) for input values = %.2f\n", salinity_pred))





