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

Baffin_trans <- Baffin_sel %>% mutate(TOC = log(TOC + 1), TN = log(TN + 1), silicate = log(silicate + 1)) %>% 
                               mutate(Salinity = standardize(Salinity),
                                      Temp = standardize(Temp),
                                      `DO%` = standardize(`DO%`),
                                      pH = standardize(pH),
                                      TOC = standardize(TOC),
                                      TN = standardize(TN),
                                      silicate = standardize(silicate)) %>% 
                               select(Salinity, Temp, `DO%`, pH, TOC, TN, silicate)
head(Baffin_trans)


# Step-5: Final Visualizations to check for any inconsistencies.
# Boxplots
# Histograms
# Correlation Plot
# Scatter plots
################################################################
dir.create("plots")

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
# Step-1: Model-1: ulam() model with all selected variables.
# Step-2: Model-2: ulam() model with 'silicate' variable removed.
# Step-3: Model-3: ulam() model with 'silicate' and 'DO_prec' removed.
# Step-4: Comparision of all 3 models using WAIC and PSIS
###############################################################################


# Step-1: Model-1: ulam() model with all selected variables.
###################################################################
# Renaming the variable DO% to DO_perc to make it make it ulam() compatible
Baffin_trans1 <- Baffin_trans %>% rename(DO_perc = `DO%`)

model1 <- ulam(alist(Salinity ~ normal(mu, sigma),
                     mu <- alpha + b_temp*Temp + b_do*DO_perc + b_pH*pH + 
                                   b_TOC*TOC + b_TN*TN + b_silicate*silicate,
                           alpha ~ normal(0, 1),
                           b_temp ~ normal(0, 0.5),
                           b_do ~ normal(0, 0.5),
                           b_pH ~ normal(0, 0.5),
                           b_TOC ~ normal(0, 0.5),
                           b_TN ~ normal(0, 0.5),
                           b_silicate ~ normal(0, 0.5),
                     sigma ~ exponential(1)),
               data = Baffin_trans1,
               chains = 4, 
               cores = 4,
               log_lik = TRUE)
precis(model1)


# Step-2: Model-2: ulam() model with 'silicate' variable removed.
###################################################################
model2 <- ulam(alist(Salinity ~ normal(mu, sigma),
                     mu <- alpha + b_temp*Temp + b_do*DO_perc + b_pH*pH + 
                                   b_TOC*TOC + b_TN*TN,
                           alpha ~ normal(0, 1),
                           b_temp ~ normal(0, 0.5),
                           b_do ~ normal(0, 0.5),
                           b_pH ~ normal(0, 0.5),
                           b_TOC ~ normal(0, 0.5),
                           b_TN ~ normal(0, 0.5),
                     sigma ~ exponential(1)),
               data = Baffin_trans1,
               chains = 4, 
               cores = 4,
               log_lik = TRUE)
precis(model2)


# Step-3: Model-3: ulam() model with 'silicate' and 'DO_prec' removed.
###################################################################
model3 <- ulam(alist(Salinity ~ normal(mu, sigma),
                     mu <- alpha + b_temp*Temp + b_pH*pH + b_TOC*TOC + b_TN*TN,
                           alpha ~ normal(0, 1),
                           b_temp ~ normal(0, 0.5),
                           b_pH ~ normal(0, 0.5),
                           b_TOC ~ normal(0, 0.5),
                           b_TN ~ normal(0, 0.5),
                     sigma ~ exponential(1)),
               data = Baffin_trans1,
               chains = 4, 
               cores = 4,
               log_lik = TRUE)
precis(model3)


# Step-4: Comparision of all 3 models using WAIC and PSIS.
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
# Step-1: Prior Predictive check
# Step-2: Model-2: ulam() model with 'silicate' variable removed.
# Step-3: Model-3: ulam() model with 'silicate' and 'DO_prec' removed.
# Step-4: Comparision of all 3 models using WAIC and PSIS
###############################################################################


# Step-1: Prior Predictive check
###################################################################
# prior choice 1: alpha = normal(0,1); b_coeffi = normal(0,0.5); sigma = expo(1)
# prior choice 2: alpha = normal(0,1); b_coeffi = normal(0,1); sigma = expo(1)
# prior choice 3: alpha = normal(0,1.5); b_coeffi = normal(0,1); sigma = expo(1)
# prior choice 4: alpha = normal(0,2); b_coeffi = normal(0,1); sigma = expo(1)
# prior choice 5: alpha = normal(0,1.5); b_coeffi = normal(0,1); sigma = expo(1)
# prior choice 6: alpha = normal(0,1.5); b_coeffi = normal(0,0.5); sigma = expo(1)

par(mfrow = c(2,2))
model2_prior <- ulam(alist(Salinity ~ normal(mu, sigma),
                            mu <- alpha + b_temp*Temp + b_do*DO_perc + b_pH*pH +
                                          b_TOC*TOC + b_TN*TN + b_silicate*silicate,
                                  alpha ~ normal(0, 1.5),
                                  b_temp ~ normal(0, 0.5),
                                  b_do ~ normal(0, 0.5),
                                  b_pH ~ normal(0, 0.5),
                                  b_TOC ~ normal(0, 0.5),
                                  b_TN ~ normal(0, 0.5),
                                  b_silicate ~ normal(0, 0.5),
                            sigma ~ exponential(1)),
                     data = Baffin_trans1,
                     chains = 4,
                     cores = 4,
                     sample_prior = TRUE)

# Simulate from the prior only (not conditioned on data)
prior_sim <- extract.samples(model2_prior)

# Generate the prior predictive distribution for mu
mu_prior <- prior_sim$alpha +
  prior_sim$b_temp * Baffin_trans1$Temp +
  prior_sim$b_do * Baffin_trans1$DO_perc +
  prior_sim$b_pH * Baffin_trans1$pH +
  prior_sim$b_TOC * Baffin_trans1$TOC +
  prior_sim$b_TN * Baffin_trans1$TN +
  prior_sim$b_silicate * Baffin_trans1$silicate

# Simulate salinity values (prior predictive)
N <- nrow(Baffin_trans1)
y_prior_pred <- matrix(NA, nrow = length(prior_sim$sigma), ncol = N)
for (i in 1:length(prior_sim$sigma)) {
  y_prior_pred[i, ] <- rnorm(N, mu_prior[i, ], prior_sim$sigma[i])
}


# Plot prior predictive salinity values
prior_df <- data.frame(SimulatedSalinity = as.vector(y_prior_pred))

print(ggplot(prior_df, aes(x = SimulatedSalinity)) +
  geom_histogram(bins = 40, fill = "#0073C2FF", alpha = 0.6) +
  geom_vline(xintercept = mean(Baffin_trans1$Salinity), color = "red", linetype = "dashed") +
  labs(title = "Prior Predictive Distribution of Salinity",
       subtitle = "Blue: Simulated from prior | Red: Mean of real (standardized) salinity",
       x = "Simulated Salinity", y = "Frequency") +
  theme_minimal())


