## Libraries
library(rethinking)
library(corrplot)
library(ggplot2)
library(tidyr)
library(dplyr)
library(GGally)
library(readxl)
library(car)


########################################################
# Phase-1: Understanding data
########################################################
# Step-1. Loading the dataset
# Step-2. Visualizations
# Step-3. 

########################################################


# Step-1: Load the dataset
################################################
Baffin <- read_excel("~/SIDDHARTHA/MASTER'S TAMUCC/COURSES/SPRING 2025/DASC 5304 Bayesian/Baffin_bay.xlsx")
#View(Baffin)
precis(Baffin)
#summary(Baffin)
Baffin_num <- Baffin[,-c(1,2)]
#View(Baffin_num)


# Step-2: Visualizations
# Boxplots
# Histogram
# Correlation plot
# Scatterplot
################################################
# Boxplots
long_data <- pivot_longer(Baffin_num, cols = where(is.numeric), names_to = "Variable", values_to = "Value")

ggplot(long_data, aes(x = "", y = Value)) +
  geom_boxplot(fill = "#1ff64d", color = "#1f4af6") +
  facet_wrap(~ Variable, scales = "free", ncol = 4) +  # adjust ncol as needed
  theme_minimal() +
  labs(title = "Separate Boxplots by Variable", x = "", y = "Value")
#####################################

# Histogram: Distiribution of salinity
ggplot(Baffin, aes(x = Salinity)) +
  geom_histogram(binwidth = 2, fill = "#1283d7", color = "#0c0c0c") +
  theme_minimal() +
  labs(title = "Distribution of Salinity in Baffin Bay", x = "Salinity", y = "Count")
#####################################

# Histograms: all other variables
baffin_long <- pivot_longer(Baffin_num, 
                            cols = everything(), 
                            names_to = "Variable", 
                            values_to = "Value")

ggplot(baffin_long, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "#1855c7", color = "white") +
  facet_wrap(~ Variable, scales = "free", ncol = 4) +
  theme_minimal() +
  labs(title = "Histograms of All Numeric Variables in Baffin Bay",
       x = "Value", y = "Frequency")
#####################################

# Correlation plot
Cor_Baffin <- cor(Baffin_num)
Cor_Baffin
corrplot(Cor_Baffin, method = "number", type = "lower", bg = "#060606",add = FALSE, tl.pos = "lt", number.cex = 0.7)
corrplot(Cor_Baffin, method = "circle", type = "upper", bg = "#060606",add = TRUE,  tl.col = "blank")         
#####################################

# Scatterplot
ggpairs(Baffin_num,
        title = "Scatterplot Matrix of Baffin Bay Variables",
        upper = list(continuous = wrap("cor", size = 3)),
        lower = list(continuous = wrap("points", alpha = 0.6)),
        diag = list(continuous = wrap("densityDiag")))
#####################################








