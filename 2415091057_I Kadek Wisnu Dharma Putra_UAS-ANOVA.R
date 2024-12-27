library(readxl)
library(lmtest)
library(ggplot2)


file_path <- "C:/Users/Wisnu Dharma/Documents/Kuliah Undiksha/Semester 1/Statiska dan Probabilitas/data_anova.xlsx"
data <- read_excel(file_path)


value <- data$value
group <- data$group

# Assumption Test : Normalitas (Shapiro-Wilk)
shapiro_test <- shapiro.test(value)
print(shapiro_test)

# Assumption Test : Homogenitas (Bartlett)
bartlett_test <- bartlett.test(value ~ group, data = data)
print(bartlett_test)

# Assumption Test : Independensi (Durbin-Watson)
model_lm <- lm(value ~ group, data = data)
durbin_watson <- dwtest(model_lm)
print(durbin_watson)


# ANOVA Analysis Test
anova_model <- aov(value ~ group, data = data)
anova_result <- summary(anova_model)
print(anova_result)


# Data Visualization
# Boxplot
ggplot(data, aes(x = group, y = value)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Boxplot Response Berdasarkan Group", x = "Group", y = "Value") +
  theme_minimal()

# Histogram
ggplot(data, aes(x = value)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +
  labs(title = "Histogram Value", x = "Value", y = "Frequency") +
  theme_minimal()





