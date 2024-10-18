# Load necessary libraries
library(ggplot2)
library(reshape2)

# Read the data from the text file
data <- read.table("test.txt", sep = ",", header = FALSE)

# Rename the columns for clarity
colnames(data) <- c("wavelength1", "absorbance1", "wavelength2", "absorbance2")

# Combine the data into a long format for ggplot2
data_long <- melt(data, id.vars = c("wavelength1", "wavelength2"), 
                  measure.vars = c("absorbance1", "absorbance2"),
                  variable.name = "Type", value.name = "Absorbance")

# Plot absorbance vs. wavelength with different colors for absorbance1 and absorbance2
ggplot(data_long, aes(x = wavelength1, y = Absorbance, color = Type)) +
  geom_line() +
  scale_color_manual(values = c("absorbance1" = "red", "absorbance2" = "blue")) +
  labs(title = "Absorbance vs Wavelength",
       x = "Wavelength",
       y = "Absorbance",
       color = "Absorbance Type") +
  theme_minimal()




# Calculate the ratio of absorbance2 to absorbance1
data$ratio <- data$absorbance2 / data$absorbance1

# Normalize the ratio
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data$ratio_normalized <- normalize(data$ratio)

# Plot absorbance values with different colors
data_long <- melt(data, id.vars = "wavelength1", 
                  measure.vars = c("absorbance1", "absorbance2"),
                  variable.name = "Type", value.name = "Absorbance")

ggplot(data_long, aes(x = wavelength1, y = Absorbance, color = Type)) +
  geom_line() +
  scale_color_manual(values = c("absorbance1" = "red", "absorbance2" = "blue")) +
  labs(title = "Absorbance vs Wavelength",
       x = "Wavelength",
       y = "Absorbance",
       color = "Absorbance Type") +
  theme_minimal()

# Plot the normalized ratio of absorbance2 to absorbance1 vs. wavelength1
ggplot(data, aes(x = wavelength1, y = ratio_normalized)) +
  geom_line(color = "green") +
  labs(title = "Normalized Absorbance of EosFP",
       x = "Wavelength",
       y = "Normalized Sample Absorbance / Background") +
  theme_minimal()

