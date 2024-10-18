library(ggplot2)
library(minpack.lm)

# Set your folder path
folder_path <- "C:/Users/Dahoam/Desktop/G8/EosFP Photoconversion 60 frames"

# List all files in the folder
file_names <- list.files(folder_path)

# Initialize an empty list to store data frames
data_list <- list()

# Loop through each file
for (file in file_names) {
  
  # Read the file
  file_path <- file.path(folder_path, file)
  file_content <- readLines(file_path)
  
  # Extract timepoint from file name
  timepoint <- as.numeric(gsub(".*__(\\d+)__(\\d+).*", "\\1", file))
  
  # Find the line where spectral data starts
  data_start <- grep(">>>>>Begin Spectral Data<<<<<", file_content) + 1
  
  # Read spectral data (assuming 2 columns: wavelength and intensity)
  spectral_data <- read.table(text = file_content[data_start:length(file_content)], 
                              sep = "\t", header = FALSE, col.names = c("Wavelength", "Intensity"))
  
  # Add timepoint column
  spectral_data$Timepoint <- timepoint
  
  # Append to the list
  data_list[[file]] <- spectral_data
}

# Combine all data frames into a single data frame
combined_data <- do.call(rbind, data_list)

# Convert wavelength and intensity to numeric (if they are factors)
combined_data$Wavelength <- as.numeric(gsub(",", ".", combined_data$Wavelength))
combined_data$Intensity <- as.numeric(gsub(",", ".", combined_data$Intensity))

# Filter for wavelengths of interest
wavelengths_of_interest <- c(515.899, 580.997)
filtered_data <- combined_data[combined_data$Wavelength %in% wavelengths_of_interest & combined_data$Timepoint >= 10, ]

# Fit exponential decay for 515.899 nm
fit_516 <- try(nlsLM(Intensity ~ a * exp(b * Timepoint), 
                     data = filtered_data[filtered_data$Wavelength == 515.899, ],
                     start = list(a = max(filtered_data$Intensity[filtered_data$Wavelength == 515.899]), 
                                  b = -0.01)), silent = TRUE)

# Fit exponential increase for 580.997 nm
fit_581 <- try(nlsLM(Intensity ~ a * exp(b * Timepoint), 
                     data = filtered_data[filtered_data$Wavelength == 580.997, ],
                     start = list(a = min(filtered_data$Intensity[filtered_data$Wavelength == 580.997]), 
                                  b = 0.01)), silent = TRUE)

# Summarize the fits
if (!inherits(fit_516, "try-error")) {
  print(summary(fit_516))
} else {
  cat("Failed to fit exponential model for wavelength 515.899\n")
}

if (!inherits(fit_581, "try-error")) {
  print(summary(fit_581))
} else {
  cat("Failed to fit exponential model for wavelength 580.997\n")
}

# Create a data frame for predictions
predictions <- data.frame(Timepoint = seq(min(filtered_data$Timepoint), max(filtered_data$Timepoint), length.out = 100))
if (!inherits(fit_516, "try-error")) {
  predictions$Intensity_516 <- predict(fit_516, newdata = predictions)
}
if (!inherits(fit_581, "try-error")) {
  predictions$Intensity_581 <- predict(fit_581, newdata = predictions)
}

# Define colors for each wavelength
colors <- c("515.899" = "green", "580.997" = "red")

# Plot the data and the fitted models
ggplot(filtered_data, aes(x = Timepoint, y = Intensity, color = factor(Wavelength))) +
  geom_point() +
  geom_line(data = predictions, aes(y = Intensity_516, color = "515.899"), linetype = "dashed") +
  geom_line(data = predictions, aes(y = Intensity_581, color = "580.997"), linetype = "dashed") +
  scale_color_manual(values = colors) +
  labs(x = "Timepoint", y = "Intensity", color = "Wavelength") +
  theme_minimal()
