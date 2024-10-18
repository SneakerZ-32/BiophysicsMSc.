library(ggplot2)

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
  timepoint <- as.numeric(gsub("[^0-9]", "", sub("Subt4__", "", file)))
  
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

# Define color palette based on Timepoint
num_timepoints <- length(unique(combined_data$Timepoint))
color_palette <- colorRampPalette(c("blue", "red"))(num_timepoints)

# Plot using ggplot2
plot1 <- ggplot(combined_data, aes(x = Wavelength, y = Intensity, color = factor(Timepoint))) +
  geom_line() +
  scale_color_manual(values = color_palette) +
  labs(x = "Wavelength", y = "Intensity", color = "Timepoint") +
  theme_minimal()

# Filter for wavelengths of interest
wavelengths_of_interest <- c(515.899, 580.997)
filtered_data <- combined_data[combined_data$Wavelength %in% wavelengths_of_interest & combined_data$Timepoint >= 1010, ]

# Define colors for each wavelength
colors <- c("515.899" = "green", "580.997" = "red")














# Calculate Pearson's correlation coefficient
pearson_corr <- cor(filtered_data$Wavelength, filtered_data$Intensity, method = "pearson")

# Plot using ggplot2 starting from Timepoint 1010
plot2 <- ggplot(filtered_data, aes(x = Timepoint, y = Intensity, color = factor(Wavelength))) +
  geom_line() +
  scale_color_manual(values = colors) +
  labs(x = "Timepoint", y = "Intensity", color = "Wavelength") +
  theme_minimal()

# Combine plots and print
library(gridExtra)
grid.arrange(plot1, plot2, ncol = 2, top = paste("Pearson's correlation:", round(pearson_corr, 2)))
