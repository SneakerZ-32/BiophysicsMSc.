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

# View the combined data (optional)
head(combined_data)




# Convert wavelength and intensity to numeric (if they are factors)
combined_data$Wavelength <- as.numeric(gsub(",", ".", combined_data$Wavelength))
combined_data$Intensity <- as.numeric(gsub(",", ".", combined_data$Intensity))

# Define color palette based on Timepoint
num_timepoints <- length(unique(combined_data$Timepoint))
color_palette <- colorRampPalette(c("blue", "red"))(num_timepoints)

# Plot using ggplot2
ggplot(combined_data, aes(x = Wavelength, y = Intensity, color = factor(Timepoint))) +
  xlim(400,800)+
  geom_line() +
  scale_color_manual(values = color_palette) +
  labs(x = "Wavelength", y = "Intensity", color = "Timepoint") +
  theme_minimal()

