
# Load necessary libraries
library(readr)

# Read data
data <- read_csv("path_to_your_file/ElkUrtm23.csv")

# Calculate sums of production types excluding date, time, and total columns
production_types <- colSums(data[, 4:ncol(data)])

# Create a pie chart
pie(production_types, labels = names(production_types), main = "Electricity Production by Type", col = rainbow(length(production_types)))

# You might need to adjust the data indexing based on your CSV structure
