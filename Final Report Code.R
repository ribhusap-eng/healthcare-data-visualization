# Load required libraries
library(circlize)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# Read dataset
healthcare <- read.csv("healthcare_dataset.csv")

# Calculate the frequency of each combination of medication and medical condition for Chord Diagram
Medication_freq <- healthcare %>%
  group_by(Medication, Medical.Condition) %>%
  summarise(Frequency = n()) %>%
  ungroup()

# Creates the chord diagram for medications by medical conditions
chordDiagram(Medication_freq, transparency = 0.5)

# Convert Date of Admission to Date format for Mosaic Plot and Calendar Heatmap
healthcare$Date.of.Admission <- as.Date(healthcare$Date.of.Admission)

# Create Mosaic Table for Medical Condition by Year of Admission
mosaic_table <- table(year(healthcare$Date.of.Admission), healthcare$Medical.Condition)

# Define Color Palette for Mosaic Plot
color_palette <- c(
  "Obesity" = "#00B050",
  "Hypertension" = "#92D050",
  "Diabetes" = "#FFFF00",
  "Cancer" = "#FF9900",
  "Asthma" = "#FF6600",
  "Arthritis" = "#FF0000"
)

# Plot Mosaic Plot
mosaicplot(mosaic_table, 
           main = "Medical Condition by Year of Admission",
           color = color_palette, 
           legend = TRUE,
           xlab = "Year of Admission",
           ylab = "Medical Condition")

# Generate Calendar Heatmap for Admission Counts
admission_counts <- table(healthcare$Date.of.Admission)
admission_counts_df <- data.frame(Date = as.Date(names(admission_counts)), Count = as.numeric(admission_counts))
source("calendarHeat.R")
calendarHeat(admission_counts_df$Date, admission_counts_df$Count, varname = "Number of Admissions")

# Create 2D Density Plot for Age vs. Billing Amount by Medical Condition
healthcare$Age <- as.numeric(as.character(healthcare$Age))
healthcare$Billing.Amount <- as.numeric(as.character(healthcare$Billing.Amount))
p <- ggplot(healthcare, aes(x = Age, y = Billing.Amount)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  scale_fill_viridis_c() + 
  facet_wrap(~ Medical.Condition, scales = "free") +
  theme_minimal() +
  labs(title = "Age vs Billing Amount by Medical Condition",
       x = "Age", y = "Billing Amount")
print(p)

# Create Heatmap of Average Billing Amount by Insurance Provider and Medical Condition
heatmap_data <- healthcare %>%
  group_by(Insurance.Provider, Medical.Condition) %>%
  summarise(AverageBillingAmount = mean(Billing.Amount, na.rm = TRUE)) %>%
  ungroup()

heatmap_data_wide <- heatmap_data %>%
  pivot_wider(names_from = Medical.Condition, values_from = AverageBillingAmount)

median_value <- median(heatmap_data$AverageBillingAmount, na.rm = TRUE)

ggplot(heatmap_data, aes(x= Medical.Condition, y= Insurance.Provider, fill=AverageBillingAmount)) +
  geom_tile() +
  geom_text(aes(label=round(AverageBillingAmount, 2)), color="black", size=3) +
  scale_fill_gradient2(low = "papayawhip", high = "peachpuff4", mid = "peachpuff2", midpoint = median_value, space = "Lab",
                       name="Average Billing\nAmount") +
  labs(title = "Heatmap of Average Billing Amount by Insurance Provider and Medical Condition",
       x = "Medical Condition", y = "Insurance Provider") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

