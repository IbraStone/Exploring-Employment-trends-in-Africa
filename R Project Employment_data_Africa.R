# File:     Employment_data-Africa1
# Project:  Exploring Employment Trends in Africa Regions

# LOAD PACKAGES ############################################

# RStudio will prompt you to download any packages that
# aren't already installed.

# Load packages
library(ggplot2)# Loads the `ggplot2` collection
library(dplyr)  # Loads the `dplyr` collection
library (readxl) # Reads CSV and Excel files

# LOAD DATA ################################################

# Load and view the data
df <- read_csv("C:/Users/ibrah/OneDrive/Desktop/Data Analytics Certificate Program/Employment_Data_Africa1.csv")
df
# ANALYZE DATA #############################################
# Calculate summary statistics
summary_df <- summary(df$`Employment rate(%)`)
cat("Summary Statistics for Employment Rate:\n", summary_df, "\n\n")


# Time series analysis
ggplot(df, aes(x = Year, y = `Employment rate(%)`, group = Region, color = Region)) +
  geom_line() +
  labs(title = "Employment Rate Over Time by Region")

# Gender-based analysis
ggplot(df, aes(x = Year, y = `Employment rate(%)`, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Employment Rate by Year and Gender")

# Categorical analysis
ggplot(df, aes(x = Category, y = `Employment rate(%)`, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Employment Rate by Category and Gender")

# Regional comparison
ggplot(df, aes(x = Region, y = `Employment rate(%)`, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Employment Rate by Region and Gender")

# Correlation Analysis
correlation <- cor(df$Year, df$`Employment rate(%)`)
cat("Correlation between Year and Employment Rate: ", round(correlation, 2), "\n")

# Scatter Plot with Correlation
scatter_plot <- ggplot(df, aes(x = Year, y = `Employment rate(%)`)) +
  geom_point() +
  labs(title = "Scatter Plot of Year vs. Employment Rate",
       x = "Year",
       y = "Employment Rate")

# Adding correlation to the plot
scatter_plot_with_correlation <- scatter_plot +
  geom_text(aes(label = paste("Correlation =", round(correlation, 2))),
            x = min(df$Year),
            y = max(df$`Employment rate(%)`),
            hjust = 0, vjust = 1,
            parse = TRUE)

# View the scatter plot with correlation
print(scatter_plot_with_correlation)

# Create a pivot table (wide format) for the heatmap
heatmap_data <- df %>%
  pivot_wider(names_from = Year, values_from = `Employment rate(%)`)

# Create the heatmap
ggplot(heatmap_data, aes(x = Category, y = Region, fill = `2005`)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Employment Rates Heatmap (Year: 2015)",
       x = "Category",
       y = "Region",
       fill = "Category") +
  theme_minimal()


# To arrange multiple heatmaps side by side, you can use facet_wrap or facet_grid.
# For example, to create a 2x2 grid of heatmaps for 2005, 2010, 2015, and 2021:
heatmap_data_long <- heatmap_data %>%
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Employment Rate")

ggplot(heatmap_data_long, aes(x = Category, y = Region, fill = `Employment Rate`)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Employment Rates Heatmaps by Year",
       x = "Category",
       y = "Region",
       fill = "Employment Rate") +
  facet_grid(. ~ Year) +
  theme_minimal()

#  Economic disparities
# Create a box plot to visualize economic disparities
ggplot(df, aes(x = Region, y = `Employment rate(%)`)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Economic Disparities by Region",
       x = "Region",
       y = "Employment Rate (%)") +
  theme_minimal()

# Perform one-way ANOVA to test differences among regions
anova_result <- aov(`Employment rate(%)` ~ Region, data = df)

# Check the summary of the ANOVA
summary(anova_result)

# SUMMARY ##################################################

# Summary for entire dataset
df |> summary()


# CLEAN UP #################################################

# Clear R 
#   Restart R to clear objects from the environment, clear
#   plots, unload external packages, reset options, relative
#   paths, dependencies, etc. Use the RStudio menu Session > 
#   Restart R, or use Ctrl+Shift+F10 (for Windows and Linux)
#   or Command+Shift+0 (for MacOS).

# Clear console
cat("\014")  # Mimics ctrl+L

# Clear mind :)
