#Econ Visualizations
install.packages("extrafont")
library(extrafont)
library(tidyverse)
library(dplyr)
library(ggplot2)
Data_Pov <- read.csv("DRC Specific.csv")
# Example dataset
data <- data.frame(
  Category = c("Bellow $2.25", "Bellow $1", "Bellow $3.65", "Bellow $6.85"),
  Frequency = c(78.94, 66.90, 97.60, 97.70)
)

# Creating the pie chart
pie(data$Frequency, labels = data$Category, main = "DRC's Poverty Thresholds 2023 in %")


# Importing custom font (change 'Arial' to your preferred font)
font_import(pattern = "Arial")

# Settting the custom font
loadfonts()

# Test
data <- data.frame(
  Category = c("Bellow $2.25", "Bellow $1", "Bellow $3.65", "Bellow $6.85"),
  Frequency = c(78.94, 66.90, 97.60, 97.70)
)

Data_stut <- read.csv("DRC Stunting.csv")

# Creating a pie chart with customizations
pie(data$Frequency, labels = data$Category, main = "DRC's Poverty Thresholds 2023 in %", col = rainbow(length(data$Frequency)), cex.main = 1.2, cex = 1.2, font = 1, family = "Arial")

# Creating the plot 1
Data_stut <- Data_stut[-1, ]


ggplot(Data_stut, aes(x = X, y = X.1)) +
  geom_line() +
  geom_point() +
  labs(title = "Stunting Prevalence Comparison",
       x = "Country",
       y = "Value",
       color = "blue") +
  theme_minimal()
# 2
ggplot(Data_stut, aes(x = X.1)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "blue", alpha = 0.8) +
  labs(title = "Histogram of Your Variable",
       x = "X",
       y = "X.1") +
  theme_minimal()

ggplot(Data_stut, aes(x = X.1, y = X, group = X, color = X)) +
  geom_line() +
  labs(title = "Line Chart of Country Values Over Time",
       x = "Value",
       y = "Country") +
  theme_minimal()

ggplot(Data_stut, aes(x = X.1, y = X, color = X)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Line Chart of Country Values Over Time",
       x = "Value",
       y = "Country") +
  theme_minimal()


ggplot(Data_stut, aes(x = X, y = X.1)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Inserting a horizontal line at y = 0
  labs(title = "Stunting Prevalence Comparison",
       x = "Country",
       y = "Value",
       color = "blue") +
  theme_minimal()

Data_GDP <- read.csv("GDP Data.csv")

ggplot(Data_GDP, aes(x = Year, y = Country, fill = GDP_billion_USD)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "GDP  Over Time",
       x = "Year",
       y = "Country",
       fill = "GDP_billion_USD") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#NORMAL GDP
GDP <- ggplot(Data_GDP, aes(x = Year, y = GDP_billion_USD, color = Country)) +
  geom_line() +
  labs(title = "GDP Trends Over Time",
       x = "Year",
       y = "GDP (billion US$)",
       color = "Country") +
  theme_minimal()+
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold"))
ggsave("GDP_Trends.pdf", GDP, width = 10, height = 6)

#GDPpc 

GDPpc <- Data_GDP %>% mutate(GDPpc_USD2 = as.numeric(gsub(",", "", GDPpc_USD))) %>%
  ggplot(aes(x = Year, y = GDPpc_USD2, color = Country)) +
  geom_line() +
  labs(title = "GDP per Capita Trends Over Time",
       x = "Year",
       y = "GDP per Capita (USD)",
       color = "Country") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold"))

# Saving the plot as a PDF file
ggsave("GDPpc_Trends.pdf", GDPpc, width = 10, height = 6)


GDPpc_bar <- ggplot(Data_GDP, aes(x = Year, y = GDPpc_USD, fill = Country)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "GDP per Capita Trends Over Time",
       x = "Year",
       y = "GDP per Capita (USD)",
       fill = "Country") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold"))

# Saving the plot as a PDF file
ggsave("GDPpc_Trends_BarChart.pdf", GDPpc_bar, width = 10, height = 6)


GDPpc <- ggplot(Data_GDP, aes(x = Year, y = GDPpc_USD, color = Country)) +
  geom_line() +
  labs(title = "GDP per Capita Trends Over Time",
       x = "Year",
       y = "GDP per Capita (USD)",
       color = "Country") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold")) +
  scale_x_continuous(breaks = seq(min(Data_GDP$Year), max(Data_GDP$Year), by = 1))

# Saving the plot as a PDF file
ggsave("GDPpc_Trends.pdf", GDPpc, width = 10, height = 6)

GDPpc_smooth <- ggplot(Data_GDP, aes(x = Year, y = GDPpc_USD, color = Country)) +
  geom_smooth(method = "auto", se = FALSE) +
  labs(title = "GDP per Capita Trends Over Time",
       x = "Year",
       y = "GDP per Capita (USD)",
       color = "Country") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold"))

# Saving the plot as a PDF file
ggsave("GDPpc_Trends_LinePlot.pdf", GDPpc_smooth, width = 10, height = 6)


GDPpc_faceted <- ggplot(Data_GDP, aes(x = Year, y = GDPpc_USD, color = Country)) +
  geom_line() +
  labs(title = "GDP per Capita Trends Over Time",
       x = "Year",
       y = "GDP per Capita (USD)") +
  theme_minimal() +
  facet_wrap(~Country, scales = "free_y") +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12))

# Saving the plot as a PDF file
ggsave("GDPpc_Trends_FacetedPlot.pdf", GDPpc_faceted, width = 10, height = 6)

# Printing the plot
print(GDPpc_faceted)



agg_data <- Data_GDP %>%
  group_by(Year, Country) %>%
  summarize(mean_GDPpc_USD = mean(GDPpc_USD))

# Plotting the aggregated data using a group line plot
GDPpc_grouped <- ggplot(agg_data, aes(x = Year, y = mean_GDPpc_USD, color = Country)) +
  geom_line() +
  labs(title = "Mean GDP per Capita Trends Over Time",
       x = "Year",
       y = "Mean GDP per Capita (USD)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12))

# Saving the plot as a PDF file
ggsave("Mean_GDPpc_Trends.pdf", GDPpc_grouped, width = 10, height = 6)

# Printing the plot
print(GDPpc_grouped)

library(dplyr)

# Aggregating the data to calculate the mean GDP per capita for each year and country
agg_data <- Data_GDP %>%
  group_by(Year, Country) %>%
  summarize(mean_GDPpc_USD = mean(GDPpc_USD))

# Plotting the aggregated data using a group line plot
GDPpc_grouped <- ggplot(agg_data, aes(x = Year, y = mean_GDPpc_USD, color = Country, group = Country)) +
  geom_line() +
  labs(title = "Mean GDP per Capita Trends Over Time",
       x = "Year",
       y = "Mean GDP per Capita (USD)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12))

# Saving the plot as a PDF file
ggsave("Mean_GDPpc_Trends.pdf", GDPpc_grouped, width = 10, height = 6)

# Printing the plot
print(GDPpc_grouped)

Data_PC <- read.csv("GDPpc.csv")

ggplot(Data_PC, aes(x = Year, y = GDPpc_USD, color = Country)) +
  geom_line() +
  labs(title = "GDP per Capita Trends Over Time",
       x = "Year",
       y = "GDP per Capita (USD)",
       color = "Country") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold"))

#HDI
HDI <- ggplot(Data_GDP, aes(x = Year, y = HDI, color = Country)) +
  geom_line() +
  labs(title = "HDI Trends Over Time",
       x = "Year",
       y = "Human Development Index",
       color = "Country") +
  theme_minimal()+
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold"))
ggsave("HDI.pdf", HDI, width = 10, height = 6)


library(ggplot2)

# Final Plot
ggplot(Data_GDP, aes(x = Year, y = GDP_billion_USD, color = Country)) +
  geom_line() +
  labs(title = "GDP Trends Over Time",
       x = "Year",
       y = "GDP (billion US$)",
       color = "Country") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "gray", linetype = "dashed"))
