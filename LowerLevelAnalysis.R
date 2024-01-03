
#Question 1#
# R Code for Side-by-Side Violin Plot
# 'data' should have columns 'AssessedValue' and 'SalesPrice'

data<-read.csv(file.choose(),header=TRUE)

library(ggplot2)

# Create a new column 'ValueType' to distinguish between Assessed Value and Sales Price
data_long <- reshape2::melt(data, measure.vars = c('AssessedValue', 'SalesPrice'))

# Create the violin plot
ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
  geom_violin(trim = FALSE) +
  labs(title = "Comparison of Assessed Home Values and Sales Prices",
       x = "Value Type",
       y = "Price",
       fill = "Type") +
  scale_fill_manual(values = c("AssessedValue" = "blue", "SalesPrice" = "red")) +
  theme_minimal()

#Question 2#
# R Code for Bar Graph of Racial Composition
# Assuming 'data' is your dataset with columns 'Region', 'White', 'NonWhite'

ggplot(data, aes(x = Region, y = White, fill = 'White')) +
  geom_bar(stat = 'identity') +
  geom_bar(aes(y = NonWhite, fill = 'NonWhite'), stat = 'identity') +
  labs(title = "Racial Composition of Each Region",
       x = "Region",
       y = "Percentage")

#Question 3#

# Calculating median sale prices for each region
median_prices <- aggregate(SalesPrice ~ Region, data = data, median)

# R Code for Bar Chart of Median Sale Prices
ggplot(median_prices, aes(x = Region, y = SalesPrice, fill = Region)) +
  geom_bar(stat = 'identity') +
  labs(title = "Median Sale Prices by Region",
       x = "Region",
       y = "Median Sale Price") +
  theme_minimal()

#Question 4

