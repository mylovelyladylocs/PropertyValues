housing<-read.csv(file.choose(),header=TRUE)
housing_data<-housing

# Load necessary libraries
install.packages("ellipsis")
install.packages("Rcpp")
install.packages("reshape2")
install.packages("tidyr")


library(ggplot2)
library(tidyr)
library(Rcpp)
library(reshape2)

#Convert Data to long

long_data <- melt(housing_data, id.vars = "Region", 
                  measure.vars = c("X2014", "X2016", "X2018", "X2020", "X2022"),
                  variable.name = "Year", value.name = "Sales.Price")
long_data$Year <- as.numeric(sub("X", "", long_data$Year))

#Check assumptions
library(dplyr)

# Assuming your reshaped data is in 'long_data'
results <- long_data %>%
  group_by(Region, Year) %>%
  summarise(shapiro_p_value = shapiro.test(Sales.Price)$p.value)

print(results)
#Cannot assume Normality. 

#Kruskal-wallis not appropriate for year by region. Instead, let's try to separate
#by year

data_2014 <- subset(long_data, Year == 2014)
shapiro_test_results <- by(data_2014$Sales.Price, data_2014$Region, function(x) shapiro.test(x)$p.value)
print(shapiro_test_results)

#Still not appropriate. Let's do kruskal-wallis for each year

# Eliminate region 1

long_data<-subset(long_data, Region != 1)
# Get unique years in the data
years <- unique(long_data$Year)

# Initialize an empty list to store results
results <- list()

# Loop through each year and perform Kruskal-Wallis test
for(year in years) {
  year_data <- subset(long_data, Year == year)
  test_result <- kruskal.test(Sales.Price ~ Region, data = year_data)
  results[[as.character(year)]] <- test_result
}


# Print results for each year
results

#Post Hoc


#year 2014
# Filtering data for 2014 and excluding Region 1
data_2014_no_region1 <- subset(long_data, Year == 2014 & Region != 1)

# Kruskal-Wallis test
kruskal_result_2014 <- kruskal.test(Sales.Price ~ Region, data = data_2014_no_region1)
print(kruskal_result_2014)
# Assuming you have the 'dunn.test' package installed
library(dunn.test)

# Perform Dunn test if Kruskal-Wallis test is significant
if (kruskal_result_2014$p.value < 0.05) {
    dunn_test_results_2014 <- dunn.test(x = data_2014_no_region1$Sales.Price, 
                                        g = data_2014_no_region1$Region, 
                                        method = "bonferroni")
    print(dunn_test_results_2014)
} else {
    print("Kruskal-Wallis test not significant, no need for Dunn test")
}

#year 2016
# Filtering data for 2016 and excluding Region 1
data_2016_no_region1 <- subset(long_data, Year == 2016 & Region != 1)

# Kruskal-Wallis test
kruskal_result_2016 <- kruskal.test(Sales.Price ~ Region, data = data_2016_no_region1)
print(kruskal_result_2016)
# Assuming you have the 'dunn.test' package installed
library(dunn.test)

# Perform Dunn test if Kruskal-Wallis test is significant
if (kruskal_result_2016$p.value < 0.05) {
    dunn_test_results_2016 <- dunn.test(x = data_2016_no_region1$Sales.Price, 
                                        g = data_2016_no_region1$Region, 
                                        method = "bonferroni")
    print(dunn_test_results_2016)
} else {
    print("Kruskal-Wallis test not significant, no need for Dunn test")
}

#year 2018
# Filtering data for 2018 and excluding Region 1
data_2018_no_region1 <- subset(long_data, Year == 2018 & Region != 1)

# Kruskal-Wallis test
kruskal_result_2018 <- kruskal.test(Sales.Price ~ Region, data = data_2018_no_region1)
print(kruskal_result_2018)
# Assuming you have the 'dunn.test' package installed
library(dunn.test)

# Perform Dunn test if Kruskal-Wallis test is significant
if (kruskal_result_2018$p.value < 0.05) {
    dunn_test_results_2018 <- dunn.test(x = data_2018_no_region1$Sales.Price, 
                                        g = data_2018_no_region1$Region, 
                                        method = "bonferroni")
    print(dunn_test_results_2018)
} else {
    print("Kruskal-Wallis test not significant, no need for Dunn test")
}


#year 2020
# Filtering data for 2020 and excluding Region 1
data_2020_no_region1 <- subset(long_data, Year == 2020 & Region != 1)

# Kruskal-Wallis test
kruskal_result_2020 <- kruskal.test(Sales.Price ~ Region, data = data_2020_no_region1)
print(kruskal_result_2020)
# Assuming you have the 'dunn.test' package installed
library(dunn.test)

# Perform Dunn test if Kruskal-Wallis test is significant
if (kruskal_result_2020$p.value < 0.05) {
    dunn_test_results_2020 <- dunn.test(x = data_2020_no_region1$Sales.Price, 
                                        g = data_2020_no_region1$Region, 
                                        method = "bonferroni")
    print(dunn_test_results_2020)
} else {
    print("Kruskal-Wallis test not significant, no need for Dunn test")
}


#year 2022
# Filtering data for 2022 and excluding Region 1
data_2022_no_region1 <- subset(long_data, Year == 2022 & Region != 1)

# Kruskal-Wallis test
kruskal_result_2022 <- kruskal.test(Sales.Price ~ Region, data = data_2022_no_region1)
print(kruskal_result_2022)
# Assuming you have the 'dunn.test' package installed
library(dunn.test)

# Perform Dunn test if Kruskal-Wallis test is significant
if (kruskal_result_2022$p.value < 0.05) {
    dunn_test_results_2022 <- dunn.test(x = data_2022_no_region1$Sales.Price, 
                                        g = data_2022_no_region1$Region, 
                                        method = "bonferroni")
    print(dunn_test_results_2022)
} else {
    print("Kruskal-Wallis test not significant, no need for Dunn test")
}


