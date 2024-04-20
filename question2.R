library(dplyr)
library(readr)

# i. Read the data set and print the first 6 rows
chocolate <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv")
head(chocolate)

# ii. Convert all characters to factors
chocolate <- chocolate %>% mutate_all(as.factor)

# iii. Obtain statistics by company location
chocolate %>%
  group_by(company_location) %>%
  summarise(mean = mean(rating),
            sd = sd(rating),
            median = median(rating),
            range = max(rating) - min(rating)) %>%
  head(10)

# iv. Filter chocolates for review date 2020 and bean origin Colombia
filtered_chocolates <- chocolate %>%
  filter(review_date == 2020 & country_of_bean_origin == "Colombia")

# v. Calculate mean rating and cocoa percent by company location
chocolate %>%
  group_by(company_location) %>%
  summarise(mean_rating = mean(rating),
            mean_cocoa_percent = mean(cocoa_percent))

# vi. Select specific columns and print first 10 rows
chocolate %>%
  select(company_manufacturer, company_location, country_of_bean_origin) %>%
  head(10)

# vii. Filter chocolates in Switzerland with rating between 3.25 and 3.5
chocolate %>%
  filter(company_location == "Switzerland" & rating >= 3.25 & rating <= 3.5) %>%
  head(5)

# viii. Calculate mean rating for each company location and order by descending
chocolate %>%
  group_by(company_location) %>%
  summarise(mean_rating = mean(rating)) %>%
  arrange(desc(mean_rating))

# ix. Count observations for Bonnat by country of bean origin
chocolate %>%
  filter(company_manufacturer == "Bonnat") %>%
  count(country_of_bean_origin)

# x. Create Rating Percentage and Class columns
chocolate <- chocolate %>%
  mutate(Rating_Percentage = rating * 25,
         Class = case_when(
           Rating_Percentage < 25 ~ "Low",
           Rating_Percentage >= 25 & Rating_Percentage < 50 ~ "Medium",
           Rating_Percentage >= 50 & Rating_Percentage < 87.5 ~ "Tasty",
           Rating_Percentage >= 87.5 ~ "Excellent"
         ))