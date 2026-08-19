# PROBLEM STATEMENT
# The objective is to explore the data and 
# predict if a bank customer will subscribe to a term/savings deposit based on
# demographic, financial and campaign related characteristics


library(tidyverse)
library(ggplot2)

bank <- read.csv("data/bank-full.csv", sep = ";")

head(bank)
str(bank)
summary(bank)

dim(bank)

names(bank)

colSums(is.na(bank))

table(bank$y)

prop.table(table(bank$y)) * 100

subscription_summary <- bank %>%
  count(y) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 2)
  )

subscription_summary

ggplot(subscription_summary, aes(
  x = y, 
  y = n,
  fill = y
)) + 
  geom_col() +
  geom_text(
    aes(label = paste0(percentage, "%")),
    vjust = -0.5
  ) + 
  labs(title = "Term Deposit Subscription", 
       x = "Subscription", 
       y = "Number of Customers"
  ) + 
  theme_minimal() +
  theme(legend.position = "none")

sapply(bank, function(x) sum(x == "unknown"))

sum(duplicated(bank))

