library(tidyverse)

bank <- read.csv("data/bank-full.csv", sep = ";")

summary(bank$balance)

mean(bank$balance)

median(bank$balance)

sd(bank$balance)

# Balance Distribution
ggplot(bank, aes(x = balance)) + 
  geom_histogram(
    binwidth = 500, 
    fill = "steelblue",
    color = "white"
  ) +  
  labs(
    title = "Distribution of Customer Balance",
    x = "Account Balance",
    y = "Number of Customers"
  ) + 
  theme_minimal()

# Balance by subscription
ggplot(bank, aes(
  x = y, 
  y = balance, 
  fill = y
)) + 
  geom_boxplot() + 
  labs(
    title = "Account Balance by Term Deposit Subscription",
    x = "Subscription",
    y = "Account Balance"
  ) + 
  theme_minimal()

# Comparing mean and median balance
balance_summary <- bank %>%
  group_by(y) %>%
  summarise(
    customers = n(),
    mean_bal = mean(balance),
    median_bal = median(balance)
  )
balance_summary
