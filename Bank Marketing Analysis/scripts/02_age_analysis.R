library(tidyverse)

bank <- read.csv("data/bank-full.csv", sep = ";")

# Customer Ages

summary(bank$age)

mean(bank$age)

median(bank$age)

sd(bank$age)

# Age Distribution

ggplot(bank, aes(x = age)) + 
  geom_histogram(
    binwidth = 5, 
    fill = "steelblue",
    color = " white"
  ) + 
  labs( 
    title = "Age Distribution of Customers",
    x = "Age", 
    y = "Number of Customers"
  ) +
  theme_minimal()

# Age relation to subscription

ggplot(bank, aes(
  x = age,
  fill = y
)) + 
  geom_histogram(
    binwidth = 5,
    position = "identity",
    alpha = 0.5
  ) + 
  labs (
    title = "Age Distribution by Term Deposit Subscription",
    x = "Age", 
    y = "Number of Customers",
    fill = "Subscribed"
  ) + 
  theme_minimal()

# Age Groups 

bank <- bank %>%
  mutate(
    age_group = case_when(
      age < 25 ~"18-24",
      age < 35 ~"25-34",
      age < 45 ~"35-44",
      age < 55 ~"45-54",
      age < 65 ~"55-64",
      TRUE ~ "65+"
    )
  )

table(bank$age_group)

# Customers by age group
age_summary <- bank %>%
  count(age_group)

age_summary

# Age group Distribution

ggplot(age_summary, aes(
  x = age_group, 
  y = n
)) + 
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = n),
    vjust = -0.5
  ) + 
  labs(
    title = "Customer Distribution by Age Group",
    x = "Age Group", 
    y = "Number of Customers"
  ) + 
  theme_minimal()

# Subscription by age group
age_subscription <- bank %>%
  group_by(age_group) %>%
  summarise(
    customers = n(),
    subscriptions = sum(y == "yes"),
    subscription_rate = mean(y == "yes") * 100
  )

age_subscription


ggplot(
  age_subscription,
  aes(x = age_group, y = subscription_rate)
) +
  geom_col(fill = "darkgreen") +
  geom_text(
    aes(label = paste0(round(subscription_rate, 1), "%")),
    vjust = -0.5
  ) +
  labs(
    title = "Term Deposit Subscription Rate by Age Group",
    x = "Age Group",
    y = "Subscription Rate (%)"
  ) +
  theme_minimal()
