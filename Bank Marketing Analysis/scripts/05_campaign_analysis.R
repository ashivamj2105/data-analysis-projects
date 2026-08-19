library(tidyverse)

bank <- read.csv("data/bank-full.csv", sep = ";")

# Subscription rate by month 
month_subscription <- bank %>%
  group_by(month) %>%
  summarise(
    customers = n(),
    subscriptions = sum(y == "yes"),
    subscription_rate = mean(y == "yes")* 100
  ) %>%
  arrange(desc(subscription_rate))

month_subscription

ggplot(
  month_subscription, aes(
    x = reorder(month, subscription_rate),
    y = subscription_rate
  )
) + 
  geom_col(fill = "steelblue") + 
  geom_text(
    aes(
      label = paste0(round(subscription_rate, 1), "%")),
      hjust = -0.1
    ) +
  coord_flip() +
  labs(
    title = "Term Deposit Susbcription Rate By Month",
    x = "Month", 
    y = "Susbcription Rate(%)"
  ) + 
  theme_minimal()

# Number of campaign contacts

summary(bank$campaign)

campaign_subscription <- bank %>%
  group_by(campaign) %>%
  summarise(
    customers = n(),
    subscriptions = sum(y == "yes"),
    subscription_rate = mean(y == "yes") * 100
  )

campaign_subscription

ggplot(
  campaign_subscription %>% filter(customers >= 50),
  aes(
    x = campaign,
    y = subscription_rate)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Subscription Rate by Number of Campaign Contacts",
    x = "Number of Contacts",
    y = "Subscription Rate (%)"
  ) +
  theme_minimal()

# Previous contact history

previous_summary <- bank %>%
  group_by(previous) %>%
  summarise(
    customers = n(),
    subscriptions = sum(y == "yes"),
    subscription_rate = mean(y == "yes") * 100
  )

previous_summary

# pDays => -1 indicated customer was not contacted previously

bank <- bank %>%
  mutate(
    previously_contacted = ifelse(pdays == -1, "No", "Yes")
  )

pdays_summary <- bank %>%
  group_by(previously_contacted) %>%
  summarise(
    customers = n(),
    subscriptions = sum(y == "yes"),
    subscription_rate = mean(y == "yes") * 100
  )

pdays_summary

ggplot(pdays_summary,aes(
  x = previously_contacted, 
  y = subscription_rate
)) + 
  geom_col(fill = "darkgreen") + 
  geom_text(
    aes(
      label = paste0(round(subscription_rate, 1), "%")
    ),
    vjust = -0.3
  ) + 
  labs(
    title = "Subscription Rate by Previous Contact Status",
    x = "Previously Contacted",
    y = "Subscription Rate(%)"
  ) +
  theme_minimal()
