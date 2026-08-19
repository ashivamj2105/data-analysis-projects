library(tidyverse)

bank <- read.csv("data/bank-full.csv", sep = ";")

job_subscription <- bank %>%
  group_by(job) %>%
  summarise(
    customers = n(),
    subscriptions = sum(y == "yes"),
    subscription_rate = mean(y == "yes") * 100
  ) %>%
  arrange(desc(subscription_rate))
  
job_subscription %>%
  mutate(subscription_rate = round(subscription_rate, 2)) %>%
  select(job, customers, subscriptions, subscription_rate)

# Conversion rate of students is more than others.
# Subscription rate of students is the best despite smaller volume.

ggplot(
  job_subscription, 
  aes(
    x = reorder(job, subscription_rate),
    y = subscription_rate
  )
) + 
  geom_col(fill = "steelblue") + 
  geom_text(
    aes(label = paste0(round(subscription_rate, 1), "%")),
    hjust = -0.1
  ) + 
  coord_flip() +           # coord flip for horizontal orientation
  labs(
    title = "Term Deposit Subscription Rate by Job",
    x = "Job",
    y = "Subscription Rate(%)"
  ) + 
  theme_minimal()


# Subscription rate by education
education_subscription <- bank %>%
  group_by(education) %>%
  summarise(
    customers = n(),
    subscriptions = sum(y == "yes"),
    subscription_rate = mean(y == "yes") * 100
  ) %>%
  arrange(desc(subscription_rate))

education_subscription

ggplot(
  education_subscription,
  aes(
    x = reorder(education, subscription_rate),
    y = subscription_rate
  )
) + 
  geom_col(fill = "darkgreen") + 
  geom_text(
    aes(
      label = paste0(round(subscription_rate, 1), "%" )),
      hjust = -0.1
  ) +
  coord_flip() +
  labs(
    title = "Term Deposit Subscription Rate by Education",
    x = "Education",
    y = "Subscription Rate (%)"
  )+ 
  theme_minimal()

# Subscription Rate by marital status

marital_subscription <- bank %>%
  group_by(marital) %>%
  summarise(
    customers = n(),
    subscriptions = sum(y == "yes"),
    subscription_rate = mean(y == "yes") * 100
  ) %>%
  arrange(desc(subscription_rate))

marital_subscription

ggplot(
  marital_subscription, 
  aes(
    x = reorder(marital, subscription_rate),
    y = subscription_rate
  )
) + 
  geom_col(fill = "darkorange") + 
  geom_text(
    aes(
      label = paste0(round(subscription_rate, 1), "%")),
      hjust = -0.1
    ) + 
  coord_flip() +
  labs(
    title = "Term Deposit Subscription Rate by Marital Status",
    x = "Marital Status",
    y = "Subscription Rate (%)"
  ) + 
  theme_minimal()

# Subscription Rate by housing Loan

housing_subscription <- bank %>%
  group_by(housing) %>%
  summarise(
    customers = n(),
    subscriptions = sum(y == "yes"),
    subscription_rate = mean(y == "yes") * 100
  ) %>%
  arrange(desc(subscription_rate))

housing_subscription

ggplot(
  housing_subscription, 
  aes(
    x = housing,
    y = subscription_rate
  )
) + 
  geom_col(fill = "purple") + 
  geom_text(
    aes(
      label = paste0(round(subscription_rate, 1), "%")),
    vjust = -0.3
  ) + 
  coord_flip() +
  labs(
    title = "Term Deposit Subscription Rate by Housing Loan Status",
    x = "Housing Loan",
    y = "Subscription Rate (%)"
  ) + 
  theme_minimal()

# Subscription Rate by personal loan

personal_subscription <- bank %>%
  group_by(loan) %>%
  summarise(
    customers = n(),
    subscriptions = sum(y == "yes"),
    subscription_rate = mean(y == "yes") * 100
  ) %>%
  arrange(desc(subscription_rate))

personal_subscription

ggplot(
  personal_subscription, 
  aes(
    x = loan,
    y = subscription_rate
  )
) + 
  geom_col(fill = "firebrick") + 
  geom_text(
    aes(
      label = paste0(round(subscription_rate, 1), "%")),
    vjust = -0.3
  ) + 
  coord_flip() +
  labs(
    title = "Term Deposit Subscription Rate by Personal Loan Status",
    x = "Personal Loan",
    y = "Subscription Rate (%)"
  ) + 
  theme_minimal()

# Subscription Rate by contact method

contact_subscription <- bank %>%
  group_by(contact) %>%
  summarise(
    customers = n(),
    subscriptions = sum(y == "yes"),
    subscription_rate = mean(y == "yes") * 100
  ) %>%
  arrange(desc(subscription_rate))

contact_subscription

ggplot(
  contact_subscription, 
  aes(
    x = contact,
    y = subscription_rate
  )
) + 
  geom_col(fill = "steelblue") + 
  geom_text(
    aes(
      label = paste0(round(subscription_rate, 1), "%")),
    vjust = -0.3
  ) + 
  coord_flip() +
  labs(
    title = "Term Deposit Subscription Rate by Customer Contact Method",
    x = "Contact Method",
    y = "Subscription Rate (%)"
  ) + 
  theme_minimal()

# Subscription Rate in previous campaign

poutcome_subscription <- bank %>%
  group_by(poutcome) %>%
  summarise(
    customers = n(),
    subscriptions = sum(y == "yes"),
    subscription_rate = mean(y == "yes") * 100
  ) %>%
  arrange(desc(subscription_rate))

poutcome_subscription

ggplot(
  poutcome_subscription, 
  aes(
    x = reorder(poutcome, subscription_rate),
    y = subscription_rate
  )
) + 
  geom_col(fill = "seagreen") + 
  geom_text(
    aes(
      label = paste0(round(subscription_rate, 1), "%")),
    vjust = -0.3
  ) + 
  coord_flip() +
  labs(
    title = "Term Deposit Subscription Rate in Previous Campaign",
    x = "Previous Campaign Outcome",
    y = "Subscription Rate (%)"
  ) + 
  theme_minimal()

# Customer Characteristics Summary

customer_characteristics_summary <- list(
  job = job_subscription,
  education = education_subscription,
  marital = marital_subscription,
  housing = housing_subscription,
  personal_loan = personal_subscription,
  contact = contact_subscription,
  previous_campaign = poutcome_subscription
)

customer_characteristics_summary
