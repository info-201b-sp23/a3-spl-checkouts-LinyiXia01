library(dplyr)
library(stringr)
options(dplyr.summarise.inform = FALSE)

checkout <- read.csv("~/Desktop/2017-2023-10-Checkouts-SPL-Data.csv",
                     stringsAsFactors = FALSE)

### Summary Information
checkout_by_class <- checkout %>%
  group_by(UsageClass, CheckoutYear) %>%
  summarize(num_checkout = sum(Checkouts, na.rm = T)) %>%
  filter(CheckoutYear != "2023")

# 1. How has the number of physical checkouts changed over time?(2017-2022)
# decreasing
physical <- checkout_by_class %>%
  filter(UsageClass == "Physical") %>%
  arrange(-num_checkout) 

# 2. How has the number of digital checkouts changed over time?(2017-2022)
# increasing
digital <- checkout_by_class %>%
  filter(UsageClass == "Digital") %>%
  arrange(-num_checkout) 

# 3. Which material type checked out the most in physical class?
# book 6312887
ph_material <- checkout %>%
  filter(UsageClass == "Physical") %>%
  group_by(MaterialType, CheckoutYear) %>%
  summarize(num_checkout = sum(Checkouts, na.rm = T)) %>%
  filter(CheckoutYear != "2023")
  
ph_material %>% 
  group_by(MaterialType) %>%
  summarize(num_material = sum(num_checkout, na.rm = T)) %>%
  filter(num_material == max(num_material)) %>%
  pull(MaterialType)

# 4. Which material type checked out the most in digital class?
# Ebook 5053218
di_material <- checkout %>%
  filter(UsageClass == "Digital") %>%
  group_by(MaterialType, CheckoutYear) %>%
  summarize(num_checkout = sum(Checkouts, na.rm = T)) %>%
  filter(CheckoutYear != "2023")

di_material %>% 
  group_by(MaterialType) %>%
  summarize(num_material = sum(num_checkout, na.rm = T)) %>%
  filter(num_material == max(num_material)) %>%
  pull(MaterialType)

# 5. What material type of the book So You Want to Talk about Race has the most
# checkouts/least checkouts?
# audio/sounddisc

checkout$concise_title[str_detect(checkout$Title, "(?i)so you want to talk about race")] <- "So You Want to Talk about Race"

concise_df <- checkout %>% 
  na.omit(concise_title) %>%
  filter(CheckoutYear != "2023")

checkouts_specific_book <- concise_df %>% 
  group_by(MaterialType, CheckoutYear) %>% 
  summarize(total_checkouts = sum(Checkouts, na.rm = T))



  
  
