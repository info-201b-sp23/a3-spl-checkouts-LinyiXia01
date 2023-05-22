library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
options(dplyr.summarise.inform = FALSE)

checkout <- read.csv("~/Desktop/2017-2023-10-Checkouts-SPL-Data.csv",
                     stringsAsFactors = FALSE)

checkout$concise_title[str_detect(checkout$Title, "(?i)so you want to talk about race")] <- "So You Want to Talk about Race"

concise_df <- checkout %>% 
  na.omit(concise_title) %>%
  filter(CheckoutYear != "2023")

checkouts_specific_book <- concise_df %>% 
  group_by(MaterialType, CheckoutYear) %>% 
  summarize(total_checkouts = sum(Checkouts, na.rm = T))

checkouts_line <- ggplot(data = checkouts_specific_book) +
  geom_line(aes(x = CheckoutYear,
                y = total_checkouts,
                color = MaterialType)) +
  scale_y_continuous(labels = label_number_si()) +
  labs(title = "Checkouts for differnt material type's So You Want to Talk about Race over time",
       x = "Year",
       y = "Total Checkouts",
       color = "Material Type")

checkouts_line
