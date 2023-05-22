library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
options(dplyr.summarise.inform = FALSE)

checkout <- read.csv("~/Desktop/2017-2023-10-Checkouts-SPL-Data.csv",
                     stringsAsFactors = FALSE)

di_material <- checkout %>%
  filter(UsageClass == "Digital") %>%
  group_by(MaterialType, CheckoutYear) %>%
  summarize(num_checkout = sum(Checkouts, na.rm = T)) %>%
  filter(CheckoutYear != "2023")

digital_bar <- ggplot(data = di_material) +
  geom_col(aes(x = CheckoutYear,
               y = num_checkout,
               fill = MaterialType)) +
  scale_y_continuous(labels = label_number_si()) +
  labs(title = "Different Digital Checkouts from 2017 to 2022",
       x = "Year",
       y = "Total Checkouts",
       fill = "Material Type")

digital_bar