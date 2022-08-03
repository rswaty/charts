

## simple bar chart
library(tidyverse)


# read bps attribute table .csv and summarize
bpsname <- read.csv(file = "bps_barchart/bps_aoi_attributes.csv") %>%
  group_by(BPS_NAME) %>%
  summarize(acres = sum(acres),
            hectare = sum(hectare),
            rel_percent = sum(rel_percent)) %>%
  arrange(desc(rel_percent)) %>%
  top_n(n = 10, wt = rel_percent)

# plot
bpsChart <- 
  ggplot(data = bpsname, aes(x = BPS_NAME, y = rel_percent)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top 10 Biophysical Settings for Deer Creek Landscape",
    subtitle = "Represents dominant vegetation systems pre-European colonization",
    caption = "Represents ~95% of the landscape. \nData from landfire.gov.",
    x = "",
    y = "Percent of landscape") +
  scale_x_discrete(limits = rev(bpsname$BPS_NAME)) +
  coord_flip() +
  theme_bw()

bpsChart

# if the plot looks good, save it
ggsave("bps_barchart/example_bps_barchart.png", width = 12, height = 5)

