## grouped bar

## load libraries
library(tidyverse)


## read data
bps_scls3 <- read_csv("bps_scl_grouped_bar/bps_scls3.csv")
View(bps_scls3)

## pivot data longer
bps_scls3 <- bps_scls3 %>%
  pivot_longer(
    cols = c(`Reference`, `Current`), 
    names_to = "refCur", 
    values_to = "Percent"
    )
## order sclasses, then reverse 'cause I messed up (but maybe this will help someone later)
# these HAVE TO MATCH what is in the spreadsheet
# try making chart without lines 21-33 first to see how it looks
bps_scls3$Sclass <- factor(bps_scls3$Sclass, levels = c(
  "Regeneration", 
  "Mid-development, open canopy", 
  "Mid-development, closed canopy", 
  "Late-development, open canopy", 
  "Late-development, closed canopy", 
  "Late-development, closed canopy, tallest",
  "UE", 
  "UN", 
  "Agriculture", 
  "Urban"))
# reverse
factor(bps_scls3$Sclass, levels = rev(levels(bps_scls3$Sclass)))

# make chart.  NOTE lines 56 and 57 where I select the BpS to make the chart for.  Make sure to adjust title, etc.
sclasplot <-
  ggplot(bps_scls3, aes(fill=factor(refCur), y=Percent, x=Sclass)) + 
  geom_col(width = 0.8, position = position_dodge()) +
  coord_flip() +
  #facet_grid(. ~BpS) +
  scale_x_discrete(limits = rev(levels(bps_scls3$Sclass))) +
  labs(
    title = "Succession Classes past and present for the Upper Cheat Watershed",
    subtitle = "Top three most prevalent Biophysical Settings",
    caption = "Late-development, closed canopy, tallest succession class not present in this BpS. \nData from landfire.gov.",
    x = "",
    y = "Percent")+
  theme_minimal(base_size = 14)+
  theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot") +
  scale_fill_manual(values = c("#3d4740", "#32a852" ), # present (grey), historical (green)
                    name = " ", 
                    labels = c("Present",
                               "Past")) +
  facet_grid(~BpS) +
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1))

sclasplot  

### if you want to select one BpS at a time and make a chart:
##  make sure to name the chart in the title or subtitle

# sclasplotHardwoods <- sclasplot %+% subset(bps_scls3, BpS %in% c("Appalachian (Hemlock-)Northern Hardwood Forest"))
# 
# sclasplotHardwoods #may not be able to view here?

ggsave(filename="bps_scl_grouped_bar/example_bps_scl_grouped_bar.png",width=10,height=8,units='in',dpi=300)



