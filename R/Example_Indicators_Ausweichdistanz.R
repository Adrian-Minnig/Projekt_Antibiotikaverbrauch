#Preliminary data example for annual conference (avoidance distance)
# Dataset is a table with farm id, AMU group and % of animals within the
# different ranges of the Avoidance distance test

#Importing dataset
library(readr)
avoid_dist <- read_delim("data/processed/Example_Indicators/Ausweichdistanz_example.csv", 
                                      delim = ";", escape_double = FALSE, col_types = cols(farm = col_integer(), 
                                      AMU = col_factor(levels = c("zero", "low", "high"))), trim_ws = TRUE)
View(avoid_dist)

avoid_dist%>%
  group_by(AMU) %>%
  summarise("0 (Touch)" = mean(touch),
            "0-50" = mean(close), 
            "50-100"= mean(middle),
            ">100" = mean(far)) %>%
  melt() %>%
  ggplot(aes(x = variable, y = value, 
             #fill = reorder(AMU, -value), 
             fill = AMU, -value,
             label = paste((round(value, digits = 2)),"%"))) +
  geom_bar(position="dodge", stat="identity", width = 0.8) +
  geom_text(aes(), position = position_dodge(0.8), vjust = -0.3,check_overlap = TRUE)+
  scale_fill_manual(values = c(hcl.colors(4, "Batlow")), labels = c('zero (n=6 farms)', 'low (n=22 farms)', 'high (n=22 farms)')) +
  theme_bw() +
  theme(plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5 ),
        legend.title = element_text(size = 20), #face = "bold"),
        legend.text = element_text(size = 15),
        axis.title.y = element_text(size = 20,margin =margin(r=20)),
        axis.title.x = element_text(size = 20, margin = margin(t=20)),
        axis.text = element_text(color = "black", size = 15),
        panel.grid.major = element_line(linewidth = 0.5, color = "lightgrey", linetype = "dashed"),
        axis.ticks.x = element_line(linewidth = 65)) +
  labs(fill = 'AMU Group') +
  xlab("Avoidance Distance [cm]") +
  ylab("% of Animals") + 
  scale_x_discrete(expand = c(0,0.45)) +
  scale_y_continuous(labels = function(x) paste(x,"%"), limits = c(0,75), 
                     expand = c(0.01,0), breaks = c(5,10,20,60,70)) +
  ggtitle("Avoidance distance observed on farms")



tbl_summary(avoid_dist %>%
              select(AMU, touch, close, middle, far),
            by = AMU, statistic = c(touch, close, middle, far) ~ "{mean}%",
            label = list(touch ~ "0 (Touch)", 
                         close ~ "0-50",
                         middle ~ "50 - 100",
                         far ~ ">100"),
            digits = everything()~ 2) %>%
  modify_header(label = "**Avoidance  \n Distance [cm]**")
  


  




