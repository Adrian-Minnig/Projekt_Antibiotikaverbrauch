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
  summarise(Touch = mean(touch),
            "0-50" = mean(close),
            "50-100"= mean(middle),
            ">100" = mean(far)) %>%
  melt() %>%
  ggplot(aes(x = variable, y = value, 
             fill = reorder(AMU, -value), 
             label = paste((round(value, digits = 2)),"%"))) +
  geom_bar(position="dodge", stat="identity", width = 0.8) +
  geom_text(aes(), position = position_dodge(0.8), vjust = -0.3,check_overlap = TRUE)+
  scale_fill_manual(values = c(hcl.colors(5, "Cividis")), labels = c('AMU1', 'AMU2', 'AMU3')) +
  theme_bw() +
  theme(plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5 ),
        legend.title = element_blank(), axis.title.y = element_text(size = 20,margin =margin(r=20)),
        axis.text = element_text(color = "black"),
        panel.grid.major = element_line(linewidth = 0.5, color = "lightgrey", linetype = "dashed"),
        axis.ticks.x = element_line(linewidth = 22)) +
  xlab("Avoidance Distance") +
  ylab("Percentage of Animals") + 
  scale_x_discrete(expand = c(0,0.5)) +
  scale_y_continuous(labels = function(x) paste(x,"%")) +
  ggtitle("Avoidance Distance")