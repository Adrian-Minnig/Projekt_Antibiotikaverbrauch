#Preliminary data example for annual conference (avoidance distance)
# Dataset is a table with farm id, AMU group and % of animals within the
# different ranges of the Avoidance distance test

#Importing dataset

setwd("~/GitHub/Projekt_Antibiotikaverbrauch")

library(readr)
avoid_dist <- read_delim("data/processed/Example_Indicators/Ausweichdistanz_example.csv", 
                                      delim = ";", escape_double = FALSE, col_types = cols(farm = col_integer(), 
                                      AMU = col_factor(levels = c("zero", "low", "high"))), trim_ws = TRUE)
View(avoid_dist)

# Plot of avoidance distance
ggplot(
avoid_dist%>%
  summarise("0 (Touch)" = mean(touch),
            "0-50" = mean(close), 
            "50-100"= mean(middle),
            ">100" = mean(far)) %>%
  melt(), aes(x = variable, y = value),) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8, fill = "#211158") +
  geom_text(aes(label = paste((round(value, digits = 2)),"%")), 
            position = position_dodge(0.8), vjust = -0.3, check_overlap = TRUE,
            size = 8) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.title.y = element_text(size = 23, margin = margin (r=20)),
    axis.title.x = element_text(size = 23, margin = margin(t=20)),
    axis.text = element_text(color = "black", size = 17),) +
  xlab("Avoidance Distance [cm]") +
  ylab("% of Animals") + 
  scale_x_discrete(expand = c(0,0.45)) +
  scale_y_continuous(labels = function(x) paste(x,"%"), limits = c(0,75), 
                     expand = c(0.01,0), breaks = c(5,10,20,60,70)) +
  ggtitle("Avoidance distance observed on 50 farms (≈ 1300 cows)")









# Plot of avoidance distance by AMU
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
  geom_bar(position="dodge", stat="identity", width = 0.9) +
  geom_text(aes(), position = position_dodge(0.9), vjust = -0.3,check_overlap = TRUE,
  size = 5) +
  scale_fill_manual(values = c(hcl.colors(4, "Batlow")), labels = c('zero (n=6 farms)', 'low (n=22 farms)', 'high (n=22 farms)')) +
  theme_bw() +
  theme(plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5 ),
        legend.position = c(0.75,0.65),
        legend.title = element_text(size = 23, face = "bold"),
        legend.text = element_text(size = 20),
        legend.box.background = element_rect(colour = "black"),
        legend.background = element_blank(),
        axis.title.y = element_text(size = 23,margin =margin(r=20)),
        axis.title.x = element_text(size = 23, margin = margin(t=20)),
        axis.text = element_text(color = "black", size = 17),
        panel.grid.major = element_line(linewidth = 0.5, color = "lightgrey", linetype = "dashed"),
        axis.ticks.x = element_line(linewidth = 90)) +
  labs(fill = 'AMU Group') +
  xlab("Avoidance Distance [cm]") +
  ylab("% of Animals") + 
  scale_x_discrete(expand = c(0.01,0.45)) +
  scale_y_continuous(labels = function(x) paste(x,"%"), limits = c(0,75), 
                     expand = c(0.01,0.1), breaks = c(5,10,20,60,70)) +
  ggtitle("Avoidance distance observed on farms by AMU")



tbl_summary(avoid_dist %>%
              select(AMU, touch, close, middle, far),
            by = AMU, statistic = c(touch, close, middle, far) ~ "{mean}%",
            label = list(touch ~ "0 (Touch)", 
                         close ~ "0-50",
                         middle ~ "50 - 100",
                         far ~ ">100"),
            digits = everything()~ 2) %>%
  modify_header(label = "**Avoidance  \n Distance [cm]**")
  


  




