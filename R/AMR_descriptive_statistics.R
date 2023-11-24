# This script is used for some first descriptive statistics with the datasets


# Importing sample level dataset with antibiotics coded as factors
#samples <- read_delim("data/processed/AMR/csv_files_for_analysis/sample_level_data_for_analysis.csv", 
#delim = ";", escape_double = FALSE, col_types = cols(Nr = col_double(), 
#date_farm = col_date(format = "%d.%m.%Y"), 
#AMU = col_factor(levels = c("low", "high")), 
#age = col_factor(levels = c("calf", "cow")), date_lab = col_date(format = "%d.%m.%Y"), 
#AMP = col_factor(levels = c("0", "1")), AZI = col_factor(levels = c("0", "1")), 
#AMI = col_factor(levels = c("0", "1")), GEN = col_factor(levels = c("0", "1")), 
#TGC = col_factor(levels = c("0", "1")), TAZ = col_factor(levels = c("0", "1")), 
#FOT = col_factor(levels = c("0", "1")), COL = col_factor(levels = c("0", "1")), 
#NAL = col_factor(levels = c("0", "1")), TET = col_factor(levels = c("0", "1")), 
#TMP = col_factor(levels = c("0", "1")), SMX = col_factor(levels = c("0", "1")), 
#CHL = col_factor(levels = c("0", "1")), MERO = col_factor(levels = c("0","1")), 
#CIP = col_factor(levels = c("0", "1")), multidrug_res = col_factor(levels = c("0", "1")), 
#drug_res = col_double(), 
#multiclass_res = col_factor(levels = c("0", "1")), class_res = col_double()), 
#trim_ws = TRUE)

#Importing sample level dataset with antibiotics coded as numerics
# this makes it easier to plot the data with ggplot
samples <- read_delim("data/processed/AMR/csv_files_for_analysis/sample_level_data_for_analysis.csv", 
                      delim = ";", escape_double = FALSE, col_types = cols(Nr = col_double(), 
                                                                           date_farm = col_date(format = "%d.%m.%Y"))
                      ,trim_ws = TRUE)

View(samples)



# Filtering samples by age 
samples %>%             
  filter(age == "calf")
samples_calf <- samples %>%
  filter(age == "calf")

samples %>%             
  filter(age == "cow")
samples_cow <- samples %>%
  filter(age == "cow")


summary(samples)
summary(samples_calf)
summary(samples_cow)




# plot1: Plot with all antibiotics ordered by decreasing number of resistant isolates      
plot1 <-
  samples %>% 
  group_by(age) %>% 
  summarise(TET = sum(TET),
            AMP = sum(AMP),
            SMX = sum(SMX),
            TMP = sum(TMP),
            CHL = sum(CHL),
            GEN = sum(GEN),
            CIP = sum(CIP),
            NAL = sum(NAL),
            AZI = sum(AZI),
            FOT = sum(FOT),
            AMI = sum(AMI),
            TGC = sum(TGC),
            TAZ = sum(TAZ),
            COL = sum(COL),
            MERO = sum(MERO)) %>%
  #janitor::adorn_totals(name = "All age groups") %>% 
  slice(3:n(), 1:2) %>% 
  melt() %>%
  ggplot(aes(x = variable, y = (value / length(samples$Nr)), 
             fill = reorder(age, -value), 
             label = ifelse(variable == "TET" | 
                              variable == "AMP" |
                              variable == "SMX" |
                              variable == "TMP" |
                              variable == "CHL" |
                              variable == "GEN" |
                              variable == "CIP" |
                              variable == "NAL" |
                              variable == "AZI" |
                              variable == "FOT" ,(value / length(samples$Nr)*100),""))) +
  geom_bar(position="dodge", stat="identity", width = 0.8) +
  geom_text(aes(), position = position_dodge(0.8), vjust = -0.3,check_overlap = TRUE)+
  scale_fill_manual(values = c(hcl.colors(5, "Cividis")), labels = c('Calf Samples (n=50)', 'Cow Samples (n=150)')) +
  theme_bw() +
  theme(plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5 ),
        legend.title = element_blank(), axis.title.y = element_text(size = 20,margin =margin(r=20)),
        axis.text = element_text(color = "black"),
        panel.grid.major = element_line(linewidth = 0.5, color = "lightgrey", linetype = "dashed"),
        axis.ticks.x = element_line(linewidth = 22)) +
  xlab("") +
  ylab("Resistance Rate") + 
  scale_y_continuous(labels = scales::percent, expand = c(0.01,0), 
                     limits = c(0,0.1)) +
  scale_x_discrete(expand = c(0,0.5)) +
  ggtitle("Resistance Rate of Antibiotics tested")

print(plot1)


# plot2: Plot with resistance rate by antibiotic class
plot2 <-
samples %>% 
  group_by(age) %>% 
  summarise(Tetracyclines = sum(TET),
            Penicillins = sum(AMP),
            Sulfonamides = sum(SMX),
            Aminopyrimidines = sum(TMP),
            Phenicoles = sum(CHL),
            Aminoglycosides = sum(GEN,AMI),
            Quinolones = sum(CIP,NAL),
            Macrolides = sum(AZI),
            Cephalosporins = sum(FOT,TAZ),
            Glycylcyclines = sum(TGC),
            Polypeptides = sum(COL),
            Carbapenemns = sum(MERO)) %>%
  #janitor::adorn_totals(name = "All age groups") %>% 
  slice(3:n(), 1:2) %>% 
  melt() %>%
  ggplot(aes(x = variable, y = (value / length(samples$Nr)), 
             fill = reorder(age, -value), 
             label = ifelse(variable == "Penicillins" | 
                            variable == "Tetracyclines" |
                            variable == "Sulfonamides" |
                            variable == "Aminopyrimidines" |
                            variable == "Phenicoles" |
                            variable == "Aminoglycosides" |
                            variable == "Quinolones" |
                            variable == "Macrolides" |
                            variable == "Cephalosporins",
                            ((value / length(samples$Nr)*100)),""))) +
  geom_bar(position="dodge", stat="identity", width = 0.8) +
  geom_text(aes(), position = position_dodge(0.8), vjust = -0.3,check_overlap = TRUE)+
  scale_fill_manual(values = c(hcl.colors(5, "Cividis")), labels = c('Calf Samples (n=50)', 'Cow Samples (n=150)')) +
  theme_bw() +
  theme(plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5 ),
        legend.title = element_blank(), axis.title.y = element_text(size = 20,margin =margin(r=20)),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1, family = "sans", size = 10),
        panel.grid.major = element_line(linewidth = 0.5, color = "lightgrey", linetype = "dashed"),
        axis.ticks.x = element_line(linewidth = 28)) +
  xlab("") +
  ylab("Resistance Rate") + 
  scale_y_continuous(labels = scales::percent, expand = c(0.01,0), 
                     limits = c(0,0.1)) +
  scale_x_discrete(expand = c(0,0.5)) +
  ggtitle("Resistance Rate against Antimicrobial Classes")

print(plot2)



#plot3: Plot with number of antibiotic classes where resistances occured
plot3 <-
samples %>%
  group_by(age) %>%
  summarise('Pan-susceptible' = sum(class_res == 0),
            'Resistance against 1-2 Classes ' = sum(class_res == 1 | class_res == 2),
            'Multiclass Resistance (≥ 3 Classes)' = sum(class_res >= 3),) %>%
  slice(3:n(), 1:2) %>% 
  melt() %>%
  ggplot(aes(x = variable, y = (value / length(samples$Nr)), 
             fill = reorder(age, -value), 
             label = paste("n=", value))) + 
             #label = paste(((value / 200)*100),"%"))) +
  geom_bar(position="dodge", stat="identity", width = 0.8) +
  geom_text(aes(), position = position_dodge(0.8), vjust = -0.3,check_overlap = TRUE)+
  scale_fill_manual(values = c(hcl.colors(5, "Cividis")), labels = c('Calf Samples (n=50)', 'Cow Samples (n=150)')) +
  theme_bw() +
  theme(plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5 ),
        legend.title = element_blank(), axis.title.y = element_text(size = 20,margin =margin(r=20)),
        legend.text = element_text(size = 15),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(#angle = 45, hjust = 1, 
                                   family = "sans", size = 15),
        panel.grid.major = element_line(linewidth = 0.5, color = "lightgrey", linetype = "dashed")) +
  xlab("") +
  ylab("% of E.coli isolates") + 
  scale_y_continuous(labels = scales::percent, expand = c(0.01,0), 
                     limits = c(0,0.75), breaks = c(0.05,0.1,0.2,0.5,0.6,0.7,0.8),) +
  scale_x_discrete(expand = c(0.21,0)) +
  ggtitle("Occurence of (Multi)Class Resistances")

print(plot3)


#plot4: Plot with number of class resistances in calfs by AMU
plot4 <-
samples_calf %>%
  group_by(AMU) %>%
  summarise('Pan-susceptible' = sum(class_res == 0),
            'Resistance against 1-2 Classes ' = sum(class_res == 1 | class_res == 2),
            'Multiclass Resistance (≥ 3 Classes)' = sum(class_res >= 3)) %>%
  melt() %>%
ggplot(aes(x = variable, y = (value / 50),
            fill = reorder(AMU, -value),
            label = paste(((value / 50)*100),"%"))) +
  geom_bar(position="dodge", stat="identity", width = 0.8)+
  geom_text(aes(), position = position_dodge(0.8), vjust = -0.3, check_overlap = TRUE)+
  scale_fill_manual(values =c(hcl.colors(5, "Cividis")), labels = c('low AMU farm (n=28)', 'high AMU farm (n=22)'))+
  theme_bw() +
  theme(plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5 ),
      legend.title = element_blank(), axis.title.y = element_text(size = 20,margin =margin(r=20)),
      legend.text = element_text(size = 15),
      axis.text = element_text(color = "black"),
      axis.text.x = element_text(#angle = 45, hjust = 1, 
        family = "sans", size = 15),
      panel.grid.major = element_line(linewidth = 0.5, color = "lightgrey", linetype = "dashed")) +
  xlab("")+
  ylab("% of E.coli isolates") +
  scale_y_continuous(labels = scales::percent, expand = c(0.01,0), limits = c(0,0.35),
                     breaks = c(0.1,0.2,0.3,0.35)) +
  scale_x_discrete(expand = c(0,0.43)) +
  ggtitle("(Multi)Class Resistance in Calves in low vs. high AMU Farms")

print(plot4)
  
#plot5: Plot with number of class resistances in cows by AMU
plot5 <-
samples_cow %>%
  group_by(AMU) %>%
  summarise('Pan-susceptible' = sum(class_res == 0),
            'Resistance against 1-2 Classes ' = sum(class_res == 1 | class_res == 2),
            'Multiclass Resistance (≥ 3 Classes)' = sum(class_res >= 3)) %>%
  melt() %>%
ggplot(aes(x = variable, y = (value / 150),
           fill = reorder(AMU, -value),
           label = scales::percent((value / 150), accuracy=0.1))) +
  geom_bar(position="dodge", stat="identity", width = 0.8)+
  geom_text(aes(), position = position_dodge(0.8), vjust = -0.3, check_overlap = TRUE)+
  scale_fill_manual(values =c(hcl.colors(5, "Cividis")), labels = c('low AMU farm (n=28)', 'high AMU farm (n=22)'))+
  theme_bw() +
  theme(plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5 ),
        legend.title = element_blank(), axis.title.y = element_text(size = 20,margin =margin(r=20)),
        legend.text = element_text(size = 15),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(#angle = 45, hjust = 1, 
          family = "sans", size = 15),
        panel.grid.major = element_line(linewidth = 0.5, color = "lightgrey", linetype = "dashed"))+
  xlab("")+
  ylab("% of E.coli isolates") +
  scale_y_continuous(labels = scales::percent, expand = c(0.01,0), limits =c(0,0.6),
                                                               breaks = c(0.05,0.1,0.2,0.3,0.4,0.5,0.6))+
  scale_x_discrete(expand = c(0,0.43)) +
  ggtitle("(Multi)Class Resistance in Cows in low vs. high AMU Farms")

print(plot5)
         
 



#adding additional variable positive (1 = sample has at least one resistance,
#otherwise, 0)
samples$positive <- ifelse(samples$drug_res > 0,1,0)
 
#preparing two summary tables to merge
table_positive <-
tbl_summary((samples %>%
            select(age, positive) %>%
            mutate(age = case_when(age == "calf" ~ "Calf",
                                     age == "cow" ~  "Cow"))),
            by = positive,
            label = list(age ~ "Age group"),
            percent = "row") %>%
            add_overall(statistic = ~ "{n}") %>%
            modify_header(label = "",
                          stat_0 = '**Overall**  \n N=200',
                          stat_1 = '**Pansusceptible**  \n N=174',
                          stat_2 = '**Resistant**  \n N=26') %>%
            modify_footnote(stat_2 ~ "Resistant = Sample with at least one resistant isolate",
                            stat_0 ~ NA)
            
           
            
table_multiclass_res <-
tbl_summary((samples %>%
               select(age, multiclass_res) %>%
               mutate(age = case_when(age == "calf" ~ "Calf",
                                      age == "cow" ~  "Cow"))),
            by = multiclass_res,
            label = list(age ~ "Age group"),
            percent = "row", 
            statistic = multiclass_res  ~ "{n}/{N} ({p}%)") %>%
                    modify_header(label = "",
                    stat_1 = '**Resistances against fewer than 3 Classes**,  \n N=183',
                    stat_2 = '**Multiclass Resistance<sup>3</sup>**  \n N=17 (of 200)') %>%
            modify_footnote(stat_2 ~ "Multiclass Resistance = resistant against ≥3 classes") %>%
            modify_column_hide(stat_1)
            
            
#table_1: Summary table of fecal samples
table_1 <-
tbl_merge(tbls =list(table_positive, table_multiclass_res), 
          tab_spanner = c('**Number of pansusceptible  \n vs. positive samples**',
                          "**Number of multiclass resistant  \n samples (of all 200)**")) #%>%
          
          #modify_caption('**Summary-Table of Fecal Samples**') 
          
          
print(table_1)
  


#Importing sample level dataset with antibiotics coded as numerics
# this makes it easier to plot the data with ggplot
farms <- read_delim("data/processed/AMR/csv_files_for_analysis/farm_level_data_for_analysis.csv", 
                      delim = ";", escape_double = FALSE, col_types = cols(Nr = col_double(), 
                                                                           date_farm = col_date(format = "%d.%m.%Y"))
                      ,trim_ws = TRUE)

View(farms)

#plot6: Plot with farms with resistances

plot <-
farms %>% 
  group_by(AMU) %>% 
  summarise(TET = sum(TET > 0, na.rm=TRUE),
            AMP = sum(AMP > 0, na.rm=TRUE),
            SMX = sum(SMX > 0, na.rm=TRUE),
            TMP = sum(TMP > 0, na.rm=TRUE),
            CHL = sum(CHL > 0, na.rm=TRUE),
            GEN = sum(GEN > 0, na.rm=TRUE),
            CIP = sum(CIP > 0, na.rm=TRUE),
            NAL = sum(NAL > 0, na.rm=TRUE),
            AZI = sum(AZI > 0, na.rm=TRUE),
            FOT = sum(FOT > 0, na.rm=TRUE),
            AMI = sum(AMI > 0, na.rm=TRUE),
            TGC = sum(TGC > 0, na.rm=TRUE),
            TAZ = sum(TAZ > 0, na.rm=TRUE),
            COL = sum(COL > 0, na.rm=TRUE),
            MERO = sum(MERO > 0, na.rm=TRUE)) %>%
  #janitor::adorn_totals(name = "All age groups") %>% 
  melt() %>%
  ggplot(aes(x = variable, y = (value / length(farms$farm)), 
             fill = reorder(AMU, -value), 
             label = ifelse(variable == "TET" | 
                              variable == "AMP" |
                              variable == "SMX" |
                              variable == "TMP" |
                              variable == "CHL" |
                              variable == "GEN" |
                              variable == "CIP" |
                              variable == "NAL" #|
                              #variable == "AZI" |
                              #variable == "FOT" 
                            ,(value / length(farms$farm)*100),""))) +
  geom_bar(position="stack", stat="identity", width = 0.8) +
  geom_text(aes(), position = position_stack( vjust = 0.5),check_overlap = TRUE, color = "white")+
  scale_fill_manual(values = c(hcl.colors(5, "Cividis")), labels = c('low AMU farms (n=28)', 'high AMU farms (n=22)')) +
  theme_bw() +
  theme(plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5 ),
        legend.title = element_blank(), axis.title.y = element_text(size = 20,margin =margin(r=20)),
        axis.text = element_text(color = "black"),
        panel.grid.major = element_line(linewidth = 0.5, color = "lightgrey", linetype = "dashed"),
        axis.ticks.x = element_line(linewidth = 22)) +
  xlab("") +
  ylab("Resistance Rate") + 
  scale_y_continuous(labels = scales::percent, expand = c(0.01,0.003), 
                     limits = c(0,0.42), breaks =c(0.1,0.2,0.3,0.4)) +
  scale_x_discrete(expand = c(0,0.5)) +
  ggtitle("% of low vs. high AMU farms with at least one resistance (antibiotics)")

print(plot6)



#plot7: not working so far...
farms %>% 
  group_by(AMU) %>% 
  summarise(Tetracyclines = sum(TET > 0, na.rm=TRUE),
            Penicillins = sum(AMP > 0, na.rm=TRUE),
            Sulfonamides = sum(SMX > 0, na.rm=TRUE),
            Aminopyrimidines = sum(TMP > 0, na.rm=TRUE),
            Phenicoles = sum(CHL > 0, na.rm=TRUE),
            Aminoglycosides = sum(sum(GEN > 0, na.rm=TRUE),sum(AMI > 0, na.rm=TRUE)),
            Quinolones = sum(sum(CIP > 0, na.rm=TRUE), sum(NAL> 0, na.rm=TRUE)),
            Macrolides = sum(AZI> 0, na.rm=TRUE),
            Cephalosporins = sum(sum(FOT > 0, na.rm=TRUE),sum(TAZ> 0, na.rm=TRUE)),
            Glycylcyclines = sum(TGC> 0, na.rm=TRUE),
            Polypeptides = sum(COL> 0, na.rm=TRUE),
            Carbapenemns = sum(MERO> 0, na.rm=TRUE)) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (value / length(farms$farm)), 
             fill = reorder(AMU, -value), 
             label = ifelse(variable == "Penicillins" | 
                              variable == "Tetracyclines" |
                              variable == "Sulfonamides" |
                              variable == "Aminopyrimidines" |
                              variable == "Phenicoles" |
                              variable == "Aminoglycosides" |
                              variable == "Quinolones", #|
                              #variable == "Macrolides" |
                              #variable == "Cephalosporins",
                            ((value / length(farms$farm)*100)),""))) +
  geom_bar(position="stack", stat="identity", width = 0.8) +
  geom_text(aes(), position = position_stack( vjust = 0.5),check_overlap = TRUE, color = "white")+
  scale_fill_manual(values = c(hcl.colors(5, "Cividis")), labels = c('low AMU farms (n=28)', 'high AMU farms (n=22)')) +
  theme_bw() +
  theme(plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5 ),
        legend.title = element_blank(), axis.title.y = element_text(size = 20,margin =margin(r=20)),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1, family = "sans", size = 10),
        panel.grid.major = element_line(linewidth = 0.5, color = "lightgrey", linetype = "dashed"),
        axis.ticks.x = element_line(linewidth = 22)) +
  xlab("") +
  ylab("Resistance Rate") + 
  scale_y_continuous(labels = scales::percent, expand = c(0.01,0.003), 
                     limits = c(0,0.42), breaks =c(0.1,0.2,0.3,0.4)) +
  scale_x_discrete(expand = c(0,0.5)) +
  ggtitle("% of low vs. high AMU farms with at least one resistance (classes)")









