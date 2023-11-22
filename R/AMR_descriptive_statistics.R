# This script is used for some first descriptive statistics with the datasets


# Importing sample level dataset with antibiotics coded as factors
samples <- read_delim("data/processed/AMR/csv_files_for_analysis/sample_level_data_for_analysis.csv", 
delim = ";", escape_double = FALSE, col_types = cols(Nr = col_double(), 
date_farm = col_date(format = "%d.%m.%Y"), 
AMU = col_factor(levels = c("low", "high")), 
age = col_factor(levels = c("calf", "cow")), date_lab = col_date(format = "%d.%m.%Y"), 
AMP = col_factor(levels = c("0", "1")), AZI = col_factor(levels = c("0", "1")), 
AMI = col_factor(levels = c("0", "1")), GEN = col_factor(levels = c("0", "1")), 
TGC = col_factor(levels = c("0", "1")), TAZ = col_factor(levels = c("0", "1")), 
FOT = col_factor(levels = c("0", "1")), COL = col_factor(levels = c("0", "1")), 
NAL = col_factor(levels = c("0", "1")), TET = col_factor(levels = c("0", "1")), 
TMP = col_factor(levels = c("0", "1")), SMX = col_factor(levels = c("0", "1")), 
CHL = col_factor(levels = c("0", "1")), MERO = col_factor(levels = c("0","1")), 
CIP = col_factor(levels = c("0", "1")), multidrug_res = col_factor(levels = c("0", "1")), 
drug_res = col_double(), 
multiclass_res = col_factor(levels = c("0", "1")), class_res = col_double()), 
trim_ws = TRUE)

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


tbl_summary(samples, by = age)


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
  scale_fill_manual(values = c(hcl.colors(5, "Cividis")), labels = c('Calf Samples', 'Cow Samples')) +
  theme_bw() +
  theme(plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5 ),
        legend.title = element_blank(), axis.title.y = element_text(size = 20,margin =margin(r=20)),
        axis.text = element_text(color = "black"),
        panel.grid.major = element_line(linewidth = 0.5, color = "lightgrey", linetype = "dashed"),
        axis.ticks.x = element_line(linewidth = 20)) +
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
  scale_fill_manual(values = c(hcl.colors(5, "Cividis")), labels = c('Calf Samples', 'Cow Samples')) +
  theme_bw() +
  theme(plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5 ),
        legend.title = element_blank(), axis.title.y = element_text(size = 20,margin =margin(r=20)),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1, family = "sans", size = 10),
        panel.grid.major = element_line(linewidth = 0.5, color = "lightgrey", linetype = "dashed"),
        axis.ticks.x = element_line(linewidth = 24)) +
  xlab("") +
  ylab("Resistance Rate") + 
  scale_y_continuous(labels = scales::percent, expand = c(0.01,0), 
                     limits = c(0,0.1)) +
  scale_x_discrete(expand = c(0,0.5)) +
  ggtitle("Resistance Rate against Antimicrobial Classes")

print(plot2)



#plot3: Plot with number of antibiotic classes where resistances occured
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
  geom_bar(position="dodge", stat="identity", width = 0.8) +
  geom_text(aes(), position = position_dodge(0.8), vjust = -0.3,check_overlap = TRUE)+
  scale_fill_manual(values = c(hcl.colors(5, "Cividis")), labels = c('Calf Samples', 'Cow Samples')) +
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



#plot4: Plot with number of class resistances in calfs by AMU
samples_calf %>%
  group_by(AMU) %>%
  summarise('Pan-susceptible' = sum(class_res == 0),
            'Resistance against 1-2 Classes ' = sum(class_res == 1 | class_res == 2),
            'Multiclass Resistance (≥ 3 Classes)' = sum(class_res >= 3)) %>%
  melt() %>%
ggplot(aes(x = variable, y = (value / 50),
            fill = reorder(AMU, -value),
            label = ((value / 50)*100))) +
  geom_bar(position="dodge", stat="identity", width = 0.8)+
  geom_text(aes(), position = position_dodge(0.8), vjust = -0.3, check_overlap = TRUE)+
  scale_fill_manual(values =c(hcl.colors(5, "Cividis")), labels = c('low AMU farm', 'high AMU farm'))+
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
  
  

#plot4: Plot with number of class resistances in cows by AMU

  
  


  
         
















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
            MERO = sum(MERO),
            multiclass_res = sum(multiclass_res))
