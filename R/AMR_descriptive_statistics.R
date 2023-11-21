# This script is used for some first descriptive statistics with the datasets


# Importing sample level dataset with antibiotics tested as factors
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


      
samples %>% 
  group_by(age) %>% 
  summarise(AMP = sum(AMP),
            AZI = sum(AZI),
            AMI = sum(AMI),
            GEN = sum(GEN),
            TGC = sum(TGC),
            TAZ = sum(TAZ),
            FOT = sum(FOT),
            COL = sum(COL),
            NAL = sum(NAL),
            TET = sum(TET),
            TMP = sum(TMP),
            SMX = sum(SMX),
            CHL = sum(CHL),
            MERO = sum(MERO),
            CIP = sum(CIP)) %>%
  janitor::adorn_totals(name = "All age groups") %>% 
  slice(3:n(), 1:2) %>% 
  melt() %>%
  ggplot(aes(x = variable, y = (value / length(samples$Nr)), fill = reorder(age, -value), label = (value / length(samples$Nr) ))) +
  geom_bar(position="dodge", stat="identity") +
  #geom_text(aes(), position = position_dodge(0.8), vjust = -0.3)+
  scale_fill_manual(values = c(hcl.colors(3, "Dynamic"))) +

  theme_bw() +
  theme(legend.title=element_blank()) +
  xlab("") +
  ylab("Resistance rate") + 
  scale_y_continuous(labels = scales::percent)

samples %>% 
  group_by(age, AMU) %>% 
  summarise(sum(multidrug_res), 
            sum(multiclass_res),
            sum(drug_res))
