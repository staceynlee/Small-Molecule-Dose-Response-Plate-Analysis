# Load libraries
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

# Merge individual plate data
d1 <- read_excel("Mock Dose Response Data.xlsx", sheet=1, col_names = FALSE)
d2 <- read_excel("Mock Dose Response Data.xlsx", sheet=2, col_names = FALSE)
d3 <- read_excel("Mock Dose Response Data.xlsx", sheet=3, col_names = FALSE)
d4 <- read_excel("Mock Dose Response Data.xlsx", sheet=4, col_names = FALSE)
d5 <- read_excel("Mock Dose Response Data.xlsx", sheet=5, col_names = FALSE)
d6 <- read_excel("Mock Dose Response Data.xlsx", sheet=5, col_names = FALSE)

d1 <- gather(d1, Sample, Drug, 1:12)
d2 <- gather(d2, Sample, Drug, 1:12)

d3 <- d3 %>% gather(Sample, Fluorescence, 1:12) %>% select(-Sample)
d4 <- d4 %>% gather(Sample, Fluorescence, 1:12) %>% select(-Sample)

d5 <- d5 %>% gather(Sample, Concentration, 1:12) %>% select(-Sample)
d6 <- d6 %>% gather(Sample, Concentration, 1:12) %>% select(-Sample)

d13 <- bind_cols(d1, d3, d5)
d24 <- bind_cols(d2, d4, d6)

# Remove NAs and arrange by drug
d24[is.na(d24)] <- 0
dfAll <- bind_rows(d13, d24) 
dfAll <- dfAll %>% select(-Sample) %>% arrange(Drug)

# Subset D1(baseline drug) data
dfD1 <- filter(dfAll, Drug == "D1")

# Subset vehicle data
dfveh <- filter(dfAll, Drug == "veh")
  
# Subset compound data only
newdf <- dfAll
newdf <- newdf %>% filter(!grepl("D1",Drug)) %>% filter(!grepl("veh",Drug))

# Statistics for mean and stdev
D1val <- as.numeric(unlist(dfD1[,2]))
meanD1 <- mean(D1val)
stdevD1 <- sd(D1val)

vehval <- as.numeric(unlist(dfveh[,2]))
meanveh <- mean(vehval)
stdevveh <- sd(vehval)

# Add variables
newdf1 <- mutate(newdf, Baseline = meanD1)
newdf2 <- mutate(newdf1, "DrugDivBase" = Fluorescence/Baseline)

# Plot dose response intensity changes for all compounds relative to D1(baseline)
ggplot(newdf2, aes(x=Concentration, y=DrugDivBase)) + 
  geom_point(shape=1) +
  facet_wrap(~ Drug) +
  scale_x_discrete(limits=c("0.1nM", "1nM", "10nM", "100nM", "1000nM")) +
  geom_hline(yintercept = 1, color = "black") + 
  geom_hline(yintercept = meanveh/meanD1, color = "blue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Dose Response to Small Molecules", x = "Concentration",
       y = "Drug/Baseline Intensity")

library(ezknitr)
ezspin(file = "Dose Response Plate Analysis Edit.R", out_dir = "reports", fig_dir = "myfigs")

