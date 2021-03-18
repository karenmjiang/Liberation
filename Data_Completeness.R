# Load libraries
library(googlesheets4)
library(tidyverse)
library(formattable)
library(gt)

setwd(dir = "Documents/Liberation/")
# From State FOIA Spreadsheet
race_eth <- read_sheet("1jT5_Ws2SNKK62-GIwJHKeR5MeH1Ga9ITRRvAR1BxrSg", 
                       sheet = "ProgramData-Race_eth")

# Codebook Link
# https://docs.google.com/spreadsheets/d/1sOHwwcIrBAP_qL3vLJegKBqwS4D3QObs/edit#gid=1542148160


missing_indicators <- race_eth %>% 
  filter(incarcerated == 1) %>%
  mutate(popsize_both = ifelse(!is.na(popsize3.1.20) & !is.na(popsize10.1.20), T,F)) %>%
  group_by(State) %>%
  mutate(n_race_test = sum(!is.na(tested)),
         n_race_positive = sum(!is.na(positive)),
         n_race_hospital = sum(!is.na(hospital)),
         n_race_death = sum(!is.na(death)),
         n_race_pop3 = sum(!is.na(popsize3.1.20)),
         n_race_pop10 = sum(!is.na(popsize10.1.20)),
         n_race_dates = sum(popsize_both))

state_indicators <- missing_indicators %>%
  select(State, starts_with("n_race")) %>%
  unique()
  
colnames(state_indicators) <- c("State", 
                                "Tested", 
                                "Positive", 
                                "Death", 
                                "Hospitalizations", 
                                "Population on March 1, 2020", 
                                "Population on October 1, 2020", 
                                "Population Reported for Both Times")

completeness <- state_indicators %>%
  pivot_longer(names_to = "Data Categories",
               values_to = "Number of Race / Ethnic Groups Reported", -State) %>%
  ggplot() +
  geom_col(aes(y = State,
               x = `Number of Race / Ethnic Groups Reported`,
               fill = `Number of Race / Ethnic Groups Reported`),
          show.legend = F) + 
  scale_x_continuous(n.breaks = 4) +
  scale_fill_distiller(palette = "Spectral", direction = 1) +
  facet_grid(~`Data Categories`, labeller = label_wrap_gen(width = 10, multi_line = TRUE)) +
  labs(title = "Number of Race / Ethnic Groups Reported",
       x = "", y = "") +
  theme_minimal()

ggsave(plot = completeness, filename = "outputs/completeness.png")
