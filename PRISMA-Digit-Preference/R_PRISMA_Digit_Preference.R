# Drafted: 2023-09-08 (original code by Xiaoyan Hu)
# Function: this code will check for any digit preferece among the following variables 
    #  MUAC
    #  SpO2
    #  Maternal Respiratory Rate 
#  To run this code, you will need to:
#     1. first set up your local directories and folders 
#     2. set today's date

#*****************************************************************************
# Data Setup -- update the code below prior to running the script
#*****************************************************************************
 
## digit preference 
library(tidyverse)

## UPDATE BEFORE YOU RUN ##
## set upload date
UploadDate = "2023-09-08"

## UPDATE BEFORE YOU RUN ##
## set path to export (where do you want to store the plots generated by this code)
path_to_save <- paste0("~/Documents/Output")
path_to_save <- "D:/Users/stacie.loisate/Documents/Output"

## UPDATE BEFORE YOU RUN ##
##Load data
mnh06 <- read_csv("~/mnh06.csv")
mnh05 <- read_csv("~/mnh05.csv")


#function to extract last digit of integers
lastdigit <- function(x) {
  floor(x -floor(x/10)*10)
}

#*****************************************************************************
# Measurement: MUAC 
# Form: MNH05 
# varname: MUAC_PERES
#*****************************************************************************

plot_muac_digit <- mnh05 %>%
  select(MOMID, PREGID, MUAC_PERES) %>% ## only select variables we need 
  pivot_longer(-c("MOMID", "PREGID"),       ## convert the data to long format
               names_to = "varname",
               values_to = "measurement") %>%
  mutate(digit = round(measurement, 1) - floor(round(measurement, 1))) %>% ## pull the last digit of the measurement
  filter(!(measurement %in% c(-7, -5)))    ## remove any default values

## plot 
plot_muac_digit %>%
  ggplot(aes(x = digit)) +
  geom_histogram(binwidth = 0.1, fill = "darkseagreen4", color = "black", alpha = 0.6) + 
  labs(title = "MUAC Digit Preference", 
       x = "Digit",   
       y = "Frequency") + scale_x_continuous(breaks = seq(0, 1, by = 0.1), labels = seq(0, 1, by = 0.1)) +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=1),
        legend.position="bottom",
        panel.grid.minor = element_blank()) +
  theme(strip.background=element_rect(fill="white")) 

ggsave(paste0("Muac_digit_preference_", UploadDate, ".pdf"), path = path_to_save)

#*****************************************************************************
# Measurement: SpO2 
# Form: MNH06 
# varname: PULSEOX_VSORRES
#*****************************************************************************
plot_sp02_digit <- mnh06 %>%
  select(MOMID, PREGID, PULSEOX_VSORRES) %>% ## only select variables we need 
  pivot_longer(-c("MOMID", "PREGID"),       ## convert the data to long format
               names_to = "varname",
               values_to = "measurement") %>%
  mutate(digit = lastdigit(measurement)) %>% ## pull the last digit of the measurement
  filter(!(measurement %in% c(-7, -5)))    ## remove any default values

## plot 
plot_sp02_digit %>%
  ggplot(aes(x = digit)) +
  geom_histogram(binwidth = 1, fill = "darkseagreen4", color = "black", alpha = 0.8) + 
  labs(title = "SpO2 Digit Preference", 
       x = "Digit",   
       y = "Frequency") + scale_x_continuous(breaks = seq(0, 10, by = 1), labels = seq(0, 10, by = 1)) +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=1),
        legend.position="bottom",
        panel.grid.minor = element_blank()) +
  theme(strip.background=element_rect(fill="white")) 

ggsave(paste0("spo2_digit_preference_", UploadDate, ".pdf"), path = path_to_save)
#*****************************************************************************
# Measurement: Maternal respiratory rate  
# Form: MNH06 
# varname: RR_VSORRES
#*****************************************************************************
plot_mat_rr_digit <- mnh06 %>%
  select(MOMID, PREGID, RR_VSORRES) %>% ## only select variables we need 
  pivot_longer(-c("MOMID", "PREGID"),       ## convert the data to long format
               names_to = "varname",
               values_to = "measurement") %>%
  mutate(digit = lastdigit(measurement)) %>% ## pull the last digit of the measurement
  filter(!(measurement %in% c(-7, -5)))    ## remove any default values

## plot 
plot_mat_rr_digit %>%
  ggplot(aes(x = digit)) +
  geom_histogram(binwidth = 1, fill = "darkseagreen4", color = "black", alpha = 0.6) + 
  labs(title = "Maternal respiratory rate Digit Preference", 
       x = "Digit",   
       y = "Frequency") + scale_x_continuous(breaks = seq(0, 10, by = 1), labels = seq(0, 10, by = 1)) +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=1),
        legend.position="bottom",
        panel.grid.minor = element_blank()) +
  theme(strip.background=element_rect(fill="white")) 

ggsave(paste0("Mat_rr_digit_preference_", UploadDate, ".pdf"), path = path_to_save)

