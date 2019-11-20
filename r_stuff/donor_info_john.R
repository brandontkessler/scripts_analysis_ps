rm(list=ls()) # clear all variables

library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(writexl)


dnr <- read.csv('../../data/donor/donors_fy08-present.csv') 


########### REVENUES ###################################

rev <- dnr %>%
  mutate(cont_dt = mdy(str_extract(cont_dt, ".*/.*/....")),
         fy = str_extract(campaign, "(?<=-)..")) %>% 
  filter(!is.na(fy),
         fy >= 10,
         fy <= 20) %>% 
  group_by(fy, channel_desc) %>% 
  summarise(revenue = sum(gift_plus_pledge))


ggplot(rev, aes(fill=fy, y=revenue, x=channel_desc)) + 
  geom_bar(position="dodge", stat="identity", color="black") +
  ggtitle("Donor Revenue by Channel - fy10-Present") +
  scale_y_continuous(labels = comma)

rev.fin <- rev %>%
  spread(key = channel_desc, value=revenue)
  

  
########### DONOR COUNT ################################### 
  
dnrs <- dnr %>%
  mutate(cont_dt = mdy(str_extract(cont_dt, ".*/.*/....")),
         fy = str_extract(campaign, "(?<=-)..")) %>% 
  filter(!is.na(fy),
         fy >= 10,
         fy <= 20) %>% 
  group_by(fy, channel_desc) %>% 
  summarise(donors = n_distinct(summary_cust_id))


ggplot(dnrs, aes(fill=fy, y=donors, x=channel_desc)) + 
  geom_bar(position="dodge", stat="identity", color="black") +
  ggtitle("Number of Donors by Channel - fy10-Present") +
  scale_y_continuous(labels = comma)

dnrs.fin <- dnrs %>%
  spread(key = channel_desc, value=donors)



############ WRITE TO EXCEL ####################################

write_xlsx(list(revenue = rev.fin, donors = dnrs.fin),
           path = 'donor_channel_info_timeline.xlsx')