################################
# Analysis of TGIF sale with pricing at the low end
################################
rm(list=ls()) # clear all variables

######### Libs ##################
library(tidyverse)
library(writexl)


######## Data Import #################

clx <- read.csv("../../data/ticket/fy20_all.csv", skip=3) %>%
  mutate(perf_dt = mdy(str_extract(perf_dt, ".*/.*/....")),
         dow = weekdays(perf_dt),
         dow = factor(dow, levels = c("Thursday", "Friday", "Saturday"))) %>% 
  filter(str_detect(season_desc, "Classics"),
         perf_dt < mdy("11/17/2019"),
         perf_dt > mdy("11/13/2019"),
         !price_type_group %in% c("Subscription", "Flex", "Comp", ""),
         !is.na(price_type_group))

clx.dow <- clx %>%
  group_by(dow) %>%
  summarise(revenue = sum(paid_amt),
            tix_sold = length(paid_amt),
            avg_tix_price = revenue / tix_sold)

write_xlsx(list(summary = clx.dow),
           path = 'TGIF_result.xlsx')
