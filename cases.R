library(tidyverse)

NSW <- read_csv("https://data.nsw.gov.au/data/dataset/3dc5dc39-40b4-4ee9-8ec6-2d862a916dcf/resource/4b03bc25-ab4b-46c0-bb3e-0c839c9915c5/download/confirmed_cases_table2_age_group_agg.csv")


NSW2 <- NSW %>%
  filter(notification_date > '2022-01-19')


NSWP <- NSW2 %>%
  filter(confirmed_by_pcr == 'Yes') %>%
  filter(age_group != 'AgeGroup_None')

NSWR <- NSW2 %>%
  filter(confirmed_by_pcr == 'No') %>%
  filter(age_group != 'AgeGroup_None')

theme_update(plot.title = element_text(hjust = 0.5))
ggplot() +
  ggtitle('NSW Confirmed cases PCR vs RAT') +
  xlab('Date') + ylab('Number of Cases') +
  geom_smooth(data=NSWP, aes(x = notification_date, y = confirmed_cases_count,
                 colour=age_group)) +
  geom_smooth(data=NSWR, aes(x = notification_date, y = confirmed_cases_count,
            colour= "Rapid Test"))+
  ylim(0,4000)+
  facet_wrap(~age_group, nrow = 3)


VIC <- read_csv("https://covidbaseau.com/historical/VIC%20Cases%20By%20Age%20Group.csv")
