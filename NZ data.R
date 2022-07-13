library(tidyverse)
library(plotly)
library(skimr)
library(janitor)
library(plotly)
library(officer)
library(viridis)
library(flextable)

`%notin%` <- Negate(`%in%`)


## Gather latest case data from NZ
NZ <- read_csv('https://raw.githubusercontent.com/minhealthnz/nz-covid-data/main/cases/covid-case-counts.csv')

## Clean Names
NZ <- clean_names(NZ)
`%notin%` <- Negate(`%in%`)

## filter data for 2022 only
NZ2 <- NZ %>%
  select(-c(case_status, overseas_travel)) %>%
  filter(report_date >= '2022-02-01') %>%
  filter(age_group != '70+') %>%
  filter(age_group %in% c('40 to 49', '50 to 59', '60 to 69','70 to 79', '80 to 89', '90+')) %>%
  group_by(report_date, age_group) %>%
  summarise(cases = sum(number_of_cases_reported))

## plot the data for 2022 by age groups

theme_update(plot.title = element_text(hjust = 0.5))
ggplot() +
  ggtitle('NZ confirmed cases') +
  xlab('Date') + ylab('Number of Cases') +
  geom_smooth(data=NZ2, aes(x = report_date, y = cases), method='gam') +
  geom_point(data=NZ2, aes(x = report_date, y = cases, colour=age_group), alpha=1/2)+
  facet_wrap(~age_group, nrow = 3, scale = 'free_y')


## looking at re-infection data

NZ3 <- NZ %>%
  filter(report_date >= '2022-06-20') %>%
  select(-c(case_status, overseas_travel, district)) %>%
  group_by(report_date, infection_status) %>%
  summarise(cases = sum(number_of_cases_reported))

  ggplot(NZ3, aes(x=report_date, y=cases, fill=infection_status)) +
  geom_bar(stat='identity', position=position_dodge()) +
    scale_color_viridis(discrete = TRUE, option = "a")+
    scale_fill_viridis(discrete = TRUE, alpha=1/2) +
  labs(x='Date',
       y='Confirmed Cases',
       title='NZ Daily Cases by Infection Type')+
    geom_text(aes(label=cases), color='black', size=4, vjust=-1, position = position_dodge(0.9))


## Looking at summrary data for re-infections



  ## looking at re-infection data

  NZ4 <- NZ %>%
    filter(report_date >= '2022-03-01') %>%
    filter(age_group %notin% c('40 to 49', '50 to 59', '60 to 69','70 to 79', '80 to 89', '90+')) %>%
    select(-c(case_status, overseas_travel, district)) %>%
    group_by(report_date, age_group, infection_status) %>%
    filter(infection_status=='Reinfection (< 90 days)') %>%
    summarise(cases = sum(number_of_cases_reported))

  NZ5 <- NZ %>%
    filter(report_date >= '2022-03-01') %>%
    filter(age_group %notin% c('40 to 49', '50 to 59', '60 to 69','70 to 79', '80 to 89', '90+')) %>%
    select(-c(case_status, overseas_travel, district)) %>%
    group_by(report_date, age_group, infection_status) %>%
    filter(infection_status=='Reinfection') %>%
    summarise(cases = sum(number_of_cases_reported))

  ggplot() +
  geom_point(data=NZ4, aes(x= report_date, y = cases, color= 'Reinfection < 90 days'), alpha=1/2) +
    geom_smooth(data=NZ4, aes(x= report_date, y = cases, method='gam', color= 'Reinfection < 90 days')) +
  geom_point(data=NZ5, aes(x = report_date, y = cases, color= 'Reinfection >90 days'), alpha=1/2) +
    geom_smooth(data=NZ5, aes(x = report_date, y = cases, method='gam', color= 'Reinfection >90 days')) +
  facet_wrap(~age_group, nrow = 3) +
    labs(x='Date',
         y='Confirmed Cases',
         title='NZ Daily Reinfections by age group')


##--------------------------------------------------------------------------

  ## try and facet by age_group
##
    NZ6 <- NZ %>%
    filter(report_date > '2022-06-28') %>%
    select(-c(case_status, overseas_travel, district)) %>%
    filter(age_group %notin% c('40 to 49', '50 to 59', '60 to 69','70 to 79', '80 to 89', '90+', '70+')) %>%
    group_by(report_date, age_group, infection_status) %>%
    summarise(cases = sum(number_of_cases_reported))

  ggplot(NZ6, aes(x=report_date, y=cases, fill=infection_status)) +
    geom_bar(stat='identity', position=position_dodge()) +
    geom_text(aes(label=cases), color='black', size=2.5, vjust=-1, position = position_dodge(0.9))+
    labs(x='Date',
         y='Confirmed Cases',
         title='NZ Daily Cases by Infection Type')+
    facet_wrap(~age_group, nrow = 3)

  ############################


NZ7 <- NZ %>%
    filter(report_date > '2022-06-28') %>%
    filter(age_group != '70+') %>%
    filter(infection_status != 'Historical') %>%
    group_by(age_group, infection_status) %>%
    summarise(cases = sum(number_of_cases_reported))



  table_wide <-
    NZ7 %>%
    pivot_wider(
      id_cols = age_group,
      names_from = infection_status,
      values_from = cases
    )

  table_wide <- as.data.frame(table_wide)

  my_table <- table_wide %>%
    adorn_totals(c("row", "col")) %>%
    adorn_percentages(denominator = 'row')%>%
    adorn_pct_formatting(digits=1) %>%
    adorn_ns(position = "front")   %>%
  flextable::flextable()

  my_table <-  my_table %>%
    add_header_row(
      top = TRUE,                # New header goes on top of existing header row
      values = c("Age Category",     # Header values for each column below
                 "",
                 "Infection Status",    # This will be the top-level header for this and two next columns
                 "",
                 "")) %>%
    fontsize(i = 1, size = 12, part = "header") %>%   # adjust font size of header
    bold(i = 1, bold = TRUE, part = "header") %>%
    bold(i = 11, bold = TRUE, part = "body")  %>%
    bg(part = "body", bg = "gray95") %>%
    flextable::autofit()
my_table
