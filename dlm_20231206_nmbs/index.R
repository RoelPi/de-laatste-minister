gc()
rm(list=ls())
options(scipen=999)

library(ragg)
library(data.table)
library(ggradar)
library(ggplot2)
library(fmsb)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(plotly)
library(zoo)

f_size <- 10

t <- theme(plot.title = element_text(face="bold"),
           axis.text.x = element_text(size=f_size,color='#000000',angle=0),
           axis.text.y = element_text(size=f_size,color='#000000'),
           axis.title.x = element_text(face="bold", size=f_size,color='#000000'),
           axis.title.y = element_text(face="bold", size=f_size,color='#000000'),
           panel.background = element_rect(fill="#f1f1f1", color='#a5a5a5',size=0.5),
           panel.ontop = F,
           panel.grid.major.y = element_line(color='#a5a5a5', linetype='blank',size=0),
           panel.grid.minor.y = element_blank(),
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           legend.text = element_text(size=f_size,color='#000000'),
           legend.title = element_text(face='bold',size=f_size,color='#000000'),
           legend.box.background = element_rect(fill='#ffffff', color='#ffffff', size=1.5),
           strip.text = element_text(size=f_size,color='#000000', face='bold'),
           strip.background = element_rect(colour = NA, fill = '#ffffff'))

pal <- c('#b3b3b3', '#b3b3b3', '#b3b3b3', '#b3b3b3', '#b3b3b3','#e5ac13','#43A39E','#e3e117','#3e21aa','#af1313','#ea360c','#a26b24','#333745','#f2f2f2','#515151','#000000')

df1 <- fread('dlm_20231206_nmbs/nationale-stiptheid-met-afgeschafte-treinen.csv')
colnames(df1) <- c('maand', 'n_treinen', 'n_aangekomen', 'pct_stipt', 'jaar')
df <- fread('dlm_20231206_nmbs/nationale-stiptheid-per-maand.csv')
colnames(df) <- c('jaar', 'maand', 'pct_stipt', 'n_treinen', 'n_vertr', 'n_min_vertr', 'pct_stipt_neutr')
df1 <- df1 %>% 
  mutate(jaar_maand = ym(maand)) %>% 
  mutate(pct_laat = 100 - pct_stipt) %>%
  select(!c(maand, jaar)) %>%
  mutate(maand = month(jaar_maand)) %>%
  mutate(jaar = as.character(year(jaar_maand))) %>%
  arrange(jaar_maand) %>%
  filter(!(jaar %in% c(2020, 2021)))

g <- ggplot(df1, aes(x = maand, y = pct_laat / 100, col = jaar, fill = jaar, linetype = jaar, shape = jaar)) + 
  geom_line(linewidth=0.5) +
  geom_point() +
  scale_color_manual(values = pal, name = "") +
  scale_fill_manual(values = pal, name = "") +
  scale_linetype_manual(values = c('dotted', 'dotdash', 'dashed', 'twodash', 'longdash', 'solid'), name = "") +
  scale_x_continuous(
    breaks = seq_along(month.abb), 
    labels = month.abb
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.2)) +
  labs(x = '', y = '% te laat', shape = "") +
  t
ggplotly(g)
ggsave(g, width = 6, height = 4, filename = "global.png")


df2 <- fread('dlm_20231206_nmbs/Data_raw_punctuality_202310.csv')

# https://infrabel.opendatasoft.com/explore/dataset/gegevens-mobipulse/export/?sort=dat_dep
df3 <- fread('dlm_20231206_nmbs/gegevens-mobipulse.csv')
colnames(df3) <- c('date', 'from', 'to', 'moment', 'moment2', 'moment3', 'punctuality', 'travel_time')
df3 %>%
  select(date, from, to, moment, punctuality, travel_time) %>% 
  filter(from == 'BRUSSEL-CENTRAAL') %>%
  filter(to == 'KORTRIJK') %>%
  filter(moment == 'Avondspits') %>%
  arrange(date) %>% 
  mutate(month = month(date)) %>%
  mutate(year = as.character(year(date))) %>%
  mutate(day = day(date)) %>%
  mutate(year_month = ym(paste(year, month, '-'))) %>%
  mutate(day_of_year = yday(date)) %>%
  mutate(rolling_punctuality = rollmean(punctuality, k = 28, align = 'right', fill = NA)) %>%
  filter(!is.na(rolling_punctuality)) %>%
  mutate(rolling_late = 100 - rolling_punctuality) %>%
  filter(!(year %in% c(2020, 2021))) -> df3

g <- ggplot(df3, aes(x = day_of_year, y = rolling_late / 100, color = year, linetype = year)) + 
  geom_line(linewidth=0.8) +
  scale_color_manual(values = pal[3:6], name = "") +
  scale_linetype_manual(values = c('dotted', 'dotdash', 'dashed', 'solid'), name = "") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = '', y = '% te laat', shape = "") +
  t
ggplotly(g)
ggsave(g, width = 6, height = 4, filename = "dlm_20231206_nmbs/kortrijk.png")

# https://infrabel.opendatasoft.com/explore/dataset/gegevens-mobipulse/export/?sort=dat_dep
df3 <- fread('dlm_20231206_nmbs/gegevens-mobipulse.csv')
colnames(df3) <- c('date', 'from', 'to', 'moment', 'moment2', 'moment3', 'punctuality', 'travel_time')
df3 %>%
  select(date, from, to, moment, punctuality, travel_time) %>% 
  filter(from == 'BRUSSEL-CENTRAAL') %>%
  filter(to == 'ANTWERPEN-CENTRAAL') %>%
  filter(moment == 'Avondspits') %>%
  arrange(date) %>% 
  mutate(month = month(date)) %>%
  mutate(year = as.character(year(date))) %>%
  mutate(day = day(date)) %>%
  mutate(year_month = ym(paste(year, month, '-'))) %>%
  mutate(day_of_year = yday(date)) %>%
  mutate(rolling_punctuality = rollmean(punctuality, k = 28, align = 'right', fill = NA)) %>%
  filter(!is.na(rolling_punctuality)) %>%
  mutate(rolling_late = 100 - rolling_punctuality) %>%
  filter(!(year %in% c(2020, 2021))) -> df3

g <- ggplot(df3, aes(x = day_of_year, y = rolling_late / 100, color = year, linetype = year)) + 
  geom_line(linewidth=0.8) +
  scale_color_manual(values = pal[3:6], name = "") +
  scale_linetype_manual(values = c('dotted', 'dotdash', 'dashed', 'solid'), name = "") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = '', y = '% te laat', shape = "") +
  t
ggplotly(g)
ggsave(g, width = 6, height = 4, filename = "dlm_20231206_nmbs/antwerpen.png")


# https://infrabel.opendatasoft.com/explore/dataset/gegevens-mobipulse/export/?sort=dat_dep
df3 <- fread('dlm_20231206_nmbs/gegevens-mobipulse.csv')
colnames(df3) <- c('date', 'from', 'to', 'moment', 'moment2', 'moment3', 'punctuality', 'travel_time')
df3 %>%
  select(date, from, to, moment, punctuality, travel_time) %>% 
  filter(from == 'BRUSSEL-CENTRAAL') %>%
  filter(to == 'HASSELT') %>%
  filter(moment == 'Avondspits') %>%
  arrange(date) %>% 
  mutate(month = month(date)) %>%
  mutate(year = as.character(year(date))) %>%
  mutate(day = day(date)) %>%
  mutate(year_month = ym(paste(year, month, '-'))) %>%
  mutate(day_of_year = yday(date)) %>%
  mutate(rolling_punctuality = rollmean(punctuality, k = 28, align = 'right', fill = NA)) %>%
  filter(!is.na(rolling_punctuality)) %>%
  mutate(rolling_late = 100 - rolling_punctuality) %>%
  filter(!(year %in% c(2020, 2021))) -> df3

g <- ggplot(df3, aes(x = day_of_year, y = rolling_late / 100, color = year, linetype = year)) + 
  geom_line(linewidth=0.8) +
  scale_color_manual(values = pal[3:6], name = "") +
  scale_linetype_manual(values = c('dotted', 'dotdash', 'dashed', 'solid'), name = "") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = '', y = '% te laat', shape = "") +
  t
ggplotly(g)
ggsave(g, width = 6, height = 4, filename = "dlm_20231206_nmbs/hasselt.png")



# https://infrabel.opendatasoft.com/explore/dataset/gegevens-mobipulse/export/?sort=dat_dep
df3 <- fread('dlm_20231206_nmbs/gegevens-mobipulse.csv')
colnames(df3) <- c('date', 'from', 'to', 'moment', 'moment2', 'moment3', 'punctuality', 'travel_time')
df3 %>%
  select(date, from, to, moment, punctuality, travel_time) %>% 
  filter(from == 'BRUSSEL-CENTRAAL') %>%
  filter(to == 'GENT-SINT-PIETER') %>%
  filter(moment == 'Avondspits') %>%
  arrange(date) %>% 
  mutate(month = month(date)) %>%
  mutate(year = as.character(year(date))) %>%
  mutate(day = day(date)) %>%
  mutate(year_month = ym(paste(year, month, '-'))) %>%
  mutate(day_of_year = yday(date)) %>%
  mutate(rolling_punctuality = rollmean(punctuality, k = 28, align = 'right', fill = NA)) %>%
  filter(!is.na(rolling_punctuality)) %>%
  mutate(rolling_late = 100 - rolling_punctuality) %>%
  filter(!(year %in% c(2020, 2021))) -> df3

g <- ggplot(df3, aes(x = day_of_year, y = rolling_late / 100, color = year, linetype = year)) + 
  geom_line(linewidth=0.8) +
  scale_color_manual(values = pal[3:6], name = "") +
  scale_linetype_manual(values = c('dotted', 'dotdash', 'dashed', 'solid'), name = "") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = '', y = '% te laat', shape = "") +
  t
ggplotly(g)
ggsave(g, width = 6, height = 4, filename = "dlm_20231206_nmbs/gent.png")
