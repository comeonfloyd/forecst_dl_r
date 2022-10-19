# Скрип для прогноза DL

# Заводим период и прогнозный период
period <- 11
forecast_period <- "?"

# Путь
path <- 'C:/Users/Alexander.Persidskiy/Desktop/Taableau/Forecast/2022/'
path <- paste0(path, period)

setwd(path)

# Считываем исходники
library(readxl)
df_pnl <- read_excel("PNL.xlsx", skip = 1)
df_hours <- read_excel("Hours.xlsx")
voc <- read_excel("voc.xlsx")

# Заводим библиотеки
library(tidyverse)
library(dplyr)

# Функция для переименование
rename_hours <- function(x){
  x %>% 
    rename(., Часы_СП = c(4))
  
}

# Функция для добавления ключа
add_key <- function(x){
  x %>%   
    select(names(.)) %>% 
    mutate(key = (paste(.$"ЦФО ID", .$"ЦФО Наименование", .$"Месяц Год ID")))
}

# Добавляем ключи
df_pnl <- add_key(df_pnl)
df_hours <- add_key(df_hours)

# Джоин данных с часамы СП
df_master <- left_join(df_pnl, df_hours[c("key", "Часы_СП")], by = 'key')

# Фильтруем справочник
voc <- subset(voc, voc$Статус != "Закрыт")

# Джоин мастера со справочником
df_master <- inner_join(df_master, voc[c('ЦФО ID', 'Статус')], by = "ЦФО ID")

# Зануляем НА
df_master[is.na(df_master)] <- 0

# Проводим расчёт
df_master <- df_master %>% select(names(.)) %>% 
  mutate(SHR = `Оплата по штатному расписанию` / `Часы_СП`, 
         Premia = `Премии ежемесячные и прочие`  /  `Часы_СП`)

# Далее:
#   1. Заводим часы АК
#   2. Считаем удельник АК
#   3. Заводим прогноз коробов
#   4. От прогноза коробов считаем прогноз часов
#   5. Считаем статьи
#   6. ???
#   7. Профит
