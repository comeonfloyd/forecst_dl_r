# ����� ��� �������� DL

# ������� ������ � ���������� ������
period <- 11
forecast_period <- "?"

# ����
path <- 'C:/Users/Alexander.Persidskiy/Desktop/Taableau/Forecast/2022/'
path <- paste0(path, period)

setwd(path)

# ��������� ���������
library(readxl)
df_pnl <- read_excel("PNL.xlsx", skip = 1)
df_hours <- read_excel("Hours.xlsx")
voc <- read_excel("voc.xlsx")

# ������� ����������
library(tidyverse)
library(dplyr)

# ������� ��� ��������������
rename_hours <- function(x){
  x %>% 
    rename(., ����_�� = c(4))
  
}

# ������� ��� ���������� �����
add_key <- function(x){
  x %>%   
    select(names(.)) %>% 
    mutate(key = (paste(.$"��� ID", .$"��� ������������", .$"����� ��� ID")))
}

# ��������� �����
df_pnl <- add_key(df_pnl)
df_hours <- add_key(df_hours)

# ����� ������ � ������ ��
df_master <- left_join(df_pnl, df_hours[c("key", "����_��")], by = 'key')

# ��������� ����������
voc <- subset(voc, voc$������ != "������")

# ����� ������� �� ������������
df_master <- inner_join(df_master, voc[c('��� ID', '������')], by = "��� ID")

# �������� ��
df_master[is.na(df_master)] <- 0

# �������� ������
df_master <- df_master %>% select(names(.)) %>% 
  mutate(SHR = `������ �� �������� ����������` / `����_��`, 
         Premia = `������ ����������� � ������`  /  `����_��`)

# �����:
#   1. ������� ���� ��
#   2. ������� �������� ��
#   3. ������� ������� �������
#   4. �� �������� ������� ������� ������� �����
#   5. ������� ������
#   6. ???
#   7. ������