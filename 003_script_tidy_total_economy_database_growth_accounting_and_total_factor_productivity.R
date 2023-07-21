library(tidyverse)

names <- read_csv(file = '000_data/003_total_economy_database_growth_accounting_and_total_factor_productivity.csv',
                  col_names = FALSE, 
                  skip=5, n_max=1, 
                  locale = locale(decimal_mark = '.', 
                                  grouping_mark = ',', 
                                  encoding = 'UTF-8')) %>% 
  select(-X1) %>% 
  .[1, ] %>% 
  unlist() %>% 
  unname() %>% 
  c('date', .)

unit_variable_label <- read_csv(file = '000_data/003_total_economy_database_growth_accounting_and_total_factor_productivity.csv',
                                col_names = FALSE, 
                                skip=1, n_max=1, 
                                locale = locale(decimal_mark = '.', 
                                                grouping_mark = ',', 
                                                encoding = 'UTF-8')) %>% 
  select(-X1) %>% 
  .[1, ] %>% 
  unlist() %>% 
  unname()

value_unit_description <- read_csv(file = '000_data/003_total_economy_database_growth_accounting_and_total_factor_productivity.csv',
                                   col_names = FALSE, 
                                   skip=2, n_max=1, 
                                   locale = locale(decimal_mark = '.', 
                                                   grouping_mark = ',', 
                                                   encoding = 'UTF-8')) %>% 
  select(-X1) %>% 
  .[1, ] %>% 
  unlist() %>% 
  unname() 

unit_variable_value_unit_description_label_tbl <- tibble(data_base_unit_variable_code = names[-1],
                                                         unit_variable_label = unit_variable_label,
                                                         value_unit_description = value_unit_description) %>% 
  separate_wider_regex(cols = data_base_unit_variable_code,
                       patterns = c(data_base_code = 'TED2',
                                    '_',
                                    unit_code = '.{3,4}',
                                    '_',
                                    variable_code = '.+')) %>% 
  separate_wider_regex(cols = unit_variable_label,
                       patterns = c(unit_label = '.+',
                                    ': ',
                                    variable_label = '.+'))

data_clean <- read_csv(file = '000_data/003_total_economy_database_growth_accounting_and_total_factor_productivity.csv', 
                          col_names = names, 
                          na = c("", "NA"), 
                          locale = locale(decimal_mark = '.', 
                                          grouping_mark = ',', 
                                          encoding = 'UTF-8'),
                          skip = 7) %>% 
  pivot_longer(cols = !date, 
               names_to = 'variable',
               values_to = 'value') %>% 
  separate_wider_regex(cols = variable,
                       patterns = c(data_base_code = 'TED2',
                                    '_',
                                    unit_code = '.{3,4}',
                                    '_',
                                    variable_code = '.+')) %>% 
  mutate(data_base_label = 'Total Economy Database - Growth Accounting and Total Factor Productivity',
         periodicity = "Annual") %>% 
  left_join(y = unit_variable_value_unit_description_label_tbl, 
            by = c('data_base_code', 'unit_code', 'variable_code')) %>% 
  select(date, 
         unit_code, unit_label, 
         variable_code, variable_label, 
         value, value_unit_description,
         periodicity,
         data_base_code, data_base_label)

data_clean %>% 
  write_csv(file = '000_data/003_tidy_TED2.csv')