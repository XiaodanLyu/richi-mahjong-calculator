library(dplyr)
library(tidyr)
raw <- readxl::read_excel("rule.xlsx", sheet = 1)
data <- raw %>% 
  tidyr::pivot_longer(cols = 3:13, names_to = "fu", values_to = "point") %>% 
  mutate(is_zhuang = grepl("^zhuang", type),
         is_mo = grepl("mo", type),
         payee = case_when(
           type == "zhuang-mo" ~ "闲家",
           type == "xian-mo-zhuang" ~ "庄家",
           type == "xian-mo-xian" ~ "闲家",
           .default = "输家"
         ),
         weight = case_when(
           type == "zhuang-mo" ~ 3,
           type == "xian-mo-xian" ~ 2,
           .default = 1
         )) %>% 
  group_by(is_zhuang, is_mo, fan, fu) %>% 
  mutate(total = sum(point*weight)) %>% 
  ungroup()

# preview_mobile("/Users/lyua/Documents/richi-mahjong-calculator/app.R", device = "iphoneX")
