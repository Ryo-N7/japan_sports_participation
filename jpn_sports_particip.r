library(readr)
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)


### 
# 11.27.18
# REDO this section to grab data through e-stat API 
# use https://github.com/yutannihilation/estatapi  ???
# do everything through R!!!

jpn_sports <- read_xlsx("jpn_sports_participation.xlsx", skip = 14)


jpn_sports_df <- read_csv("jpn_sports_particip.csv")




glimpse(jpn_sports)

glimpse(jpn_sports_df)

# take out the columns in japanese (that are all gibberish anyways)
# take out the top 3 rows
jpn_sports_df <- jpn_sports_df %>% 
  select(-X1, -X2, -X3) %>% 
  slice(-c(1, 2, 3, 4))


# NOTE: all populations in thousands except for sample size!!!
colnames(jpn_sports_df) <- c("gender", "region", "densely_inhabited_district", "sample_size", 
                             "est_population", "total_all_sport", "baseball", "softball", 
                             "volleyball", "basketball", "soccer", "table_tennis", "tennis", 
                             "badminton", "golf", "judo", "kendo", "gateball", "bowling", 
                             "fishing", "swimming", "ski_snowboard", "hiking", "cycling", 
                             "jogging", "walking_light_exercise", "weight_training",
                             "Other")

colnames(jpn_sports_df)

# no NAs in data >>> NOICE
jpn_sports_df %>% is.na() %>% sum()


glimpse(jpn_sports_df)


class(jpn_sports_df)

jpn_sports_df %>% 
  mutate(est_population = est_population %>% 
           str_replace(",", "") %>% 
           as.numeric()) %>% 
  glimpse()

fix <- jpn_sports_df %>% 
  select(est_population:Other) %>% 
  mutate_all(funs(str_replace(., ",", ""))) %>% 
  mutate_all(funs(as.numeric)) %>% 
  mutate_all(funs(. * 1000))
# use mutate_at() instead>>>!

fix %>% is.na() %>% sum()
fix %>% slice(432) %>% select(judo, kendo, golf, soccer)

jpn_sports_df %>% 
  mutate_at(vars(est_population:Other), funs(str_replace(., ",", ""))) %>% 
  mutate_at(vars(est_population:Other), funs(as.numeric)) %>% 
  filter(region == "01_Hokkaido" & gender == "0_Both sexes" & densely_inhabited_district == "0_Total") %>% 
  glimpse()

# people can choose multiple sports... therefore total sum > sample size n



jpn_sports_df <- jpn_sports_df %>% 
  mutate_at(vars(est_population:Other), funs(str_replace(., ",", ""))) %>% 
  mutate_at(vars(est_population:Other), funs(as.numeric)) %>% 
  mutate_at(vars(est_population:Other), funs(. * 1000)) %>% glimpse()

library(forcats)  

jpn_sports_df <- jpn_sports_df %>% 
  mutate(gender = as_factor(gender),
         #region = as_factor(region),
         densely_inhabited_district = as_factor(densely_inhabited_district)) 


class(jpn_sports_df$gender)

jpn_sports_df %>% select(gender) %>% purrr::map(levels)

# recode factors
jpn_sports_df$gender <- jpn_sports_df$gender %>% 
  fct_recode("Total" = "0_Both sexes",
             "Male" = "1_Male", 
             "Female" = "2_Female") 

jpn_sports_df$gender

jpn_sports_df$region

jpn_sports_df %>% select(region) %>% purrr::map(levels)

# [1] "00_Japan"         "01_Hokkaido"      "02_Aomori-ken"    "03_Iwate-ken"    
# [5] "04_Miyagi-ken"    "05_Akita-ken"     "06_Yamagata-ken"  "07_Fukushima-ken"
# [9] "08_Ibaraki-ken"   "09_Tochigi-ken"   "10_Gumma-ken"     "11_Saitama-ken"  
# [13] "12_Chiba-ken"     "13_Tokyo-to"      "14_Kanagawa-ken"  "15_Niigata-ken"  
# [17] "16_Toyama-ken"    "17_Ishikawa-ken"  "18_Fukui-ken"     "19_Yamanashi-ken"
# [21] "20_Nagano-ken"    "21_Gifu-ken"      "22_Shizuoka-ken"  "23_Aichi-ken"    
# [25] "24_Mie-ken"       "25_Shiga-ken"     "26_Kyoto-fu"      "27_Osaka-fu"     
# [29] "28_Hyogo-ken"     "29_Nara-ken"      "30_Wakayama-ken"  "31_Tottori-ken"  
# [33] "32_Shimane-ken"   "33_Okayama-ken"   "34_Hiroshima-ken" "35_Yamaguchi-ken"
# [37] "36_Tokushima-ken" "37_Kagawa-ken"    "38_Ehime-ken"     "39_Kochi-ken"    
# [41] "40_Fukuoka-ken"   "41_Saga-ken"      "42_Nagasaki-ken"  "43_Kumamoto-ken" 
# [45] "44_Oita-ken"      "45_Miyazaki-ken"  "46_Kagoshima-ken" "47_Okinawa-ken" 

# use regex to parse out instead of manually relabelling????
# if_else()  -ken >>> replace ""  then IF -fu >>> replace ""  then IF -to >>> replace ""
# ELSE region (as is)

# regex for START two-digits _   ???
# "^[0-9]{2}\\_"
library(stringr)

jpn_sports_df %>% 
  select(region) %>% 
  str_extract_all("\\d{2}\\_")
  
jpn_sports_df %>% 
  select(region) %>% 
  str_extract_all("\\-ken")

jpn_sports_df %>% 
  select(region) %>% 
  str_extract_all("\\-fu")

jpn_sports_df %>% 
  select(region) %>% 
  str_extract_all("\\-to")

jpn_sports_df %>% 
  select(region) %>% 
  str_replace_all("\\d{2}\\_", "") %>% 
  str_replace_all("\\-ken", "") %>% 
  str_replace_all("\\-fu", "") %>% 
  str_replace_all("\\-to", "")


jpn_sports_df %>% 
  mutate(region = region %>% str_replace_all("\\d{2}\\_", "")) %>% 
  mutate(region = region %>% str_replace_all("\\-ken", "")) %>% 
  mutate(region = region %>% str_replace_all("\\-fu", "")) %>% 
  mutate(region = region %>% str_replace_all("\\-to", ""))

jpn_sports_df <- jpn_sports_df %>% 
  mutate(region = region %>% str_replace_all("\\d{2}\\_", ""),
         region = region %>% str_replace_all("\\-ken", ""),
         region = region %>% str_replace_all("\\-fu", ""),
         region = region %>% str_replace_all("\\-to", ""))

glimpse(jpn_sports_df)

# do same with DIDs

jpn_sports_df <- jpn_sports_df %>% 
  mutate(densely_inhabited_district = densely_inhabited_district %>% 
           str_replace("\\d\\_", ""),
         densely_inhabited_district = as_factor(densely_inhabited_district))

glimpse(jpn_sports_df)

# prop_baseball

jpn_sports_df %>% 
  mutate(prop_baseball = baseball/total_all_sport) %>% 
  filter(gender == "Male" & densely_inhabited_district == "Total") %>% 
  select(region, prop_baseball) %>% 
  arrange(desc(prop_baseball)) %>% 
  head(5)

# prop_soccer
jpn_sports_df %>% 
  mutate(prop_soccer = soccer/total_all_sport) %>% 
  select(region, gender, prop_soccer) %>% 
  arrange(desc(prop_soccer)) %>% 
  head(5)

jpn_sports_df %>% 
  mutate(prop_soccer = soccer/total_all_sport) %>% 
  select(region, gender, prop_soccer) %>% 
  filter(gender == "Female") %>% 
  arrange(desc(prop_soccer))


jpn_sports_total <- jpn_sports_df %>% 
  filter(region != "Japan" & gender == "Total" & densely_inhabited_district == "Total")

save(jpn_sports_total, file = "jpn_sports_total.Rdata")

library(ggplot2)

jpn_sports_total %>% 
  ggplot(aes(reorder(region, total_all_sport), total_all_sport)) +
  geom_col() +
  scale_y_continuous(breaks = scales::pretty_breaks(10)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30))


#### ---- TOTALS, Total, All Gender

jpn_sports_total %>% 
  mutate(asdf = total_all_sport/est_population) %>% 
  select(region, asdf) %>%  
  arrange(desc(asdf)) %>% 
  head(5)

jpn_sports_total %>% 
  mutate(asdf = total_all_sport/est_population) %>% 
  select(region, asdf) %>%  
  arrange(desc(asdf)) %>% 
  tail(5)

# - Soccer
jpn_sports_total %>% 
  mutate(asdf = soccer/est_population) %>% 
  select(region, asdf) %>%  
  arrange(desc(asdf)) %>% 
  head(10)

# Participation rates in 78-2 are rounded to nearest 10th percents...









