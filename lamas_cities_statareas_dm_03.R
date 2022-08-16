Sys.setlocale("LC_ALL", "Hebrew") # This function is helpful for Hebrew compatibility


library(tidyverse)



## Level: places / Yeshuvim ## 

#load data
yeshuvim_cbs_2020_raw <- read_csv("data/lamas/yeshuvim_cbs_2020.csv")
yeshuvim_piba_raw <- read_csv("data/lamas/yeshuvim_piba.csv")



yeshuvim_cbs_2020_proc <- yeshuvim_cbs_2020_raw %>% 
  select(place_name = 1,
         place_name_english = 3,
         place_symbol = 2,
         place_type_raw = 7,
         district_symbol = 4, 
         nafa_symbol = 5,
         cluster_symbol = 23,
         population_total_2020_yeshuv = 10,
         population_arab_2020_yeshuv = 13,
         religion_place = 9) %>% 
  mutate(place_symbol = place_symbol %>% stringr::str_pad(4, pad = "0")) %>% 
  
  mutate(district_name = district_symbol %>% car::Recode("1='ירושלים';
                                                 2='צפון';
                                                 3='חיפה';
                                                 4='מרכז';
                                                 5='תל אביב';
                                                 6='דרום';
                                                 7='יהודה ושומרון'")) %>% 
  mutate(nafa_name = nafa_symbol %>% car::Recode("
11='ירושלים';
21='צפת';
22='כנרת';
23='יזרעאל (עפולה)';
24='עכו';
25='יזרעאל (נצרת)';
29='גולן';
31='חיפה';
32='חדרה';
41='השרון';
42='פתח תקווה';
43='רמלה';
44='רחובות';
51='תל אביב (תל אביב)';
52='תל אביב (רמת גן)';
53='תל אביב (חולון)';
61='אשקלון';
62='באר שבע';
71='גנין';
72='שכם';
73='טול כרם';
74='ראמאללה';
75='ירדן (יריחו)';
76='בית לחם';
77='חברון'
")) %>% 
  mutate(place_type = case_when(
    place_type_raw ==0 ~ "city",
    place_type_raw ==99 ~ "local council",
    TRUE ~ "small settlement")) %>% 
  mutate(cluster_name = cluster_symbol %>% car::Recode("
  210='אשכול רשויות בית הכרם הגלילי';
211='אשכול רשויות גליל מזרחי';
212='אשכול רשויות גליל מערבי';
213='אשכול רשויות הגליל והעמקים';
214='אשכול רשויות הכנרת והעמקים';
310='אשכול רשויות המפרץ';
410='אשכול רשויות השרון';
420='אשכול רשויות שורק דרומי';
610='אשכול רשויות נגב מזרחי';
620='אשכול רשויות נגב מערבי';
710='אשכול רשויות יהודה ושומרון'
")) %>% 
  mutate(religion_place_lab = religion_place %>% car::Recode("
           1='יהודי';
           2='לא יהודי';
           3='שבט בדווי';
           4='ישוב מעורב'
 ")) %>% 
  mutate(population_total_2020_yeshuv = population_total_2020_yeshuv %>% replace_na(0),
         population_arab_2020_yeshuv = population_arab_2020_yeshuv %>% replace_na(0))




yeshuvim_piba_proc <- yeshuvim_piba_raw %>% 
  select(place_symbol = 1,
         place_name = 2,
         place_name_english = 3,
         municipality_symbol = 8,
         municipality_name = 9,
         nafa_symbol = 4,
         nafa_name = 5) %>% 
  mutate(place_symbol = place_symbol %>% stringr::str_pad(4, pad = "0"),
         municipality_symbol = municipality_symbol %>% stringr::str_pad(4, pad = "0"))


yeshuvim_piba_regional_councils <- yeshuvim_piba_proc %>%
  drop_na(municipality_name) %>% 
  select(place_symbol,
         municipality_symbol,
         municipality_name)


  
yeshuvim <- yeshuvim_cbs_2020_proc %>% 
  filter(population_total_2020_yeshuv>0) %>% 
  left_join(yeshuvim_piba_regional_councils,by = c("place_symbol")) %>% 
  select(place_name,
         place_name_english,
         place_symbol,
         place_type,
         municipality_name,
         municipality_symbol,
         district_name,
         district_symbol,
         nafa_name,
         nafa_symbol,
         cluster_name,
         cluster_symbol,
         population_total_2020_yeshuv,
         population_arab_2020_yeshuv) %>% 
  mutate(municipality_name = case_when(
    place_type=="small settlement" ~ str_c("regional council ",municipality_name),
    place_type=="local council" ~ str_c("local council ",place_name),
    place_type=="city" ~ str_c("city municipality ",place_name)
  ))%>% 
  mutate(municipality_symbol = case_when(
    place_type=="small settlement" ~ str_c("regional_council_",municipality_symbol),
    place_type!="small settlement" ~ place_symbol
  ))




## Level: regional councils

regional_councils <- yeshuvim %>% 
  filter(place_type=="small settlement") %>% 
  group_by(district_name,
           district_symbol,
           nafa_name,
           nafa_symbol,
           municipality_name,
           municipality_symbol) %>% 
  summarise(population_total_council_2020 = sum(population_total_2020_yeshuv),
            n_yeshuvim = n())


## Level: statareas

#load data

population_statareas_04_raw <- read_csv("data/lamas/population_statareas_04.csv")
population_statareas_06_raw <- read_csv("data/lamas/population_statareas_06.csv")
population_statareas_07_arab_raw <- read_csv("data/lamas/population_statareas_07_arab.csv")
population_statareas_07_jewish_raw <- read_csv("data/lamas/population_statareas_07_jewish.csv")
population_statareas_07_mixed_raw <- read_csv("data/lamas/population_statareas_07_mixed.csv")
population_statareas_08_01_raw <- read_csv("data/lamas/population_statareas_08_01.csv")
population_statareas_08_02_raw <- read_csv("data/lamas/population_statareas_08_02.csv")
population_statareas_09_raw <- read_csv("data/lamas/population_statareas_09.csv")
population_statareas_12_yeshuvim_raw <- read_csv("data/lamas/population_statareas_12_yeshuvim.csv")
population_statareas_12_statareas_raw <- read_csv("data/lamas/population_statareas_12_statareas.csv")


population_statareas_09 <- population_statareas_09_raw %>% 
  select(place_name = 3,
         place_symbol = 2,
         statarea = 4,
         population_total_2020 = 5) %>% 
  mutate(population_total_2020 = population_total_2020 %>% str_remove(c(",")) %>% as.numeric() %>% replace_na(0)) %>% 
  mutate(place_symbol = place_symbol %>% stringr::str_pad(4, pad = "0"),
         statarea = statarea %>% stringr::str_pad(4, pad = "0")) %>% 
  mutate(subdivision = statarea %>% stringr::str_sub(-4,-3)) %>% 
  mutate(symbol_statarea = str_c(place_symbol,statarea),
         symbol_subdivision = str_c(place_symbol,subdivision))
  
t1 <- population_statareas_09 %>% 
  group_by(place_symbol) %>% 
  summarise(n_statareas_place = n())

yeshuvim <- yeshuvim %>% 
  left_join(t1,by="place_symbol")

t1 <- regional_councils %>% ungroup() %>% 
  select(municipality_symbol,
         population_total_council_2020,
         n_yeshuvim)

yeshuvim <- yeshuvim %>% 
  left_join(t1,by="municipality_symbol")


population_statareas_07_mixed <- population_statareas_07_mixed_raw %>% 
  select(place_name = 2,
         place_symbol = 1,
         statarea = 3,
         sector = 4,
         gender = 5,
         population_sector_2020 = 6) %>% 
  filter(gender=='סה"כ') %>% 
  mutate(sector = case_when(
    sector == 'סה"כ' ~ "total",
    sector == 'ערבים' ~ "arabs",
    sector == 'יהודים' ~ "jewish",
    sector == 'יהודים ואחרים' ~ "jewish_and_others"
  )) %>% 
  mutate(population_sector_2020 = population_sector_2020 %>% str_remove(c(",")) %>% as.numeric() %>% replace_na(0)) %>% 
  mutate(place_symbol = place_symbol %>% stringr::str_pad(4, pad = "0"),
         statarea = statarea %>% stringr::str_pad(4, pad = "0")) %>% 
  pivot_wider(values_from = population_sector_2020,
              names_from = sector) %>% 
  data.frame() %>% 
  mutate(across(c(arabs,total,jewish,jewish_and_others),~replace_na(.,0))) %>% 
  mutate(percent_arab_2020 = (arabs/total) %>% round(2),
         symbol_statarea = str_c(place_symbol,statarea))

population_statareas_07_arab <- population_statareas_07_arab_raw %>% 
  select(place_name = 2,
         place_symbol = 1,
         statarea = 3,
         gender = 4,
         population_sector_2020 = 5) %>% 
  filter(gender=='סה"כ') %>% 
  mutate(sector = "arabs") %>% 
  mutate(population_sector_2020 = population_sector_2020 %>% str_remove(c(",")) %>% as.numeric() %>% replace_na(0)) %>% 
  mutate(place_symbol = place_symbol %>% stringr::str_pad(4, pad = "0"),
         statarea = statarea %>% stringr::str_pad(4, pad = "0") %>% replace_na("0001")) %>% 
  pivot_wider(values_from = population_sector_2020,
              names_from = sector) %>% 
  data.frame() %>% 
  mutate(percent_arab_2020 = (arabs/arabs) %>% round(2),
                  symbol_statarea = str_c(place_symbol,statarea))


t1 <- population_statareas_07_mixed %>% 
  select(symbol_statarea,arabs,percent_arab_2020) %>% bind_rows(
    population_statareas_07_arab %>% 
      select(symbol_statarea,arabs,percent_arab_2020)
  )

statareas <- population_statareas_09 %>% 
  select(place_name,
         place_symbol,
         statarea,
         symbol_statarea,
         population_total_2020,
         subdivision,
         symbol_subdivision) %>% 
  left_join(yeshuvim,by="place_symbol") %>% 
  left_join(t1,by="symbol_statarea") %>%
  mutate(across(c(arabs,percent_arab_2020),~replace_na(.,0)))



statareas_urban <- statareas %>% 
  filter(place_type!="small settlement")

statareas_nonurban <- statareas %>% 
  filter(place_type=="small settlement")

subdivisions_urban <- statareas_urban %>% group_by(municipality_name,
                                       municipality_symbol,
                                       symbol_subdivision,
                                       place_type) %>% 
  summarise(n_statareas =n(),
            population_total_2020 = sum(population_total_2020),
            population_arab_2020 = sum(arabs)) %>% 
  ungroup()

subdivisions_nonurban <- statareas_nonurban %>% group_by(municipality_name,
                                                   municipality_symbol,
                                                   place_type) %>% 
summarise(n_statareas = n(),
          population_total_2020 = sum(population_total_2020),
          population_arab_2020 = sum(arabs)) %>% 
  mutate(symbol_subdivision=municipality_symbol)%>% 
  ungroup()


subdivisions <- subdivisions_urban %>% 
  full_join(subdivisions_nonurban) %>% 
  mutate(percent_arab_2020 = (population_arab_2020/population_total_2020) %>% round(2))



subdivisions %>% 
  ggplot(aes(x=place_type,y=population_total_2020))+
  geom_boxplot()

subdivisions %>% 
  ggplot(aes(x=percent_arab_2020))+
  geom_histogram()


