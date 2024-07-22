### Alcohol data

color_hunt <- c("#FEF3E2","#BEC6A0","#708871","#606676")
color_hunt2 <- c("#987D9A", "#BB9AB1", "#EECEB9", "#FEFBD8")
# library
library(tidyverse)
library(countrycode)
library(pwrench)
# read in data
al <- read_csv("Alcohol/total-alcohol-consumption-per-capita-litres-of-pure-alcohol.csv")

head(al)

# changing names

names(al)[4] <- "total_consumption"

# data cleaning

al_clean <- al |> 
  janitor::clean_names() |> 
  mutate(bottle_per_year = round((total_consumption /0.12) *1.34 , digits = 1))

# explore

al_clean |> 
  filter(year == 2019) |> 
  arrange(desc(total_consumption)) |> 
  ggplot(aes(total_consumption))+
  geom_histogram()+
  pwrench::theme_fa()

#visualization

al_clean |> 
  filter(year == 2019) |> 
  arrange(desc(total_consumption)) |> 
  ggplot(aes(total_consumption))+
  geom_histogram(color = color_hunt[4], fill = color_hunt[3], binwidth = 0.5)+
  expand_limits(x = c(0,18))+
  scale_x_continuous(breaks = seq(0,18))+
  labs(
    title ="پراکندگی مصرف الکل در کشورهای جهان" ,
    subtitle ="لیتر در سال" ,
    caption = "source: ourworld in data",
    x= "میزان مصرف بر حسب لیتر",
    y= "تعداد کشورها بر حسب لیتر"
  )+
  pwrench::theme_fa()

codelist |> 
  filter(region == "Middle East & North Africa") |> 
  select(cldr.name.fa,cldr.name.en, iso3c) |> 
  left_join(al_clean |>  filter(year == 2019), by = c(iso3c = "code")) |> 
  filter(!is.na(iso3c)) |> 
  filter(!cldr.name.en == "Malta") |>
  drop_na() |>
  mutate(cldr.name.fa = fct_reorder(cldr.name.fa, total_consumption)) |> 
  ggplot(aes(cldr.name.fa, total_consumption))+
  geom_col(aes(fill = total_consumption), show.legend = FALSE, color = color_hunt2[3])+
  #geom_col(fill= color_hunt2[2], color = color_hunt2[3])+
  scale_fill_gradient(low = color_hunt2[4], high = color_hunt2[1])+
  scale_y_continuous(breaks = seq(0,3, 1))+
  coord_flip()+
  labs(
    title = "میانگین مصرف الکل در خاورمیانه",
    subtitle = "2019",
    x = "",
    y = "لیتر در سال"
  )+
  theme_fa()

al_clean |> 
  filter(year == 2019,
         entity %in% c("Russia", "Iran", "Romania","Czechia"))
  ggplot(aes(year, total_consumption, color = code))+
  geom_line(show.legend = FALSE)
  
# most consumer iin the world
al_clean |> 
  filter(year == 2019) |> 
  arrange(desc(total_consumption)) |> 
  head(12) |> 
  inner_join(countrycode::codelist , by = c(code = "iso3c")) |> 
  mutate(cldr.name.fa = fct_reorder(cldr.name.fa, total_consumption)) |> 
  ggplot(aes(cldr.name.fa, total_consumption))+
  geom_col(aes(fill = total_consumption), show.legend = FALSE , color = color_hunt2[3])+
  coord_flip()+
  geom_text(aes(label = round(total_consumption, 1)), hjust = 1 , family = "Sahel FD", size= 6)+
  scale_fill_gradient(low = color_hunt2[4] , high = color_hunt2[1])+
  labs(
    title = "بیشترین مصرف‌کنندگان الکل در جهان",
    subtitle = "2019",
    x = "",
    y = "میزان مصرف الکل درسال",
    caption = "منبع: سازمان بهداشت جهانی"
  )+
  theme_fa()
