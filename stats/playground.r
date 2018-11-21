## Default libs
library(tidyverse)
library(stringr)
## Extra Themes
library(ggthemes)
library(ggsci)
## A dataset
library(nycflights13)

ggplot(data = mpg) +
    geom_jitter(mapping = aes(x = displ, y = hwy, color = class)) +
    theme_fivethirtyeight() + scale_color_jama()

delays <- flights %>%
    group_by(dest) %>%
    summarise(
        count = n(),
        dist = mean(distance, na.rm = TRUE),
        delay = mean(arr_delay, na.rm = TRUE)) %>%
    filter(count > 20, dest != "HNL")

ggplot(data = delays, mapping = aes(x = dist, y = delay)) +
    geom_point(aes(size = count), alpha = 1/3) +
    geom_smooth(se = FALSE) +
    theme_tufte() + scale_color_jama()

stocks <- tibble(
    year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
    qtr    = c(   1,    2,    3,    4,    2,    3,    4),
    return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)

stocks %>%
    complete(year, qtr)

who %>% gather(code, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>%
    mutate(code = stringr::str_replace(code, "newrel", "new_rel")) %>%
    separate(code, c("new", "var", "sexage")) %>%
    select(-new, -iso2, -iso3) %>%
    separate(sexage, c("sex", "age"), sep = 1)
