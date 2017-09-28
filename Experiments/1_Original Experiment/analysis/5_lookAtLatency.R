library(tidyverse)

fix_dat <- read_csv("fixations.csv")
trl_dat <- read_csv("responses.csv")


dat <-full_join(fix_dat, trl_dat)

# just take distracter trials
dat <- filter(dat, distracter == 1)