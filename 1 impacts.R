# Calculating impacts
# Use summaries saved as csv for input
# Actually, using the scenarios, which are kind of hand-built

library(tidyverse)


# Read data & clean up ---------------------------------------------------------------
cont_imp_l <- read_csv("./data/cont_imp_l.csv")
binb_imp_l <- read_csv("./data/cont_imp_l_binb.csv")   # per liter of beverage volume, as drunk
bev_imp_l <- read_csv("./data/bev_imp_l.csv")     # per liter of beverage volume, as drunk
flow <- read_csv("./data/flow_bsc_allscen.csv")   # "vol" is liter of beverage, as drunk
bev_name <- read_csv("./data/bevtype_list_name.csv")  # List of the beverage types in the dataset
#bev_SSB <- read_csv("./data/bevtype_SSB.csv")  # List of the beverage types in the dataset
#View(bev_name)
#View(bev_imp_l)
#View(cont_imp_l)
#View(flow)

# Beverage (bev) impacts: all scenarios ---------------------------------------------------

bev_imp_allscen <- flow %>%
  left_join(bev_imp_l) %>%
  mutate(across(ghg:h2o, ~.x * vol)) %>%
  select(!vol) %>%
  pivot_longer(
    cols = ghg:h2o,
    names_to = "impact_type",
    values_to = "value") %>%
  add_column(item = "bev", .before = "value")
#View(bev_imp_allscen)


# Container (cont) impacts: all scenarios --------------------------------------------------
# We have to do this in two parts - BinB, and everything else
# Because we have to use BinB cont impacts that are specific to the bev_type
# because of mixing ratios

## containers except BinB

xBinB <- flow %>%   # Not including BinB (needs custom treatment)
  filter(cont_type != "bag_in_box") %>%
  left_join(cont_imp_l) %>%
  mutate(across(ghg:plastic, ~.x * vol)) %>%
  select(!vol) %>%
  pivot_longer(
    cols = ghg:plastic,
    names_to = "impact_type",
    values_to = "value") %>%
  add_column(item = "cont", .before = "value")
#View(xBinB)

BinB <- flow %>%
  filter(cont_type == "bag_in_box") %>%
  left_join(binb_imp_l) %>%    #!!!! What if these don't have all matches?
  mutate(across(ghg:plastic, ~.x * vol)) %>%
  select(!vol) %>%
  pivot_longer(
    cols = ghg:plastic,
    names_to = "impact_type",
    values_to = "value") %>%
  add_column(item = "cont", .before = "value")
#View(BinB)

cont_imp_allscen <- bind_rows(xBinB, BinB) %>%
  arrange(scen, bev_type, cont_type)
#View(cont_imp_allscen)  


# Combined table with cont and bev impacts --------------------------------

imp_allscen <- bind_rows(bev_imp_allscen, cont_imp_allscen) %>%
  write_csv("./data_gen/imp_bsc_allscen.csv")
#View(imp_allscen)
