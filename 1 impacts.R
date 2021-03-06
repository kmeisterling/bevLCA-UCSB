## Calculating impacts

## ¡¡ ISSUES
  # Make sure all b-in-b beverage combos are contained in the binb_impact_l file
  

library(tidyverse)


# Read data ---------------------------------------------------------------
flow <- read_csv("./data/flow_bsc_allscen.csv")   # column 'vol' = volume liter of beverage, as drank
View(flow)
# beverage flows are in liters
 # data contains volume as packaged and volume as drank

bev_impact_l <- read_csv("./data/bev_imp_l.csv")     # impact of beverage per liter of beverage, as drank
cont_impact_l <- read_csv("./data/cont_imp_l.csv")      # impact of container per liter of beverage, as packaged
binb_impact_l <- read_csv("./data/cont_imp_l_binb.csv")   # impact of bag-in-box container per liter of beverage, as drank
#Impacts are 
 # g CO2e
 # g plastic pollution
 # liter blue water
#View(bev_imp_l)
#View(cont_imp_l)
#View(binb_imp_l)

bev_name <- read_csv("./data/bevtype_list_name.csv")  # List of the beverage types in the dataset, with nicely formatted names
View(bev_name)

## CHECK THAT ALL NAMES ARE CONSISTENT:
  # names from 'flow' are in 'bev_name'
  # names from 'bev_imp_l' are in 'bev_name'

# Beverage (bev) impacts: all scenarios ---------------------------------------------------

## Should I test that all joining values are known? (no NA in these joins?)
  # Actually some NA in bev_impact_l are lost?
  # But those don't actually show up in the flow data (e.g. SSB tap water is "NA" in bev_impact_l, but is not in flow data)

test1 <- flow %>%
  left_join(bev_impact_l)
View(test1)

bev_imp_allscen <- flow %>%
  left_join(bev_impact_l) %>%
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

xBinB <- flow %>%   # Not including BinB (BinB needs container impact per volume DRANK, not per volume PACKAGED)
  filter(cont_type != "bag_in_box") %>%
  left_join(cont_impact_l) %>%
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
  left_join(binb_impact_l) %>%    #!!!! What if these don't have all matches?
  mutate(across(ghg:plastic, ~.x * vol)) %>%
  select(!vol) %>%
  pivot_longer(
    cols = ghg:plastic,
    names_to = "impact_type",
    values_to = "value") %>%
  add_column(item = "cont", .before = "value")
View(BinB)

cont_imp_allscen <- bind_rows(xBinB, BinB) %>%
  arrange(scen, bev_type, cont_type)
#View(cont_imp_allscen)  


# Combined table with cont and bev impacts --------------------------------

imp_allscen <- bind_rows(bev_imp_allscen, cont_imp_allscen) %>%
  write_csv("./data/data_gen/imp_bsc_allscen.csv")
#View(imp_allscen)

