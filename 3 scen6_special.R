# Calculate impacts separately for vending, since scenario 6 involves only changing beverages served via vending

library(tidyverse)

s_all <- read_csv("./data/vol_dbsc_scen0_scen6.csv")

# Impacts -----------------------------------------------------------------

imp_cont <- read_csv("./data/cont_imp_l.csv")
#View(imp_cont)
imp_binb <- read_csv("./data/cont_imp_l_binb.csv")
#View(imp_binb)
imp_bev <- read_csv("./data/bev_imp_l.csv") %>%
  mutate(plastic = 0) %>%
  na.omit() #Water SSB are NA, so omit those
#View(imp_bev)


xbinb_temp <- s_all %>%
  filter(cont_type != "bag_in_box") %>%
  mutate(item = "cont") %>% #Getting impacts for the container 
  left_join(imp_cont) %>%
  mutate(across(ghg:plastic, ~.x * liter_drink))
View(xbinb_temp)

binb_temp <- s_all %>%
  filter(cont_type == "bag_in_box") %>%
  mutate(item = "cont") %>% #Getting impacts for the container 
  left_join(imp_binb) %>%
  mutate(across(ghg:plastic, ~.x * liter_drink))
View(binb_temp)

cont <- bind_rows(xbinb_temp,binb_temp)
#%>%  arrange(scen, distrib, bev_type, SSB_status, cont_type)
View(cont)

bev <- s_all %>%
  mutate(item = "bev") %>% #Getting impacts for the beverage
  left_join(imp_bev) %>%
  mutate(across(ghg:plastic, ~.x * liter_drink))
#View(bev)

df_imp <- bind_rows(bev, cont) %>%
  select(!c(liter_drink, liter_cont))
#View(df_imp)
any(is.na(df_imp))

imp_di <- df_imp %>%
  group_by(scen, distrib, item) %>%
  summarize(across(c(ghg:plastic), ~ sum(.x))) %>%
  mutate(across(ghg:h2o, ~ .x/10^6)) %>%
  mutate(plastic = plastic/1000) %>%
  ungroup() %>%
  write_csv("./tbls_s6/imp_di.csv")
#View(imp_di)

imp_i <- df_imp %>%
  group_by(scen, item) %>%
  summarize(across(ghg:plastic, ~ sum(.x))) %>%
  mutate(across(ghg:h2o, ~ .x/10^6)) %>%
  mutate(plastic = plastic/1000) %>%
  ungroup() %>%
  write_csv("./tbls_s6/imp_i.csv")
#View(imp_i)

imp_d <- df_imp %>%
  group_by(scen, distrib) %>%
  summarize(across(ghg:plastic, ~ sum(.x))) %>%
  mutate(across(ghg:h2o, ~ .x/10^6)) %>%
  mutate(plastic = plastic/1000) %>%
  ungroup() %>%
  write_csv("./tbls_s6/imp_d.csv")
#View(imp_d)

imp <- df_imp %>%
  group_by(scen) %>%
  summarize(across(ghg:plastic, ~ sum(.x))) %>%
  mutate(across(ghg:h2o, ~ .x/10^6)) %>%
  mutate(plastic = plastic/1000) %>%
  ungroup() %>%
  write_csv("./tbls_s6/imp.csv")
#view(imp)

imp_s <- df_imp %>%
  group_by(scen, SSB_status) %>%
  summarize(across(ghg:plastic, ~ sum(.x))) %>%
  mutate(across(ghg:h2o, ~ .x/10^6)) %>%
  mutate(plastic = plastic/1000) %>%
  ungroup() %>%
  write_csv("./tbls_s6/imp_s.csv")
#View(imp_s)

imp_ds <- df_imp %>%
  group_by(scen, distrib, SSB_status) %>%
  summarize(across(ghg:plastic, ~ sum(.x))) %>%
  mutate(across(ghg:h2o, ~ .x/10^6)) %>%
  mutate(plastic = plastic/1000) %>%
  ungroup() %>%
  write_csv("./tbls_s6/imp_ds.csv")
#View(imp_ds)