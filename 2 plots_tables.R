# Summarize, plot, and make tables for results and documentation we want to show
  # Flow (in liter) of beverages by scenario, beverage type, container type, and SSB status
  # This is where we rescale as necessary, and give nicer names

library(tidyverse)
#library(ggtext)


# Definitions ------------------------------------------------------

# Colors --
col_vir1 <- c("beverage" = "#21908C", "container" = "#440154")
col_bev <- c("#21908C")
col_cont <- c("#440154")


# Names -------------------------------------------------------------------------

# Human-friendly impact names to be used in ggplot.labeller
  #(should probably do something similar with 'the 'bev_type', 'scen', etc, but not right now)

# Names -------------------------------------------------------------------

bevNames_temp <- read_csv("./data/bevtype_list_name.csv")
#View(bevNames_temp)
bevNames <- bevNames_temp %>%
  mutate(bev_regex = bev_name) %>%
  #  mutate(bev_regex = str_replace(bev_regex, "+ ", "+~")) %>%
  mutate(bev_regex = str_replace(bev_regex, " ", "~")) %>%
  mutate(bev_regex = str_replace(bev_regex, ",", "*','*"))
#View(bevNames)
remove(bevNames_temp)

contNames <- read_csv("./data/conttype_list_name.csv")
#View(contNames)

impNames <- read_csv("./data/imptype_list_name.csv")
#View(impNames)

scenNames <- read_csv("./data/scen_list_name.csv")
#View(scenNames)

# BIG impacts (tonnes, M liter, kg)
 # Formatted in markdown, to use element_markdown
# impnames_hand <- c(
#   ghg = "Climate<br> t CO2eq (100yr) ",
#   h2o = "Blue Water Use<br>[ M liter (Blue) water ]",
#   plastic = "Plastic Pollution<br>[ kg plastic ]"
#)
#View(impnames_hand)

# Impacts per liter
# impname_l <- c(
#   ghg = "Climate<br>[ g CO2eq / liter ]",
#   h2o = "Blue Water Use<br>[ liter (Blue) water / liter ]",
#   plastic = "Plastic Pollution<br>[ g plastic / liter ]"
#)

# FIG 1 vol_dbsc_s0 ------------------------------------------------------

flow_s0_fig <- read_csv("./data/vol_scen0_dbsc_din+vend.csv") %>%
  mutate(distrib = factor(distrib, levels=c("Dining + Vending", "dining", "vending"))) %>%
  mutate(SSB_status = as_factor(SSB_status)) %>%
  mutate(SSB_status = fct_rev(SSB_status))
View(flow_s0_fig)


FIG1 <- flow_s0_fig %>%
  left_join(contNames) %>%
  left_join(bevNames) %>%
  ggplot() + theme_bw() +
  geom_col(aes(y = vol_kL, x = bev_name, fill = cont_name), width = 0.8) +
  facet_grid(rows = vars(distrib),
             cols = vars(SSB_status),
             switch = "y") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0.5) +
  labs(
    x ="Beverage Category",
    y = "thousand liters / yr",
    fill = "container\ntype") +
  theme(
    text = element_text(size=14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    #    legend.text = element_text(size = 16),
    #    axis.text.y = element_text(size = 16),
    panel.border = element_blank(),
    strip.placement = "outside") +
  scale_fill_brewer(palette = "Dark2")
FIG1
ggsave("./figs/FIG1_vol_scen0_dbsc.pdf", width=8.5, height=8.5, units="in")

## Make B&W version
FIG1 +
  scale_fill_grey()
ggsave("./figs/figsBW/FIG1bw_vol_scen0_dbsc.pdf", width=8.5, height=8.5, units="in")

remove(FIG1)


# vol_bsc_s0 --------------------------------------------

#Scale the Flow data for allscen (not including distrib)
flow_scl <- read_csv("./data/flow_bsc_allscen.csv") %>%
  mutate(vol_kL = vol/1000) %>%   #Resacle values
  select(!vol) %>%
  mutate(SSB_status = as_factor(SSB_status)) %>%
  mutate(SSB_status = fct_rev(SSB_status))
View(flow_scl)

# !!! Test that all scenarios have same volume
flow_scl %>%
  group_by(scen) %>%
  summarize(TOTAL = sum(vol_kL))

flow_bsc_scen0 <- flow_scl %>%
  filter(scen == "scen0") %>%
  group_by(bev_type, SSB_status, cont_type) %>%
  summarize(vol_kL = sum(vol_kL)) %>%
  ungroup() %>%
  filter(bev_type != "water_ucsb") %>%
  left_join(bevNames) %>%
  left_join(contNames) %>%
  mutate(bev_name = fct_reorder(bev_name, vol_kL, .fun = (sum), .desc =  TRUE))
#View(flow_0)

## By bev type, cont type, and SSB status
flow_bsc_scen0 %>%
  ggplot() + theme_bw() +
  geom_col(aes(y = vol_kL, x = bev_name, fill = cont_name), width = 0.8) +
  facet_wrap(vars(SSB_status)) +
  #             cols = vars(distrib),
  #             switch = "y") 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0.5) +
  labs(
    x ="Beverage Category",
    y = "thousand liters / yr",
    fill = "container\ntype") +
  theme(
    text = element_text(size=14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    #    legend.text = element_text(size = 16),
    #    axis.text.y = element_text(size = 16),
    panel.border = element_blank(),
    strip.placement = "outside") +
  scale_fill_brewer(palette = "Dark2")

ggsave("./figs/vol_scen0_bsc.pdf", width=8.5, height=7, units="in")

remove(flow_bsc_scen0)

# vol_bs_s0 -------------------------------------------------------

flow_bs_s0 <- flow_scl %>%
  filter(scen == "scen0") %>%
  group_by(bev_type, SSB_status) %>%
  summarize(vol_kL = sum(vol_kL)) %>%
  ungroup() %>%
  filter(bev_type != "water_ucsb") %>%
  left_join(bevNames) %>%
  mutate(bev_name = fct_reorder(bev_name, vol_kL, .fun = (sum), .desc =  TRUE))

flow_bs_s0 %>%
  ggplot() + theme_bw() +
  geom_col(aes(y = vol_kL, x = bev_name, fill = SSB_status), width = 0.8) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0.5) +
  labs(
    x ="Beverage Category",
    y = "thousand liters / yr",
    title = "Beverages purchased at UCSB") +
  theme(
    text = element_text(size=14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_blank(),
    strip.placement = "outside")

ggsave("./figs/vol_scen0_bs.pdf", width=7, height=4, units="in")
remove(flow_bs_s0)

# vol_bc_allscen ---------------------------------------------------------------

## Bevtype, container type, scenario

flow_bc_allscen <- flow_scl %>%
  group_by(scen, bev_type, cont_type) %>%
  summarize(vol_kL = sum(vol_kL)) %>%
  ungroup() %>%
  left_join(contNames) %>%
  left_join(bevNames) %>%
  left_join(scenNames)

FIGSI1 <- flow_bc_allscen %>% 
  ggplot() + 
  theme_bw() +
  geom_col(aes(y = vol_kL, x = bev_name, fill = cont_name),
           width = 0.8) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0.5) +
  facet_grid(rows = vars(scen_name),
             scales = "free_y",
             switch = "y") +
  labs(
    x ="Beverage Category",
    y = "thousand liters / yr",
    fill = "container\ntype") +
  theme(
    text = element_text(size=14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_blank(),
    strip.placement = "outside") +
  scale_fill_brewer(palette = "Dark2")
FIGSI1  
ggsave("./figs/FIGSI1_vol_bc_allscen.pdf", width=9, height=11, units = "in")
remove(FIGSI1)
  
## Make tbl for reporting

#View(flow_bc_allscen) # Note the units = kilo liters / yr
flow_bc_allscen %>%
  select(c("scen_name", "vol_kL", "cont_name", "bev_name")) %>%
  pivot_wider(names_from = bev_name, values_from = vol_kL) %>%
  arrange(cont_name) %>%
  arrange(scen_name) %>%
  replace(is.na(.), 0) %>%
  write_csv("./data/data_tbls/TSI9_flow_kL_bc_allscen.csv")

remove(flow_bc_allscen)
    
# IMPACTS: format data -----------------------------------------------------------------

imp_temp <- read_csv("./data/data_gen/imp_bsc_allscen.csv") %>%
  rename(imp_type = impact_type)
#View(imp_temp)

## Rescale impact values
imp_scl <- imp_temp %>%
  mutate(
    value = case_when(
      imp_type == "ghg" ~ value / 10^6,
      imp_type == "h2o" ~ value / 10^6,
      imp_type == "plastic" ~ value / 10^3
    )) %>%
  mutate(item = recode(item, bev = "beverage", cont = "container"))
  
#View(imp_scl)
remove(imp_temp)

## Hack 1: give "SSB" versions of bottled water and ucsb water (filtered tap)
# To have names line up in facet figures

temp_s0 <- imp_scl %>%
  filter(scen == "scen0") %>%
  filter(bev_type != "water_ucsb")
#View(temp_s0)

h1 <- temp_s0 %>%
  filter(bev_type == "water_bottled")%>%
  mutate(SSB_status = "SSB") %>%
  mutate(value = 0)
#View(h1)

imp_temp <- temp_s0 %>%
  bind_rows(h1) %>%
  group_by(bev_type, SSB_status, item, imp_type) %>%
  summarize(value = sum(value)) %>%
  ungroup()
#View(imp_temp)

## Make sure column names are same as the "names" file (up next)
imp_s0 <- imp_temp %>%
  left_join(bevNames) %>%
  left_join(impNames)
View(imp_s0)

remove(h1)
remove(temp_s0)
remove(imp_temp)

# FIG 4 imp_ibs_s0 ----------------------------------------------------------

FIG4 <- imp_s0 %>%
  ggplot() + theme_bw() +
  geom_col(aes(y = value, x = SSB_status, fill = item),
           width = 0.8) +
  facet_grid(vars(imp_regex_abs),
             vars(bev_name),
             switch = "y",
             scales = "free",
#             labeller = label_parsed
             labeller = labeller(
               imp_regex_abs = label_parsed,
               bev_name = label_wrap_gen(11)
             )
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0.5) +
  xlab("") +
  ylab("") +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title=element_blank(),
    legend.text=element_text(size=18),
    panel.border = element_blank(),
    strip.placement = "outside",
    legend.position = c(0.93, 0.25),
    legend.key.size = unit(0.92, 'cm'),
    legend.box.background = element_rect(colour = "black")
    #    strip.text = element_markdown()
  ) +
  scale_fill_manual(values = col_vir1)
FIG4
ggsave("./figs/FIG4_imp_ibs_scen0.pdf", width=13, height=8, units="in")

## BW version
FIG4 +
  scale_fill_grey(
    start = 0.7,
    end = 0.4)
ggsave("./figs/figsBW/FIG4bw_imp_ibs_scen0.pdf", width=13, height=8, units="in")

## Data table for SUPP

#View(imp_s0)
imp_s0 %>%
  select(imp_type, SSB_status, item, bev_name, value) %>%
  pivot_wider(names_from = bev_name, values_from = value) %>%
  arrange(imp_type) %>%
  write_csv("./data/data_tbls/TSI10_imp_ibs_scen0.csv")


# imp_i_allscen -------------------------------------------------------
View(imp_scl)

imp_i_allscen <- imp_scl %>%
  group_by(scen, item, imp_type) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  left_join(scenNames) %>%
  relocate(scen_name) %>%
  select(-scen) %>%
  left_join(impNames)
View(imp_i_allscen)

FIG5 <- ggplot() + theme_bw() +
  geom_col(data = imp_i_allscen,
           aes(y = value, x = scen_name, fill = item),
           width = 0.8) +
  facet_grid(rows = vars(imp_regex_abs),
             switch = "y",
             scales = "free",
             labeller = labeller(
               imp_regex_abs = label_parsed
             )
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0.5) +
  xlab("") +
  ylab("") +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_blank(),
    strip.placement = "outside",
    legend.title=element_blank(),
    legend.position = "top",
    legend.text=element_text(size=18),
    legend.key.size = unit(1, 'cm'),
    ) +
  scale_fill_manual(values = col_vir1)
FIG5
ggsave("./figs/FIG5_imp_i_allscen.pdf", width=10, height=10, units="in")

## BW version
FIG5 + 
  scale_fill_grey(
    start = 0.7,
    end = 0.35)
ggsave("./figs/figsBW/FIG5bw_imp_ibs_scen0.pdf", width=13, height=8, units="in")

## Make a data table for SUPP

imp_i_allscen %>%
  select(scen_name, imp_name, item, value) %>%
  pivot_wider(names_from = scen_name, values_from = value) %>%
  arrange(imp_name) %>%
  write_csv("./data/data_tbls/TSI11_imp_i_allscen_wide.csv")


# imp_is_allscen ----------------------------------------------

imp_is_allscen <- imp_scl %>%
  group_by(scen, item, SSB_status, imp_type) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  left_join(scenNames) %>%
  relocate(scen_name) %>%
  select(-scen) %>%
  left_join(impNames)
View(imp_is_allscen)

imp_is_allscen %>% ggplot() +
  theme_bw() +
  geom_col(aes(y = value, x = SSB_status, fill = item),
           width = 0.8) +
  facet_grid(vars(imp_regex_abs),
             vars(scen_name),
             switch = "y",
             scales = "free",
             labeller = labeller(
               imp_regex_abs = label_parsed
             )
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0.5) +
  xlab("") +
  ylab("") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_blank(),
    strip.placement = "outside",
    text = element_text(size=14),
    legend.title=element_blank()
    ) +
  scale_fill_manual(values = col_vir1)

ggsave("./figs/imp_is_allscen.pdf", width=13, height=8, units="in")


# Impact tables for reporting ---------------------------------------------

summ_imp_wide <- imp_scl %>%
  pivot_wider(names_from = imp_type, values_from = value) %>%
  replace(is.na(.), 0)
View(summ_imp_wide)
write_csv(summ_imp_wide, "./data/data_tbls/imp_bsc_allscen_wide.csv")

#df_imp_scen_bev %>% pivot_wider(names_from = scen, values_from = value))
#Totals by scenario

View(imp_i_allscen)
imp_i_allscen %>%
  group_by(scen_name, imp_name) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  pivot_wider(names_from = imp_name, values_from = value) %>%
  write_csv("./data/data_tbls/imp_allscen.csv")

imp_s0 %>%
  group_by(imp_name, item) %>%
  summarize(value = sum(value)) %>%
  pivot_wider(names_from = item, values_from = value) %>%
  replace_na(list(beverage = 0)) %>%
  write_csv("./data/data_tbls/imp_scen0_summ.csv")

 tbl_imp_scen0 <- imp_s0 %>%
  group_by(imp_name, item) %>%
  summarize(value = sum(value)) %>%
  pivot_wider(names_from = item, values_from = value) %>%
  replace_na(list(beverage = 0)) %>%
  write_csv("./data/data_tbls/result_overview.csv")

#View(tbl_imp_scen0)

# Per liter container impacts -------------------------------------------------

#View(contNames)

c1 <- read_csv("./data/cont_imp_l.csv") %>%
  left_join(contNames) %>%
  select(!cont_name_2) %>%
  select(!cont_type) %>%
  pivot_longer(!cont_name, names_to = "imp_type", values_to = "val") %>%
  left_join(impNames)
View(c1)
View(impNames)

FIG3 <- c1 %>%
  ggplot() + theme_bw() +
  geom_col(aes(y = val, x = cont_name), width = 0.8, fill = col_cont) +
  facet_grid(rows = vars(imp_regex_perL),
             switch = "y",
             scales = "free",
             labeller = labeller(
               imp_regex_perL = label_parsed
             )) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0.4) +
  labs(
    x ="Container Category",
    y = "Impacts per liter of container (no beverage)") +
  theme(
    text = element_text(size=14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    #    legend.text = element_text(size = 16),
    #    axis.text.y = element_text(size = 16),
    panel.border = element_blank(),
    strip.placement = "outside")
FIG3
ggsave("./figs/FIG3_cont_imp_liter.pdf", width=4, height=8, units="in")


## B&W version


c1 %>%
  ggplot() + theme_bw() +
  geom_col(aes(y = val, x = cont_name), width = 0.8) +
  facet_grid(rows = vars(imp_regex_perL),
             switch = "y",
             scales = "free",
             labeller = labeller(
               imp_regex_perL = label_parsed
             )) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0.4) +
  labs(
    x ="Container Category",
    y = "Impacts per liter of container (no beverage)") +
  theme(
    text = element_text(size=14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    #    legend.text = element_text(size = 16),
    #    axis.text.y = element_text(size = 16),
    panel.border = element_blank(),
    strip.placement = "outside")
ggsave("./figs/figsBW/FIG3bw_cont_imp_liter.pdf", width=4, height=8, units="in")


# Per liter beverage impacts ----------------------------------------------
  
View(bevNames)

temp1 <- read_csv("./data/bev_imp_l.csv")
View(temp1)
b1 <- temp1 %>%
  filter(bev_type != "water_tap") %>%
  left_join(bevNames) %>%
  select(-c("bev_type", "bev_regex")) %>%
  pivot_longer(!c("bev_name", "SSB_status"), names_to = "imp_type", values_to = "val") %>%
  drop_na() %>%
  left_join(impNames)
View(b1)
remove(temp1)


## The previous way to get good impact names
# b1$imp_type <- impname_l[
#   match(b1$imp_type, names(impname_l)) ]
#View(b1)
  
FIG2 <- b1 %>% ggplot() +
  theme_bw() +
    geom_col(aes(y = val, x = bev_name), width = 0.8, fill = col_bev) +
    facet_grid(rows = vars(imp_regex_perL),
               cols = vars(SSB_status),
               switch = "y",
               scales = "free",
               labeller = labeller(
                 imp_regex_perL = label_parsed
               )) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0.41) +
    labs(
      x ="Beverage Category",
      y = "Impacts per liter of beverage (no container)") +
    theme(
      text = element_text(size=14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.placement = "outside",
      #    legend.text = element_text(size = 16),
      #    axis.text.y = element_text(size = 16),
      panel.border = element_blank() ) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific=FALSE))
FIG2
ggsave("./figs/FIG2_bev_imp_liter.pdf", width=8, height=8, units="in")

## Make BW version
b1 %>% ggplot() +
  theme_bw() +
  geom_col(aes(y = val, x = bev_name), width = 0.8) +
  facet_grid(rows = vars(imp_regex_perL),
             cols = vars(SSB_status),
             switch = "y",
             scales = "free",
             labeller = labeller(
               imp_regex_perL = label_parsed
             )) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0.41) +
  labs(
    x ="Beverage Category",
    y = "Impacts per liter of beverage (no container)") +
  theme(
    text = element_text(size=14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.placement = "outside",
    #    legend.text = element_text(size = 16),
    #    axis.text.y = element_text(size = 16),
    panel.border = element_blank() ) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific=FALSE))
ggsave("./figs/figsBW/FIG2bw_bev_imp_liter.pdf", width=8, height=8, units="in")
