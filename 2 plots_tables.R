# Summarize, plot, and make tables for results and documentation we want to show
  # Flow (in liter) of beverages by scenario, beverage type, container type, and SSB status
  # This is where we rescale as necessary, and give nicer names

library(tidyverse)
library(ggtext)


# Definitions ------------------------------------------------------

# Colors --
col_vir1 <- c("beverage" = "#21908C", "container" = "#440154")
col_bev <- c("#21908C")
col_cont <- c("#440154")

# Names --
  # Human-friendly impact names to be used in ggplot.labeller
    #(should probably do something similar with 'the 'bev_type', 'scen', etc, but not right now)

# BIG impacts (tonnes, M liter, kg)
 # Formatted in markdown, to use element_markdown
impnames <- c(
  ghg = "Climate<br> t CO2eq (100yr) ",
  h2o = "Blue Water Use<br>[ M liter (Blue) water ]",
  plastic = "Plastic Pollution<br>[ kg plastic ]"
)
#View(impnames)

# Impacts per liter
impname_l <- c(
  ghg = "Climate<br>[ g CO2eq / liter ]",
  h2o = "Blue Water Use<br>[ liter (Blue) water / liter ]",
  plastic = "Plastic Pollution<br>[ g plastic / liter ]"
)

## Get names

bev_names <- read_csv("./data/bevtype_list_name.csv")
#View(bev_names)
cont_names <- read_csv("./data/conttype_list_name.csv")
#View(cont_names)
impNames <- read_csv("./data/imptype_list_name.csv")  ## For experimenting
View(impNames)

# vol_dbsc_s0 ------------------------------------------------------

flow_s0 <- read_csv("./data/flow_dbsc_scen0.csv") %>%
  mutate(vol_kL = vol/1000) %>%   # Resacle as-consumed volume
  select(!vol) %>%
  left_join(bev_names) %>%
  select(!bev_type) %>%
  rename(bev_type = bev_name) %>%
  write_csv("./data/data_tbls/flow_dbsc_scen0_scaled.csv")
#View(flow_s0)

temp0 <- flow_s0 %>% # for vol fig, we want a row with distrib = "total"
  group_by(bev_type, SSB_status, cont_type) %>%
  summarize(vol_kL = sum(vol_kL)) %>%
  ungroup() %>%
  mutate(distrib = "Dining + Vending") %>%
  mutate(bev_type = fct_reorder(bev_type, vol_kL, .fun = (sum), .desc =  TRUE))
#View(temp0)

# Make table for plotting with distrib = 'total', "vend", and "dining"
flow_s0_fig <- flow_s0 %>%
#  mutate(bev_type = fct_reorder(bev_type, vol_kL, .fun = (sum), .desc =  TRUE)) %>%
  bind_rows(temp0) %>%
  mutate(distrib = factor(distrib, levels=c("Dining + Vending", "dining", "vending"))) %>%
  mutate(SSB_status = as_factor(SSB_status)) %>%
  mutate(SSB_status = fct_rev(SSB_status)) %>%
  write_csv("./data/data_tbls/vol_scen0_dbsc_DATA.csv")
#View(flow_s0_fig)

FIG1 <- flow_s0_fig %>%
  left_join(cont_names) %>%
  ggplot() + theme_bw() +
  geom_col(aes(y = vol_kL, x = bev_type, fill = cont_name), width = 0.8) +
  facet_grid(rows = vars(distrib),
             cols = vars(SSB_status),
             switch = "y") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0.5) +
  labs(
    x ="Beverage Category",
    y = "thousand liters / yr",
    fill = "container \ntype") +
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


# vol_bsc_s0 --------------------------------------------

#Scale the Flow data for allscen (not including distrib)
flow_scl <- read_csv("./data/flow_bsc_allscen.csv") %>%
  mutate(vol_kL = vol/1000) %>%   #Resacle values
  select(!vol) %>%
  left_join(bev_names) %>%
  select(!bev_type) %>%
  rename(bev_type = bev_name)
#View(flow_scl)

# !!! Test that all scenarios have same volume
flow_scl %>%
  group_by(scen) %>%
  summarize(TOTAL = sum(vol_kL))

flow_0 <- flow_scl %>%
  filter(scen == "scen0") %>%
  mutate(bev_type = fct_reorder(bev_type, vol_kL, .fun = (sum), .desc =  TRUE))
#View(flow_0)

## By bev type, cont type, and SSB status
flow_0 %>%
  group_by(bev_type, SSB_status, cont_type) %>%
  summarize(vol_kL = sum(vol_kL)) %>%
  ungroup() %>%
  filter(bev_type != "Water, Filtered tap") %>%
  ggplot() + theme_bw() +
  geom_col(aes(y = vol_kL, x = bev_type, fill = cont_type), width = 0.8) +
  facet_wrap(vars(SSB_status)) +
  #             cols = vars(distrib),
  #             switch = "y") 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0.5) +
  labs(
    x ="Beverage Category",
    y = "thousand liters / yr") +
  #    title = "Beverages purchased at UCSB") +
  theme(
    text = element_text(size=14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    #    legend.text = element_text(size = 16),
    #    axis.text.y = element_text(size = 16),
    panel.border = element_blank(),
    strip.placement = "outside") +
  scale_fill_brewer(palette = "Dark2")

ggsave("./figs/vol_scen0_bsc.pdf", width=8.5, height=7, units="in")


# vol_bs_s0 -------------------------------------------------------

flow_0 %>%
  group_by(bev_type, SSB_status) %>%
  summarize(vol_kL = sum(vol_kL)) %>%
  ungroup() %>%
  filter(bev_type != "Water, Filtered tap") %>%
  ggplot() + theme_bw() +
  geom_col(aes(y = vol_kL, x = bev_type, fill = SSB_status), width = 0.8) +
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


# vol_bc_allscen ---------------------------------------------------------------

## Bevtype, container type, scenario

flow_scl %>%
  left_join(cont_names) %>%
  group_by(scen, bev_type, cont_name) %>%
  summarize(vol_kL = sum(vol_kL)) %>%
  ungroup() %>%
  mutate(scen = recode(scen, scen0 = "Baseline")) %>%
#  mutate(bev_type = recode(bev_type, "Water, UCSB" = "Water, tap")) %>%
#  select(!vol) %>% 
  ggplot() + theme_bw() +
  geom_col(aes(y = vol_kL, x = bev_type, fill = cont_name),
           width = 0.8) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0.5) +
  facet_grid(rows = vars(scen),
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
  
ggsave("./figs/vol_bc_allscen.pdf", width=9, height=11, units = "in")
  
# Make tbl for reporting

#View(flow_scl) # Note the units = kilo liters / yr

flow_scl %>%
#  group_by(scen, cont_type, bev_type) %>%
  group_by(across(c(-SSB_status, -vol_kL)))%>%
  summarize(vol_kL = sum(vol_kL)) %>%
  ungroup() %>%
  pivot_wider(names_from = bev_type, values_from = vol_kL) %>%
  mutate(scen = recode(scen, scen0 = "Baseline")) %>%
  arrange(cont_type) %>%
  arrange(scen) %>%
  replace(is.na(.), 0) %>%
  write_csv("./data/data_tbls/flow_kL_bc_allscen.csv")
    
# IMPACTS: format data -----------------------------------------------------------------

## Impact data
imp <- read_csv("./data/data_gen/imp_bsc_allscen.csv")
#View(imp)

## Rescale impact values
imp_scl <- read_csv("./data/data_gen/imp_bsc_allscen.csv") %>%
  mutate(
    value = case_when(
      impact_type == "ghg" ~ value / 10^6,
      impact_type == "h2o" ~ value / 10^6,
      impact_type == "plastic" ~ value / 10^3
    )) %>%
  left_join(bev_names) %>%
  select(!bev_type) %>%
  rename(bev_type = bev_name)
View(imp_scl)
## Give impacts nicer names
## Seems like there should be a neater way to do this
## But not now
imp_scl$impact_name <- impnames[ 
  match(imp_scl$impact_type, names(impnames)) ]
View(imp_scl)

# imp_ibs_s0 ----------------------------------------------------------
#View(imp_scl)
temp_s0 <- imp_scl %>%
  select(-impact_type) %>%
  filter(scen == "scen0") %>%
  filter(bev_type != "Water, Filtered tap")
View(temp_s0)

## Make a "hack" to give SSB impacts = 0 for water bev types
## For aesthetic and plot readability reasons
h1 <- temp_s0 %>%
  filter(bev_type == "Water, bottled")%>%
  mutate(SSB_status = "SSB") %>%
  mutate(value = 0)
View(h1)

imp_s0 <- temp_s0 %>%
  bind_rows(h1) %>%
  group_by(bev_type, SSB_status, item, impact_name) %>%
  summarize(value = sum(value)) %>%
  ungroup()
View(imp_s0)

FIG4 <- imp_s0 %>%
  filter(bev_type != "Water, Filtered tap") %>%
  mutate(item = recode(item, bev = "beverage", cont = "container")) %>%
ggplot() + theme_bw() +
  geom_col(aes(y = value, x = SSB_status, fill = item),
           width = 0.8) +
  facet_grid(vars(impact_name),
             vars(bev_type),
#             switch = "y",
             scales = "free"
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0.5) +
  xlab("") +
  ylab("") +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title=element_blank(),
    legend.text=element_text(size=18),
    panel.border = element_blank(),
    strip.placement = "outside",
    legend.position = c(0.93, 0.25),
    legend.key.size = unit(1, 'cm'),
    legend.box.background = element_rect(colour = "black"),
#    strip.text = element_markdown(),
    strip.text.y = element_markdown(size = 12)
    ) +
  scale_fill_manual(values = col_vir1)
#  scale_fill_brewer(palette = "Dark2")
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
  pivot_wider(names_from = bev_type, values_from = value) %>%
  relocate(impact_name) %>%
  arrange(impact_name) %>%
  write_csv("./data/data_tbls/imp_ibs_scen0.csv")


# imp_i_allscen -------------------------------------------------------
#View(imp_scl)

imp_i_allscen <- imp_scl %>%
  select(-impact_type) %>%
  group_by(scen, item, impact_name) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  mutate(scen = recode(scen, scen0 = "Baseline")) %>%
  mutate(item = recode(item, bev = "beverage", cont = "container"))
#View(imp_i_allscen)

FIG5 <- ggplot() + theme_bw() +
  geom_col(data = imp_i_allscen,
           aes(y = value, x = scen, fill = item),
           width = 0.8) +
  facet_grid(rows = vars(impact_name),
             switch = "y",
             scales = "free"
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
  pivot_wider(names_from = scen, values_from = value) %>%
  relocate(impact_name) %>%
  arrange(impact_name) %>%
  write_csv("./data/data_tbls/imp_i_allscen_wide.csv")


# imp_is_allscen ----------------------------------------------

imp_is_allscen <- imp_scl %>%
  select(-impact_type) %>%
  group_by(scen, item, SSB_status, impact_name) %>%
  summarize(value = sum(value)) %>%
  ungroup()
#View(imp_is_allscen)

imp_is_allscen %>%
  mutate(scen = recode(scen, scen0 = "Baseline")) %>%
  mutate(item = recode(item, bev = "beverage", cont = "container")) %>%
ggplot() + theme_bw() +
  geom_col(aes(y = value, x = SSB_status, fill = item),
           width = 0.8) +
  facet_grid(vars(impact_name),
             vars(scen),
             switch = "y",
             scales = "free",
             labeller = label_wrap_gen()
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
  select(-impact_type) %>%
  pivot_wider(names_from = impact_name, values_from = value) %>%
  replace(is.na(.), 0)
#View(summ_imp_wide)
write_csv(summ_imp_wide, "./data/data_tbls/imp_bsc_allscen_wide.csv")

#df_imp_scen_bev %>% pivot_wider(names_from = scen, values_from = value))
#Totals by scenario

imp_i_allscen %>%
  group_by(scen, impact_name) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  pivot_wider(names_from = impact_name, values_from = value) %>%
  write_csv("./data/data_tbls/imp_allscen.csv")

imp_s0 %>%
  group_by(impact_name, item) %>%
  summarize(value = sum(value)) %>%
  pivot_wider(names_from = item, values_from = value) %>%
  replace_na(list(bev = 0)) %>%
  write_csv("./data/data_tbls/imp_scen0_summ.csv")

tbl_imp_scen0 <- imp_s0 %>%
  group_by(impact_name, item) %>%
  summarize(value = sum(value)) %>%
  pivot_wider(names_from = item, values_from = value) %>%
  replace_na(list(bev = 0)) %>%
  write_csv("./data/data_tbls/result_overview.csv")

#View(tbl_imp_scen0)

# Per liter container impacts -------------------------------------------------

#View(cont_names)

c1 <- read_csv("./data/cont_imp_l.csv") %>%
  left_join(cont_names) %>%
  select(!cont_name_2) %>%
  select(!cont_type) %>%
  rename(cont_type = cont_name)  %>%
  pivot_longer(!cont_type, names_to = "imp_type", values_to = "val")
#View(c1)

c1$imp_type <- impname_l[
  match(c1$imp_type, names(impname_l)) ]
#View(c1)

FIG3 <- c1 %>%
  ggplot() + theme_bw() +
  geom_col(aes(y = val, x = cont_type), width = 0.8, fill = col_cont) +
  facet_grid(rows = vars(imp_type),
             switch = "y",
             scales = "free") +
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
ggsave("./figs/FIG3_cont_imp_liter.pdf", width=4, height=8, units="in")


## B&W version

c1 %>%
  ggplot() + theme_bw() +
  geom_col(aes(y = val, x = cont_type), width = 0.8) +
  facet_grid(rows = vars(imp_type),
             switch = "y",
             scales = "free") +
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
  
#View(bev_names)

b1 <- read_csv("./data/bev_imp_l.csv") %>%
  left_join(bev_names) %>%
  select(!bev_type) %>%
  rename(bev_type = bev_name)   %>%
  pivot_longer(!c("bev_type", "SSB_status"), names_to = "imp_type", values_to = "val")
#View(b1)

b1$imp_type <- impname_l[
  match(b1$imp_type, names(impname_l)) ]
#View(b1)
  
b1 %>%
  drop_na() %>%
  filter(bev_type != "Water, tap") %>%
#  mutate(bev_type = recode(bev_type, "Water, UCSB" = "Water, tap")) %>%
    ggplot() + theme_bw() +
    geom_col(aes(y = val, x = bev_type), width = 0.8, fill = col_bev) +
    facet_grid(rows = vars(imp_type),
               cols = vars(SSB_status),
               switch = "y",
               scales = "free") +
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
ggsave("./figs/FIG2_bev_imp_liter.pdf", width=8, height=8, units="in")

## Make BW version
b1 %>%
  drop_na() %>%
  filter(bev_type != "Water, tap") %>%
  #  mutate(bev_type = recode(bev_type, "Water, UCSB" = "Water, tap")) %>%
  ggplot() + theme_bw() +
  geom_col(aes(y = val, x = bev_type), width = 0.8) +
  facet_grid(rows = vars(imp_type),
             cols = vars(SSB_status),
             switch = "y",
             scales = "free") +
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


orig_sum_labs <- c("peak_time","peak_vir","eq_vir","rel_peak")
new_sum_labs <- c("peak\ntime","peak\nvirulence","equilibrium\nvirulence",
                  "relative\npeak")
orig_symb_labs <- c("beta","gamma","x[1]")

fake <- data.frame(f1=rep(orig_sum_labs,each=12),
                   f2=factor(rep(1:3,16),levels=1:3,
                             labels=c("beta","gamma","x[1]")),
                   x=rep(1:4,12),y=rep(1:4,12))
View(fake)

fake <- data.frame(f1=rep(new_sum_labs,each=12),
                   f2=rep(orig_symb_labs, each = 16),
                   x=rep(1:4,12),y=rep(1:4,12))