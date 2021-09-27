
library(tidyverse)
#getwd()


# names -------------------------------------------------------------------

# Colors --
col_vir1 <- c("beverage" = "#21908C", "container" = "#440154")
col_bev <- c("#21908C")
col_cont <- c("#440154")

# Names --
# Human-friendly impact names to be used in ggplot.labeller
#(should probably do something similar with 'the 'bev_type', 'scen', etc, but not right now)

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


# data --------------------------------------------------------------------

imp_temp <- read_csv("./data/data_gen/imp_bsc_allscen.csv") %>%
  rename(imp_type = impact_type) %>%
  mutate(item = recode(item, bev = "beverage", cont = "container"))
  
#View(imp_temp)

## Rescale impact values
imp_scl <- imp_temp %>%
  mutate(
    value = case_when(
      imp_type == "ghg" ~ value / 10^6,
      imp_type == "h2o" ~ value / 10^6,
      imp_type == "plastic" ~ value / 10^3
    ))
#View(imp_scl)
remove(imp_temp)



temp_s0 <- imp_scl %>%
#  select(-impact_type) %>%
  filter(scen == "scen0") %>%
  filter(bev_type != "water_ucsb")
#View(temp_s0)

## Make a "hack" to give SSB impacts = 0 for water bev types
# For aesthetic and plot readability reasons
h1 <- temp_s0 %>%
  filter(bev_type == "water_bottled")%>%
  mutate(SSB_status = "SSB") %>%
  mutate(value = 0)
#View(h1)

imp_s0 <- temp_s0 %>%
  bind_rows(h1) %>%
  group_by(bev_type, SSB_status, item, imp_type) %>%
  summarize(value = sum(value)) %>%
  ungroup()
View(imp_s0)
remove(h1)
remove(temp_s0)

# Names -------------------------------------------------------------------

bevNames_temp <- read_csv("./data/bevtype_list_name.csv")
#View(bevNames_temp)
bevNames <- bevNames_temp %>%
  mutate(bev_regex = bev_name) %>%
#  mutate(bev_regex = str_replace(bev_regex, "+ ", "+~")) %>%
  mutate(bev_regex = str_replace(bev_regex, " ", "~")) %>%
  mutate(bev_regex = str_replace(bev_regex, ",", "*','*"))
View(bevNames)
remove(bevNames_temp)
contNames <- read_csv("./data/conttype_list_name.csv")
#View(contNames)
impNames <- read_csv("./data/imptype_list_name.csv")
#View(impNames)

#View(imp_s0)
impact_Pdata_temp <- imp_s0 %>%
  left_join(bevNames)








impact_Pdata <- impact_Pdata_temp %>%
  left_join(impNames)

# imp_scl$impact_name <- impnames_hand[ 
#   match(imp_scl$impact_type, names(impnames_hand)) ]
# View(imp_scl)


# Plot --------------------------------------------------------------------
View(impact_Pdata)

FIG4 <- impact_Pdata %>%
  ggplot() + theme_bw() +
  geom_col(aes(y = value, x = SSB_status, fill = item),
           width = 0.8) +
  facet_grid(vars(imp_regex_abs),
             vars(bev_regex),
             switch = "y",
             scales = "free",
             labeller = label_parsed
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
    legend.box.background = element_rect(colour = "black")
    #    strip.text = element_markdown()
  ) +
  scale_fill_manual(values = col_vir1)
#  scale_fill_brewer(palette = "Dark2")
FIG4
ggsave("./figs/figsTest/FIG4_imp_ibs_scen0_exp.pdf", width=13, height=8, units="in")

