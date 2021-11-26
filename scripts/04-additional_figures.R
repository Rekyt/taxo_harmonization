# Script to plot additional figures related to presentation of our work
# Packages ---------------------------------------------------------------------
library("dplyr")
library("ggplot2")

# Load data --------------------------------------------------------------------

database_df = readxl::read_xlsx(
  "data/data_raw/Table comparing taxonomic tools.xlsx",
  sheet = 4, na = c("", "NA")
)

raw_db_links_df = readxl::read_xlsx(
  "data/data_raw/Table comparing taxonomic tools.xlsx",
  sheet = 6, na = c("", "NA")
)

# Figure of Number of Databases Indexed by categories --------------------------

fig_number_dbs = database_df %>%
  mutate(
    `Spatial Scale` = `Spatial Scale` %>%
           factor(levels = c("Regional", "Global")),
    `Taxonomic Breadth` = `Taxonomic Breadth` %>%
      factor(levels = c("Small", "Medium", "Large"))
    ) %>% 
  ggplot(aes(`Taxonomic Breadth`, `Spatial Scale`)) +
  geom_count(aes(color = after_stat(n), size = after_stat(n))) +
  guides(color = 'legend') +
  scale_color_viridis_c() +
  labs(color = "N. databases", size = "N. databases") +
  theme_bw(18) +
  theme(legend.position = "top")


# Relationship network between databases ---------------------------------------