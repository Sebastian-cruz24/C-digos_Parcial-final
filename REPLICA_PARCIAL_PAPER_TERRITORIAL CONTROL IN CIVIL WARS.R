rm(list = ls())
############################
# Territorial Control paper
# Graphs and figures
#
# Versión 100% funcional desde GitHub + salida fija + vista inmediata
# Noviembre 2025
############################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, sf, lubridate, geodata, scales, ggpubr, ggalluvial,
  httr, xtable, VGAM, raster, rgeos
)

########################################
# CARPETA DE SALIDA FIJA (tuya)
########################################
plotdir <- "C:/Users/USUARIO/Desktop/CHAMBA PROVISIONAL/MAESTRÍA/ANÁLITICA DE DATOS/REVISIÓN_CÓDIGOS_ANDRE/OUTPUT"
if (!dir.exists(plotdir)) dir.create(plotdir, recursive = TRUE)

########################################
# Función para cargar RDS desde GitHub
########################################
load_github_rds <- function(url){
  tmp <- tempfile(fileext = ".rds")
  download.file(url, tmp, mode = "wb", quiet = TRUE)
  readRDS(tmp)
}

########################################
# URLs base
########################################
base_prepped <- "https://raw.githubusercontent.com/thereseanders/territorialcontrol-jpr/master/prepped/"
base_utils   <- "https://raw.githubusercontent.com/thereseanders/territorialcontrol-jpr/master/utils/"
base_acled   <- "https://raw.githubusercontent.com/thereseanders/territorialcontrol-jpr/master/prepped/acled/"

########################################
# Cargar funciones auxiliares
########################################
temp_w <- tempfile(fileext = ".R")
temp_h <- tempfile(fileext = ".R")
download.file(paste0(base_utils, "func_weights.R"), temp_w, mode = "wb", quiet = TRUE)
download.file(paste0(base_utils, "func_hmm.R"), temp_h, mode = "wb", quiet = TRUE)
source(temp_w); source(temp_h)
unlink(c(temp_w, temp_h))

########################################
# Cargar datos desde GitHub
########################################
nigeria_districts <- gadm(country = "NGA", level = 2, path = tempdir()) %>% st_as_sf()

events_nga      <- load_github_rds(paste0(base_prepped, "events_nga_hex25.rds"))
events_col      <- load_github_rds(paste0(base_prepped, "events_col_hex25.rds"))
events_nga_cont <- load_github_rds(paste0(base_prepped, "events_decay_nga_hex25_logistic_trunc.rds"))
events_col_cont <- load_github_rds(paste0(base_prepped, "events_decay_col_hex25_logistic_trunc.rds"))
hmm_col         <- load_github_rds(paste0(base_prepped, "hmm_col.rds"))
hmm_nga         <- load_github_rds(paste0(base_prepped, "hmm_nga.rds"))
grid_col        <- load_github_rds(paste0(base_prepped, "grids_hex_col_hex25.rds"))
grid_nga        <- load_github_rds(paste0(base_prepped, "grids_hex_nga_hex25.rds"))

# ACLED desde GitHub (datos reales)
acled_files <- c(
  "acledtesting_nganorth_hex25_control_flip_monthly_12m_full.rds",
  "acledtesting_nganorth_hex25_control_flip_monthly_12m_select.rds",
  "acledtesting_nganorth_hex25_control_flip_monthly_3m_full.rds",
  "acledtesting_nganorth_hex25_control_flip_monthly_3m_select.rds",
  "acledtesting_nganorth_hex25_control_flip_monthly_6m_full.rds",
  "acledtesting_nganorth_hex25_control_flip_monthly_6m_select.rds",
  "acledtesting_nganorth_hex25_control_flip_yearly_12m_full.rds",
  "acledtesting_nganorth_hex25_control_flip_yearly_12m_select.rds",
  "acledtesting_nganorth_hex25_control_flip_yearly_3m_full.rds",
  "acledtesting_nganorth_hex25_control_flip_yearly_3m_select.rds",
  "acledtesting_nganorth_hex25_control_flip_yearly_6m_full.rds",
  "acledtesting_nganorth_hex25_control_flip_yearly_6m_select.rds"
)

acled_ls <- list()
for (f in seq_along(acled_files)) {
  message("Cargando ACLED: ", acled_files[f])
  tmp_data <- load_github_rds(paste0(base_acled, acled_files[f]))
  name <- str_remove(acled_files[f], ".rds") %>%
    str_remove("acledtesting_nganorth_hex25_control_flip_")
  acled_ls[[f]] <- tmp_data %>%
    rename(control_test = control) %>%
    mutate(type = name) %>%
    separate(type, c("temporal", "duration", "type"), sep = "_")
  names(acled_ls)[[f]] <- name
}

########################################
# Figure 1 - Versión 100% funcional 2025
########################################
districts <- sort(c("Geidam", "Magumeri", "Kaga", "Jere", "Maiduguri", "Damboa", "Chibok", "Biu", "Kwaya Kusar", "Bayo", "Shani",
                    "Kukawa", "Monguno", "Marte", "Konduga", "Michika",
                    "Abadam", "Mobbar", "Guzamala", "Gubio", "Nganzai", "Mafa", "Ngala", "Kala/Balge", "Dikwa", "Bama", "Gwoza", "Madagali", "Askira/Uba", "Gujba", "Gulani", "Hawul"))

contested0225 <- c("Kukawa", "Monguno", "Marte", "Konduga", "Michika")
boko0225 <- c("Abadam", "Mobbar", "Guzamala", "Gubio", "Nganzai", "Mafa", "Ngala", "Kala/Balge", "Dikwa", "Bama", "Gwoza", "Madagali", "Askira/Uba", "Gujba", "Gulani")
contested0310 <- c("Mobbar", "Abadam", "Kukawa", "Monguno", "Marte", "Dikwa", "Mafa", "Konduga")
boko0310 <- c("Guzamala", "Gubio", "Nganzai", "Ngala", "Kala/Balge", "Bama", "Gwoza", "Askira/Uba","Gulani")
boko0318 <- c("Abadam", "Kala/Balge", "Gwoza")
boko0424 <- c("Gwoza")

df_control <- tibble(district = districts) %>%
  mutate(control0225 = case_when(district %in% contested0225 ~ "Contested", district %in% boko0225 ~ "Boko Haram", TRUE ~ "Government")) %>%
  mutate(control0310 = case_when(district %in% contested0310 ~ "Contested", district %in% boko0310 ~ "Boko Haram", TRUE ~ "Government")) %>%
  mutate(control0318 = case_when(district %in% boko0318 ~ "Boko Haram", TRUE ~ "Government")) %>%
  mutate(control0424 = case_when(district %in% boko0424 ~ "Boko Haram", TRUE ~ "Government"))

# CLAVE: Limpiamos y estandarizamos los objetos sf antes del join
events_nga_clean <- events_nga %>%
  st_zm(drop = TRUE) %>%           # Elimina dimensiones Z/M problemáticas (causa común del error)
  st_make_valid() %>%              # Repara geometrías inválidas
  st_set_crs(st_crs(nigeria_districts))  # Fuerza mismo CRS

select_sf <- nigeria_districts %>%
  filter(NAME_2 %in% districts) %>%
  st_make_valid() %>%
  select(NAME_2) %>%               # Quitamos columnas innecesarias que confunden a sf
  left_join(df_control, by = c("NAME_2" = "district")) %>%
  pivot_longer(cols = starts_with("control0"), names_to = "control", values_to = "actor") %>%
  mutate(window = case_when(
    control == "control0225" ~ "25 February",
    control == "control0310" ~ "10 March",
    control == "control0318" ~ "18 March",
    control == "control0424" ~ "24 April"
  ) %>% factor(levels = c("25 February", "10 March", "18 March", "24 April")))

# Ahora el join funciona perfecto
merged_sub <- st_join(events_nga_clean, select_sf, join = st_within) %>%
  filter(!is.na(window)) %>%
  mutate(type_label = case_when(
    type == "terrorism" ~ "Terrorist attack (GTD data)",
    type == "conventional" ~ "Conventional fighting (GED data)"
  ) %>% factor(levels = c("Conventional fighting (GED data)", "Terrorist attack (GTD data)")))

# Gráficos (idénticos al original)
p_map <- ggplot() +
  geom_sf(data = select_sf, aes(fill = actor), size = 0.2, color = "grey98", alpha = 0.2) +
  facet_wrap(~ window, nrow = 1) +
  geom_sf(data = merged_sub, aes(color = type_label, shape = type_label), alpha = 0.7, size = 1.7) +
  coord_sf(datum = NA) +
  scale_fill_manual(values = rev(c("#2c7bb6", "goldenrod", "#d7191c")),
                    name = "Territorial Control",
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) +
  scale_color_manual(values = c("#d7191c", "#2c7bb6"), name = "Rebel tactics") +
  scale_shape_manual(values = c(19, 17), name = "Rebel tactics") +
  labs(title = "Territorial control and conflict events in NE Nigeria in 2015",
       subtitle = "Conflict events within two weeks of observing territorial control") +
  theme_void() + theme(legend.position = "bottom")

ggsave(file.path(plotdir, "reuters_map.png"), width = 10, height = 4.5, dpi = 500)
print(p_map)
browseURL(file.path(plotdir, "reuters_map.png"))

p_map_bw <- p_map +
  scale_fill_manual(values = c("black", "grey", "white"), name = "Territorial Control")

ggsave(file.path(plotdir, "reuters_map_bw.png"), width = 10, height = 4.5, dpi = 500)
print(p_map_bw)
browseURL(file.path(plotdir, "reuters_map_bw.png"))

message("Figure 1 (color + blanco y negro) generada correctamente!")
########################################
# Figuras 4 y 5 (Colombia y Nigeria)
########################################
mycols <- rev(c("#2c7bb6","#abd9e9","#ffffbf","#fdae61","#d7191c"))

hmm_col_monthly <- hmm_col %>% mutate(control_num = case_when(control=="R"~0, control=="DR"~0.25, control=="D"~0.5, control=="DG"~0.75, control=="G"~1))
hmm_col_yearly <- hmm_col_monthly %>% group_by(gid,year) %>% summarise(control_mean = mean(control_num)) %>% left_join(grid_col, by="gid") %>% st_as_sf()

col_out_sf <- gadm("COL", level=0, path=tempdir()) %>% st_as_sf() %>% st_simplify(dTolerance=1000)

p_col_yearly <- ggplot() + geom_sf(data = col_out_sf, alpha=0.2, size=0.05) +
  geom_sf(data = subset(hmm_col_yearly, year %in% 2006:2017), aes(fill=control_mean), size=0.01, color="lightgrey") +
  facet_wrap(~year, nrow=3) + scale_fill_gradientn(colors=mycols, limits=c(0,1), breaks=c(0,0.5,1),
                                                   labels=c("Full rebel\ncontrol","Highly\ncontested","Full government\ncontrol")) +
  coord_sf(xlim=c(-79,-67), ylim=c(-4,12.5), datum=NA) + theme_void() + theme(legend.position="bottom", legend.key.width=unit(2,"cm"))
ggsave(file.path(plotdir, "hmm_col_yearly.png"), width=8, height=10, dpi=500); print(p_col_yearly); browseURL(file.path(plotdir, "hmm_col_yearly.png"))

nga_out_sf <- gadm("NGA", level=0, path=tempdir()) %>% st_as_sf() %>% st_simplify(dTolerance=500)
hmm_nga_monthly <- hmm_nga %>% mutate(control_num = case_when(control=="R"~0, control=="DR"~0.25, control=="D"~0.5, control=="DG"~0.75, control=="G"~1))
hmm_nga_yearly <- hmm_nga_monthly %>% group_by(gid,year) %>% summarise(control=mean(control_num)) %>% left_join(grid_nga, by="gid") %>% st_as_sf()

p_nga_yearly <- ggplot(subset(hmm_nga_yearly, year>2008)) + geom_sf(data=nga_out_sf, alpha=0.6, size=0.1) +
  geom_sf(aes(fill=control), size=0.01, color="lightgrey") + facet_wrap(~year, nrow=3) +
  scale_fill_gradientn(colors=mycols, limits=c(0,1), breaks=c(0,0.5,1),
                       labels=c("Full rebel\ncontrol","Highly\ncontested","Full government\ncontrol")) +
  theme_void() + theme(legend.position="bottom", legend.key.width=unit(2,"cm"))
ggsave(file.path(plotdir, "hmm_nga_yearly.png"), width=8, height=8, dpi=500); print(p_nga_yearly); browseURL(file.path(plotdir, "hmm_nga_yearly.png"))

########################################
# Figura 6 - Validación ACLED
########################################
acled <- bind_rows(acled_ls)
merged_monthly <- acled %>% filter(temporal=="monthly") %>%
  select(gid, year, month, control_test, type, duration) %>% left_join(hmm_nga_monthly)

ti <- unique(merged_monthly$timeindex)
cors_full_6m <- numeric(length(ti))
for(y in seq_along(ti)){
  sub <- merged_monthly %>% filter(timeindex == ti[y])
  cors_full_6m[y] <- cor(sub$control_test[sub$type=="full" & sub$duration=="6m"],
                         sub$control_num[sub$type=="full" & sub$duration=="6m"], use="complete.obs")
}

df_plot <- tibble(timeindex=ti, rho=cors_full_6m) %>%
  left_join(merged_monthly %>% distinct(timeindex, year, month)) %>% filter(year >= 2011)

p_acled_paper <- ggplot(df_plot, aes(timeindex, rho)) + geom_line() + geom_smooth(se=FALSE, color="black") +
  labs(title="Monthly correlation between HMM estimates and ACLED testing data",
       subtitle="Full sample, 6-month lag imputation, 2011-2017", y="Spearman correlation coefficient", x="") +
  theme_light() + coord_cartesian(ylim=c(0,0.6))
ggsave(file.path(plotdir, "cor_acled_paper.png"), width=7, height=4, dpi=500); print(p_acled_paper); browseURL(file.path(plotdir, "cor_acled_paper.png"))

message("¡TODO LISTO! Todas las figuras están en:")
message(plotdir)
