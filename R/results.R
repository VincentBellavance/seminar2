# Scripts to make figures for results

# Calculer le BDI pour chaque cellule chaque année
# Résultat overall (col + ext)
# Résultat overall spatial (col + ext)
# Résultat local (time-series et spatial)

# Résultat par groupe (Proie et Insectivores)
# Comparaison avec ICOAN

devtools::load_all("../rbdi")
library(raster)
library(ggplot2)


plot_theme <- function() {
  ggplot2::theme_classic() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 16),
                   axis.title = ggplot2::element_text(size = 19),
                   plot.title = ggplot2::element_text(size = 20, hjust = 0.5),
                   plot.margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
                   legend.title = element_text(size = 19),
                   legend.text = element_text(size = 15),
                   legend.key.width = unit(2, "cm"),
                   legend.key.height = unit(0.3, "cm")
)
}


###################
# bdi Québec
###################

sp_rast <- readRDS("data/sp_rast")

bdi <- calc_bdi_groups(sp_rast, NULL)

p <- plot_bdi(bdi)

ggplot2::ggsave(plot = p, 
                filename = "images/results/quebec.png", 
                device = "png",
                height = 7,
                width = 12)

p <- plot_delta(bdi)

ggplot2::ggsave(plot = p, 
                filename = "images/results/quebec_delta.png", 
                device = "png",
                height = 7,
                width = 12)


###################
# bdi Québec spat
###################

sp_rast[[]]