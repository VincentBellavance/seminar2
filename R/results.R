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

sp_rast <- readRDS("data/sp_rast.rds")

bdi <- calc_bdi_groups(sp_rast, NULL)

p <- ggplot2::ggplot() +
       # plot all taxa trends in green
       ggplot2::geom_line(data = bdi,
                          ggplot2::aes(x = years, y = index),
                          #col = colorRampPalette(RColorBrewer::brewer.pal(10,"RdYlGn"))(length(unique(bdi_df$groups))),
                          lwd = .7) + 
       # baseline reference
       ggplot2::geom_hline(yintercept = 1,
                           lty = 1,
                           col = "grey20",
                           lwd = .2) +
       ggplot2::labs(y = "Valeur de l'indice", 
                     x = "Années", 
                     title = "Biodiversity Distribution Index") +
       ggplot2::scale_y_continuous(breaks = c(0.5, 1, 1.5, 2), trans = "log", limits = c(0.5,2)) +
       ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1])+1, floor(x[2]-1), by = 5)) + # round to integer, since the x axis is years 
       plot_theme()



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


####################
# bdi Québec species
####################

groups <- data.frame(species = names(sp_rast), groups = names(sp_rast))

bdi_groups <- calc_bdi_groups(sp_rast, groups)

p <- plot_bdi(bdi_groups)

ggplot2::ggsave(plot = p, 
                filename = "images/results/quebec_species.png", 
                device = "png",
                height = 7,
                width = 12)

###################
# bdi Québec spat
###################

delta_map <- calc_delta_map(sp_rast,c(1992, 2018))

col <- map_sdm_year(delta_map[["col"]], col = rev(pals::ocean.delta(200)[1:100]), limits_val = c(0, round(max(delta_map[["col"]][,,], na.rm = T) + 0.05, digits = 1)), show_legend = TRUE, legend_title = "\u0394 p(occ)")

ggplot2::ggsave(plot = col, 
                filename = "images/results/col.png", 
                device = "png",
                height = 9,
                width = 8)

ext <- map_sdm_year(delta_map[["ext"]], col = rev(pals::ocean.delta(200)[101:200]), limits_val = c(round(min(delta_map[["ext"]][,,], na.rm = T) - 0.05, digits = 1),0), show_legend = TRUE, legend_title = "\u0394 p(occ)")

ggplot2::ggsave(plot = ext, 
                filename = "images/results/ext.png", 
                device = "png",
                height = 9,
                width = 8)


############
# bdi local
############
