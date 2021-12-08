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

map_sdm_year <- function(sdm, col = pal()$r, limits_val = c(0,1), show_legend = FALSE, legend_title = "", title) {
  
  rast_df <- sdm |>
  	(\(.) raster::rasterToPoints(.))() |> 
  		(\(.) as.data.frame(.))()

  breaks_val <- ceiling(
                  seq(limits_val[1], 
                      limits_val[2], 
                      length.out = 5) * 100)/100

  p <- ggplot() +
         geom_tile(data = rast_df, 
         			  	 aes(x = x, y = y, 
         			  	 fill = !! sym(names(sdm)))) +
         scale_fill_gradientn(colors=col, 
         										  limits=limits_val, 
         										  breaks=breaks_val, 
         										  name = legend_title, 
         										  guide = ifelse(show_legend == TRUE, "colourbar", "none")) +
         ggtitle(title) +
         theme_map()
  
  return(p)
}

theme_map  <- function() {
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20),
        legend.key.height= unit(1.8, 'cm'),
        legend.key.width= unit(0.3, 'cm'),
        legend.title = element_text(size = 21),
        legend.text = element_text(size = 19)
        )
}

###################
# Import all SDMs
###################

species <- list.dirs("data/maps", full.names = F, recursive = F)

sp_rast <- lapply(species, function(x) {
  raster::stack(paste0("data/maps/", x, "/maps.gri"))
})

names(sp_rast) <- species


#######################
# Dataframe of species
#######################

insectivores <- read.csv2("data/sp_habitats.csv") |> 
                  (\(.) .[!is.na(.$insectivores) & .$species %in% stringr::str_to_sentence(gsub("_", " ", species)), "species"])()

proie <- read.csv2("data/sp_habitats.csv") |> 
                  (\(.) .[!is.na(.$oiseau_proie) & .$species %in% stringr::str_to_sentence(gsub("_", " ", species)), "species"])()

groups <- data.frame(species = species, groups = NA)

groups[stringr::str_to_sentence(gsub("_", " ", groups$species)) %in% insectivores, "groups"] <- "insectivores aériens"
groups[stringr::str_to_sentence(gsub("_", " ", groups$species)) %in% proie, "groups"] <- "oiseaux de proie"

###################
# BDI Québec
###################

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
                     title = "Indice de Distribution Biodiversité") +
       ggplot2::scale_y_continuous(breaks = c(0.5, 1, 1.5, 2), trans = "log", limits = c(0.5,2)) +
       ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1])+1, floor(x[2]-1), by = 5)) + # round to integer, since the x axis is years 
       plot_theme()



ggplot2::ggsave(plot = p, 
                filename = "images/results/quebec.png", 
                device = "png",
                height = 7,
                width = 12)

p <- ggplot2::ggplot() +
       # plot col in blue and ext in orange
       ggplot2::geom_line(data = bdi[-1,],
                          ggplot2::aes(x = years, y = ext, group = 1),
                          col = pal()$ts["ext"],
                          lwd = .7) +
       ggplot2::geom_line(data = bdi_df[-1,],
                      ggplot2::aes(x = years, y = col, group = 1),
                      col = pal()$ts["col"],
                      lwd = .7) +
       # baseline reference
       ggplot2::geom_hline(yintercept = 0,
                           lty = 1,
                           col = "grey20",
                           lwd = .2) +
       ggplot2::labs(y = "Taux de croissance", 
                     x = "Années", 
                     title = "Augmentation et réduction de l'aire de distribution") +
       ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1]+1), floor(x[2]-1), by = 5)) + # round to integer, since the x axis is years 
       ggplot2::ylim(-0.5, 0.5) +
       plot_theme()

ggplot2::ggsave(plot = p, 
                filename = "images/results/quebec_delta.png", 
                device = "png",
                height = 7,
                width = 12)


#######################
# bdi Québec fonctional
#######################

bdi_groups <- calc_bdi_groups(sp_rast, groups)
colnames(bdi_groups)[6] <- "Groupes"

p <- ggplot2::ggplot() +
       # plot all taxa trends in green
       ggplot2::geom_line(data = bdi_groups,
                          ggplot2::aes(x = years, y = index, colour = Groupes),
                          #col = colorRampPalette(RColorBrewer::brewer.pal(10,"RdYlGn"))(length(unique(bdi_df$groups))),
                          lwd = .7) + 
       # baseline reference
       ggplot2::geom_hline(yintercept = 1,
                           lty = 1,
                           col = "grey20",
                           lwd = .2) +
       ggplot2::labs(y = "Valeur de l'indice", 
                     x = "Années", 
                     title = "Indice de Distribution Biodiversité") +
       ggplot2::scale_y_continuous(breaks = c(0.5, 1, 2, 5), trans = "log", limits = c(0.5,5)) +
       ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1])+1, floor(x[2]-1), by = 5)) + # round to integer, since the x axis is years 
       plot_theme()

ggplot2::ggsave(plot = p, 
                filename = "images/results/quebec_groups.png", 
                device = "png",
                height = 7,
                width = 12)


#######################
# Stel truc bdi
#######################

bdi <- calc_bdi_groups(sp_rast[16], NULL)

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
                     title = "Indice de Distribution Biodiversité de Stelgidopteryx serripennis") +
       ggplot2::scale_y_continuous(breaks = c(0.5, 1, 2, 10, 25), trans = "log", limits = c(0.5,25)) +
       ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1])+1, floor(x[2]-1), by = 5)) + # round to integer, since the x axis is years 
       plot_theme()

ggplot2::ggsave(plot = p, 
                filename = "images/results/quebec_stel.png", 
                device = "png",
                height = 7,
                width = 12)


p <- ggplot2::ggplot() +
       # plot all taxa trends in green
       ggplot2::geom_line(data = bdi,
                          ggplot2::aes(x = years, y = index),
                          #col = colorRampPalette(RColorBrewer::brewer.pal(10,"RdYlGn"))(length(unique(bdi_df$groups))),
                          lwd = .7) + 
       ggplot2::geom_point(data = bdi[c(9,26),],
                          ggplot2::aes(x = years, y = index),
                          col = "red",
                          size = 5) +
       # baseline reference
       ggplot2::geom_hline(yintercept = 1,
                           lty = 1,
                           col = "grey20",
                           lwd = .2) +
       ggplot2::labs(y = "Valeur de l'indice", 
                     x = "Années", 
                     title = "Indice de Distribution Biodiversité de Stelgidopteryx serripennis") +
       ggplot2::scale_y_continuous(breaks = c(0.5, 1, 2, 10, 25), trans = "log", limits = c(0.5,25)) +
       ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1])+1, floor(x[2]-1), by = 5)) + # round to integer, since the x axis is years 
       plot_theme()

ggplot2::ggsave(plot = p, 
                filename = "images/results/quebec_stel2.png", 
                device = "png",
                height = 7,
                width = 12)


#######################
# Stel truc map
#######################

sdm2000 <- sp_rast[[16]][["mean_all_2000"]]
sdm2017 <- sp_rast[[16]][["mean_all_2017"]]

map2000 <- map_sdm_year(sdm2000,
                        limits_val = c(0, 1),
                        show_legend = TRUE,
                        legend_title = "p(occ)",
                        title = "Modèle de l'aire de distribution de \nStelgidopteryx serripennis en 2000")

ggplot2::ggsave(plot = map2000,
                filename = "images/results/map2000.png", 
                device = "png",
                height = 9,
                width = 8)


map2017 <- map_sdm_year(sdm2017,
                        limits_val = c(0, 1),
                        show_legend = TRUE,
                        legend_title = "p(occ)",
                        title = "Modèle de l'aire de distribution de \nStelgidopteryx serripennis en 2017")

ggplot2::ggsave(plot = map2017,
                filename = "images/results/map2017.png", 
                device = "png",
                height = 9,
                width = 8)

####################
# bdi Québec species
####################

groups <- data.frame(species = names(sp_rast), groups = names(sp_rast))

bdi_groups <- calc_bdi_groups(sp_rast, groups)

#dev.new(width=12, height=7)
p <- ggplot2::ggplot() +
       # plot all taxa trends in green
       ggplot2::geom_line(data = bdi_groups,
                          ggplot2::aes(x = years, y = index, colour = groups),
                          #col = colorRampPalette(RColorBrewer::brewer.pal(10,"RdYlGn"))(length(unique(bdi_df$groups))),
                          lwd = .7) + 
       # baseline reference
       ggplot2::geom_hline(yintercept = 1,
                           lty = 1,
                           col = "grey20",
                           lwd = .2) +
       ggplot2::labs(y = "Valeur de l'indice", 
                     x = "Années", 
                     title = "Indice de Distribution Biodiversité") +
       ggplot2::scale_y_continuous(breaks = c(0.1, 0.5, 1, 2, 5, 10, 20, 30), trans = "log", limits = c(0.1,30)) +
       ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1])+1, floor(x[2]-1), by = 5)) + # round to integer, since the x axis is years
       ggplot2::scale_colour_manual(values = c("#e6194b", "#3cb44b", "#ffe119","#0082c8","#f58231","#911eb4","#46f0f0","#f032e6","#d2f53c","#fabebe","#008080","#e6beff","#aa6e28","#fffac8","#800000","#aaffc3","#808000","#ffd8b1","#000080")) +
       plot_theme()

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

col <- map_sdm_year(delta_map[["col"]], col = rev(pals::ocean.delta(200)[1:100]), limits_val = c(0, round(max(delta_map[["col"]][,,], na.rm = T) + 0.05, digits = 1)), show_legend = TRUE, legend_title = "\u0394 p(occ)", title = "Augmentation d'aires de distribution")

ggplot2::ggsave(plot = col, 
                filename = "images/results/col.png", 
                device = "png",
                height = 9,
                width = 8)

ext <- map_sdm_year(delta_map[["ext"]], col = rev(pals::ocean.delta(200)[101:200]), limits_val = c(round(min(delta_map[["ext"]][,,], na.rm = T) - 0.05, digits = 1),0), show_legend = TRUE, legend_title = "\u0394 p(occ)", title = "Réduction d'aires de distribution")

ggplot2::ggsave(plot = ext, 
                filename = "images/results/ext.png", 
                device = "png",
                height = 9,
                width = 8)
