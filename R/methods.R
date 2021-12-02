# Scripts to make figures for method

# 1. GIF
# 2. Calculate_areas???
# 3. Time-series areas
# 4. Index for one species
# 5. Col and Ext

# 5. Index

devtools::load_all("../rbdi")
library(raster)
library(magick)
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

##################
# Make GIF images
##################

cardinalis <- raster::stack("data/cardinalis.gri")
map_sdm_gif(cardinalis, dir.out = "images/method", "cardinalis_cardinalis", "p(occ)")


##################
# Calculate areas
##################

areas <- data.frame(years = 1992:2018, areas = NA)
areas[,"species"] <- "Cardinalis cardinalis"

for(i in 1:length(areas$years)) {
  areas[i, "areas"] <- sum(cardinalis[[i]][,,], na.rm = T) * 4
}

p <- ggplot2::ggplot() +
     # plot all taxa trends in green
     ggplot2::geom_line(data = areas,
                        ggplot2::aes(x = years, y = areas),
                        lwd = .7) + 
     ggplot2::labs(y = "Aires de distribution (km²)", 
                   x = "Années", 
                   title = "Aires de distribution du Cardinal Rouge de 1992 à 2018") +
     ggplot2::ylim(round(min(areas$areas) - 1000, digits = -3), round(max(areas$areas) + 1000, digits = -3)) +
     ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1])+1, floor(x[2]-1), by = 5)) + # round to integer, since the x axis is years 
     plot_theme()

ggplot2::ggsave(plot = p, 
                filename = "images/method/areas.png", 
                device = "png",
                height = 7,
                width = 12)


##################
# Calculate index
##################

areas[1, "index"] <- 1
areas[2:nrow(areas), "index"] <- areas[2:nrow(areas), "areas"]/areas[1, "areas"]

p <- ggplot2::ggplot() +
     # plot all taxa trends in green
     ggplot2::geom_line(data = areas,
                        ggplot2::aes(x = years, y = index),
                        lwd = .7) +
     ggplot2::geom_hline(yintercept = 1,
                         lty = 1,
                         col = "grey20",
                         lwd = .2) + 
     ggplot2::labs(y = "Indice", 
                   x = "Années", 
                   title = "Indice de Distribution Biodiversité du Cardinal Rouge") +
     ggplot2::scale_y_continuous(breaks = c(0.5, 1, 1.5, 2), trans = "log", limits = c(0.5,2)) +
     ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1])+1, floor(x[2]-1), by = 5)) + # round to integer, since the x axis is years 
     plot_theme()

ggplot2::ggsave(plot = p, 
                filename = "images/method/index_cardinalis.png", 
                device = "png",
                height = 7,
                width = 12)


######################
# For multiple species
######################

dolichonyx <- raster::stack("data/dolichonyx.gri")
antigone <- raster::stack("data/antigone.gri")

areas_antigone <- data.frame(years = 1992:2018, areas = NA)
areas_antigone[,"species"] <- "Antigone canadensis"

areas_dolichonyx <- data.frame(years = 1992:2018, areas = NA)
areas_dolichonyx[,"species"] <- "Dolichonyx oryzivorus"

for(i in 1:length(areas_antigone$years)) {
  areas_antigone[i, "areas"] <- sum(antigone[[i]][,,], na.rm = T) * 4
}

for(i in 1:length(areas_dolichonyx$years)) {
  areas_dolichonyx[i, "areas"] <- sum(dolichonyx[[i]][,,], na.rm = T) * 4
}

areas_antigone[1, "index"] <- 1
areas_antigone[2:nrow(areas_antigone), "index"] <- areas_antigone[2:nrow(areas_antigone), "areas"]/areas_antigone[1, "areas"]

areas_dolichonyx[1, "index"] <- 1
areas_dolichonyx[2:nrow(areas_dolichonyx), "index"] <- areas_dolichonyx[2:nrow(areas_dolichonyx), "areas"]/areas_dolichonyx[1, "areas"]

areas <- rbind(areas, areas_dolichonyx, areas_antigone)

p <- ggplot2::ggplot() +
     # plot all taxa trends in green
     ggplot2::geom_line(data = areas,
                        ggplot2::aes(x = years, y = index, color = species),
                        lwd = .7) +
     ggplot2::geom_hline(yintercept = 1,
                         lty = 1,
                         col = "grey20",
                         lwd = .2) +
     ggplot2::labs(y = "Aires de distribution (km²)", 
                   x = "Années", 
                   title = "Indice de Distribution Biodiversité pour trois espèces") +
     ggplot2::scale_y_continuous(breaks = c(0.5, 1, 1.5, 2, 5, 10, 20), trans = "log", limits = c(0.5,20)) +
     ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1])+1, floor(x[2]-1), by = 5)) + # round to integer, since the x axis is years 
     plot_theme()

ggplot2::ggsave(plot = p, 
                filename = "images/method/split_index.png", 
                device = "png",
                height = 7,
                width = 12)


###################################
# Multiple species composite index
###################################

bdi <- data.frame(years = 1992:2018, bdi = NA)
bdi[1, "bdi"] <- 1

for(i in 1993:2018) {
  mean_dt <- mean(log10(areas[areas$years == i, "index"]))
  bdi[bdi$years == i, "bdi"] <- 10^mean_dt
  rm(mean_dt)
}

p <- ggplot2::ggplot() +
     # plot all taxa trends in green
     ggplot2::geom_line(data = areas,
                        ggplot2::aes(x = years, y = index, group = species),
                        col = "grey40",
                        lwd = .7) + 
     ggplot2::geom_line(data = bdi,
                        ggplot2::aes(x = years, y = bdi),
                        col = "red",
                        lwd = .7) + 
     ggplot2::geom_hline(yintercept = 1,
                         lty = 1,
                         col = "grey20",
                         lwd = .2) +
     ggplot2::labs(y = "Aires de distribution (km²)", 
                   x = "Années", 
                   title = "Indice de Distribution Biodiversité pour trois espèces") +
     ggplot2::scale_y_continuous(breaks = c(0.5, 1, 1.5, 2, 5, 10, 20), trans = "log", limits = c(0.5,20)) +
     ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1])+1, floor(x[2]-1), by = 5)) + # round to integer, since the x axis is years 
     plot_theme()

ggplot2::ggsave(plot = p, 
                filename = "images/method/composite_index.png", 
                device = "png",
                height = 7,
                width = 12)
