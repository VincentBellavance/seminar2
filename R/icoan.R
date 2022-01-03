# Code pour figure rapports ICOAN

devtools::install_github("VincentBellavance/rbdi")
library(raster)
library(ggplot2)

# Functions for plot_theme
plot_theme <- function() {
  ggplot2::theme_classic() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 14),
                   axis.title = ggplot2::element_text(size = 17),
                   plot.title = ggplot2::element_text(size = 20, hjust = 0.5),
                   plot.margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
                   legend.key.height= unit(1.8, 'cm'),
                   legend.key.width= unit(0.3, 'cm'),
                   legend.title = element_text(size = 19),
                   legend.text = element_text(size = 17))

}

# List of qc species
qc_sp <- read.csv2("data/list_sp_qc.csv") |> 
	(\(.) .[.$status == "Nicheur", "species"])()

# List of insectivores
proie <- c("Accipiter cooperii",
           "Accipiter gentilis",
           "Buteo jamaicensis",
           "Cathartes aura",
           "Falco peregrinus",
           "Haliaeetus leucocephalus")

# Import ICOAN data
icoan <- read.csv2("data/icoan.csv")

# Plot proie species individually from 1992 to 2016
icoan <- icoan[icoan$"Scientific.Name_Nom.scientifique" %in% proie, ]
icoan_norm <- data.frame(species = sort(rep(proie, length(1992:2016))), year = rep(1992:2016,length(proie)), index = NA)

# Normalize to have first year (1992) = 1
for(i in proie) {
  for(j in 1992:2016) {
    if(j == 1992) {
      icoan_norm[icoan_norm$species == i & icoan_norm$year == j, "index"] <- 1
    } else {
      icoan_norm[icoan_norm$species == i & icoan_norm$year == j, "index"] <- 
        icoan[icoan$Scientific.Name_Nom.scientifique == i & icoan$Year_Année == j, "Annual.Index_Indice.annuel"]/
          icoan[icoan$Scientific.Name_Nom.scientifique == i & icoan$Year_Année == 1992, "Annual.Index_Indice.annuel"]
    }
  }
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

groups <- data.frame(species = names(sp_rast), groups = names(sp_rast))

#######################
# bdi Québec fonctional
#######################

bdi_groups <- calc_bdi_groups(sp_rast, groups)
bdi_groups <- bdi_groups[bdi_groups$groups %in% tolower(gsub(" ", "_", proie)), ]
bdi_groups$groups <- stringr::str_to_sentence(gsub("_", " ", bdi_groups$groups))
bdi_groups <- bdi_groups[bdi_groups$years <= 2016,]

colors <- c("ICOAN" = "black", "Indice Distribution \nBiodiversité" = "red")

for (i in unique(bdi_groups$groups)) {
  # Plot
  p <- ggplot2::ggplot() +
        # plot col in blue and ext in orange
        ggplot2::geom_line(data = icoan_norm[icoan_norm$species == i, ],
                           ggplot2::aes(x = year, y = index, color = "ICOAN"),
                           lwd = .7) +
        ggplot2::geom_line(data = bdi_groups[bdi_groups$groups == i, ],
                           ggplot2::aes(x = years, y = index, color = "Indice Distribution \nBiodiversité"),
                           lwd = .7) +                           
        # baseline reference
        ggplot2::geom_hline(yintercept = 1,
                            lty = 1,
                            col = "grey20",
                            lwd = .2) +
        ggplot2::labs(y = "Valeur de l'indice", 
                      x = "Années", 
                      title = paste0("Comparaison de l'état de ", i, " selon l'ICOAN et \nl'Indicateur de Distribution de la Biodiversité"),
                      color = "Indicateur") +
        ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1]+1), floor(x[2]-1), by = 5)) + # round to integer, since the x axis is years 
        ggplot2::scale_y_continuous(breaks = c(0.5, 1, 2, 5), trans = "log", limits = c(0.5,6.5)) +
        scale_color_manual(values = colors) +
        plot_theme()
  
  ggplot2::ggsave(plot = p, 
                  filename = paste0("images/results/icoan_",i,".png"), 
                  device = "png",
                  height = 7,
                  width = 12)

}

