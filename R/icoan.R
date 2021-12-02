# Code pour figure rapports ICOAN

# Functions for plot_theme
plot_theme <- function() {
  ggplot2::theme_classic() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 14),
                   axis.title = ggplot2::element_text(size = 17),
                   plot.title = ggplot2::element_text(size = 20, hjust = 0.5),
                   plot.margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"))
}

# List of qc species
qc_sp <- read.csv2("data/list_sp_qc.csv") |> 
	(\(.) .[.$status == "Nicheur", "species"])()

# List of insectivores
insectivores <- read.csv2("data/sp_habitats.csv") |> 
	(\(.) .[!is.na(.$insectivores) & .$species %in% qc_sp & .$species != "Polioptila caerulea", "species"])()

# Import ICOAN data
icoan <- read.csv2("data/icoan.csv")

# Plot insectivores species individually from 1992 to 2016
icoan <- icoan[icoan$"Scientific.Name_Nom.scientifique" %in% insectivores, ]
icoan_norm <- data.frame(species = sort(rep(insectivores, length(1992:2016))), year = rep(1992:2016,length(insectivores)), index = NA)

# Normalize to have first year (1992) = 1
for(i in insectivores) {
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


# Plot
  ggplot2::ggplot() +
    # plot col in blue and ext in orange
    ggplot2::geom_line(data = icoan_norm,
                       ggplot2::aes(x = year, y = index, col = species),
                       lwd = .7) +
    # baseline reference
    ggplot2::geom_hline(yintercept = 1,
                        lty = 1,
                        col = "grey20",
                        lwd = .2) +
    ggplot2::labs(y = "Index", 
                  x = "Years", 
                  title = "ICOAN") +
    ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1]+1), floor(x[2]-1), by = 5)) + # round to integer, since the x axis is years 
    ggplot2::ylim(0, 1.2) +
    plot_theme()
