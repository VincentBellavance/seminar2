# Script to import maps in output/maps directory

species_dir <- list.dirs("data/maps", recursive = F)
species <- list.dirs("data/maps", recursive = F, full.names = F)

sp_rast <- lapply(species_dir, function(x) {
  raster::stack(paste0(x, "/maps.gri"))
})

names(sp_rast) <- species

saveRDS(sp_rast, "data/sp_rast.rds")