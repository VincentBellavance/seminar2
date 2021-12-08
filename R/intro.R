# Plots for intro

# Plot LPI data for Quebec

lpi <- readRDS("data/lpd_qc.RDS")
qc <- readRDS("data/qc_spacePoly.rds")
qc <- sp::spTransform(qc, "EPSG:4326")

svg("images/quebec/lpi.svg")
par(mar = c(0,0,4,0))
sp::plot(qc, main = "Suivis de populations à long terme \ndisponibles au Québec", cex.main = 1.5)
plot(lpi, pch = 16, col = "red", add = TRUE)
dev.off()
