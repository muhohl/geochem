## Generate synthetic geochemical test datasets
##
## Run this script to regenerate the package test data.
## Source: set.seed(42) for reproducibility.
##
## Produces three datasets saved to data/:
##   whole_rock_data  - major oxides + trace elements for 4 volcanic rock types
##   laser_map_data   - synthetic 50×50 laser ICP-MS element map
##   detection_limit_data - PGE assay table with below-detection values

set.seed(42)

# ── helpers ──────────────────────────────────────────────────────────────────

rpos <- function(n, mean, sd, min = 0) {
  pmax(rnorm(n, mean, sd), min)
}

# ── 1. Whole-rock dataset ─────────────────────────────────────────────────────
# 50 samples across 4 volcanic rock types; major oxides in wt%, traces in ppm.
# Compositions are geologically realistic but fully synthetic.

n_per_type <- c(Basalt = 13, Andesite = 13, Dacite = 12, Rhyolite = 12)
rock_types  <- rep(names(n_per_type), times = n_per_type)
n_total     <- sum(n_per_type)

# Base major-oxide compositions (wt%) per rock type
base_ox <- list(
  Basalt    = c(SiO2=49.0, TiO2=1.50, Al2O3=15.0, Fe2O3=3.0, FeO=7.0,
                MnO=0.18,  MgO=9.0,   CaO=10.0,   Na2O=2.8,  K2O=0.80,
                P2O5=0.20),
  Andesite  = c(SiO2=57.0, TiO2=1.00, Al2O3=17.0, Fe2O3=2.5, FeO=5.0,
                MnO=0.15,  MgO=4.0,   CaO=7.0,    Na2O=3.5,  K2O=1.80,
                P2O5=0.25),
  Dacite    = c(SiO2=65.0, TiO2=0.60, Al2O3=17.0, Fe2O3=2.0, FeO=2.8,
                MnO=0.10,  MgO=2.0,   CaO=4.5,    Na2O=4.0,  K2O=2.50,
                P2O5=0.12),
  Rhyolite  = c(SiO2=73.0, TiO2=0.20, Al2O3=13.5, Fe2O3=1.0, FeO=1.0,
                MnO=0.06,  MgO=0.4,   CaO=1.2,    Na2O=3.8,  K2O=4.50,
                P2O5=0.05)
)

# Standard deviations for each oxide (same for all types)
sd_ox <- c(SiO2=1.5, TiO2=0.15, Al2O3=0.8, Fe2O3=0.4, FeO=0.6,
           MnO=0.02, MgO=0.8, CaO=0.7, Na2O=0.3, K2O=0.3, P2O5=0.04)

oxide_mat <- do.call(rbind, lapply(seq_along(rock_types), function(i) {
  type <- rock_types[i]
  b    <- base_ox[[type]]
  sapply(names(b), function(ox) rpos(1, b[ox], sd_ox[ox], min = 0.01))
}))
colnames(oxide_mat) <- names(sd_ox)

# Base trace-element compositions (ppm) per rock type
base_tr <- list(
  Basalt   = c(Ni=130, Cu=80,  Zn=85,  Rb=18,  Sr=350, Y=25,
               Zr=100, Nb=5,   Ba=180, La=12,  Ce=28,  Nd=15,
               Sm=4.0, Eu=1.4, Gd=4.5, Yb=2.0, Lu=0.30),
  Andesite = c(Ni=40,  Cu=55,  Zn=80,  Rb=55,  Sr=450, Y=22,
               Zr=160, Nb=8,   Ba=450, La=22,  Ce=50,  Nd=24,
               Sm=5.0, Eu=1.5, Gd=4.8, Yb=2.2, Lu=0.32),
  Dacite   = c(Ni=15,  Cu=30,  Zn=70,  Rb=110, Sr=320, Y=20,
               Zr=250, Nb=12,  Ba=750, La=35,  Ce=75,  Nd=32,
               Sm=6.0, Eu=1.4, Gd=5.0, Yb=2.4, Lu=0.36),
  Rhyolite = c(Ni=5,   Cu=10,  Zn=55,  Rb=200, Sr=100, Y=35,
               Zr=320, Nb=18,  Ba=900, La=45,  Ce=95,  Nd=38,
               Sm=7.5, Eu=0.9, Gd=6.5, Yb=4.0, Lu=0.60)
)

sd_tr <- c(Ni=20, Cu=15, Zn=12, Rb=10, Sr=40, Y=4, Zr=25, Nb=2,
           Ba=80, La=5, Ce=10, Nd=5, Sm=0.8, Eu=0.2, Gd=0.8, Yb=0.5, Lu=0.06)

trace_mat <- do.call(rbind, lapply(seq_along(rock_types), function(i) {
  type <- rock_types[i]
  b    <- base_tr[[type]]
  sapply(names(b), function(el) rpos(1, b[el], sd_tr[el], min = 0.1))
}))
colnames(trace_mat) <- names(sd_tr)

whole_rock_data <- data.frame(
  Sample_ID = sprintf("WR-%03d", seq_len(n_total)),
  rock_type  = rock_types,
  oxide_mat,
  trace_mat,
  stringsAsFactors = FALSE
)

# ── 2. Laser map dataset ─────────────────────────────────────────────────────
# Synthetic 50×50 point laser ICP-MS map with spatial clustering of ore minerals.
# Elements in ppm; Au and Cu follow a log-normal ore zone near the centre.

grid_n  <- 50
xs      <- seq(0, 200, length.out = grid_n)
ys      <- seq(0, 200, length.out = grid_n)
grid    <- expand.grid(x = xs, y = ys)

# Background element concentrations (log-normal)
set.seed(42)
ore_mask <- with(grid, ((x - 100)^2 + (y - 100)^2) < 1500)   # circular ore zone

laser_map_data <- data.frame(
  x = grid$x,
  y = grid$y,
  Fe_ppm  = exp(rnorm(nrow(grid), log(5000),  0.5)) * ifelse(ore_mask, 5, 1),
  Cu_ppm  = exp(rnorm(nrow(grid), log(20),    0.8)) * ifelse(ore_mask, 50, 1),
  Zn_ppm  = exp(rnorm(nrow(grid), log(40),    0.6)) * ifelse(ore_mask, 8,  1),
  Pb_ppm  = exp(rnorm(nrow(grid), log(15),    0.7)) * ifelse(ore_mask, 4,  1),
  As_ppm  = exp(rnorm(nrow(grid), log(10),    0.9)) * ifelse(ore_mask, 12, 1),
  S_ppm   = exp(rnorm(nrow(grid), log(500),   0.4)) * ifelse(ore_mask, 20, 1),
  Au_ppb  = exp(rnorm(nrow(grid), log(0.05),  1.2)) * ifelse(ore_mask, 200, 1),
  Bi_ppm  = exp(rnorm(nrow(grid), log(0.5),   1.0)) * ifelse(ore_mask, 15, 1)
)

# ── 3. Detection-limit dataset ────────────────────────────────────────────────
# PGE / noble-metal assay table with some values below detection limit,
# represented as strings like "<0.1". Use with Random_Number_Imputer().

n_dl <- 40

make_bdl_col <- function(n, true_vals, detection_limit, fraction_bdl = 0.4) {
  bdl_idx <- sample(seq_len(n), size = round(n * fraction_bdl))
  result  <- as.character(round(true_vals, 3))
  result[bdl_idx] <- paste0("<", detection_limit)
  result
}

Au_true <- exp(rnorm(n_dl, log(0.08), 1.0))
Pd_true <- exp(rnorm(n_dl, log(0.15), 1.2))
Pt_true <- exp(rnorm(n_dl, log(0.20), 1.1))
Ag_true <- exp(rnorm(n_dl, log(2.5),  0.8))

detection_limit_data <- data.frame(
  Sample_ID = sprintf("DL-%03d", seq_len(n_dl)),
  Au_ppb    = make_bdl_col(n_dl, Au_true, 0.05,  fraction_bdl = 0.45),
  Pd_ppb    = make_bdl_col(n_dl, Pd_true, 0.10,  fraction_bdl = 0.40),
  Pt_ppb    = make_bdl_col(n_dl, Pt_true, 0.10,  fraction_bdl = 0.35),
  Ag_ppb    = make_bdl_col(n_dl, Ag_true, 0.50,  fraction_bdl = 0.25),
  stringsAsFactors = FALSE
)

# ── Save ──────────────────────────────────────────────────────────────────────

save(whole_rock_data,      file = "data/whole_rock_data.rda",      compress = "xz")
save(laser_map_data,       file = "data/laser_map_data.rda",       compress = "xz")
save(detection_limit_data, file = "data/detection_limit_data.rda", compress = "xz")

message("Test datasets saved to data/")
