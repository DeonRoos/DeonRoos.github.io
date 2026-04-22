library(NLMR)
library(landscapetools)
library(ggplot2)
library(gganimate)


# Habitat -----------------------------------------------------------------
habitat <- NLMR::nlm_fbm(ncol = 100, nrow = 100, fract_dim = 0.6, user_seed = 1988)
habitat_df <- raster::as.data.frame(habitat, xy = TRUE)
colnames(habitat_df) <- c("x", "y", "z")
ggplot(habitat_df) +
  geom_tile(aes(x = x, y = y, fill = z))
habitat_df$t <- NULL
df <- merge(habitat_df, data.frame(t = 0:100), all = TRUE)

# Wave parameters ---------------------------------------------------------
x0 <- 25
y0 <- 60
b0 <- 0.4
b1 <- 5

x1 <- 40
y1 <- 15
a0 <- 0.3
a1 <- 6

x2 <- 78
y2 <- 48
g0 <- 0.4
g1 <- 4

# Wave variables ----------------------------------------------------------
df$dist0 <- -sqrt((x0 - df$x)^2 + (y0 - df$y)^2)
df$zeta0 <- b0 + b1 * df$z
df$rho0 <- df$t + (1/df$zeta0) * df$dist0

df$dist1 <- -sqrt((x1 - df$x)^2 + (y1 - df$y)^2)
df$zeta1 <- a0 + a1 * df$z
df$rho1 <- df$t + (1/df$zeta1) * df$dist1

df$dist2 <- -sqrt((x2 - df$x)^2 + (y2 - df$y)^2)
df$zeta2 <- g0 + g1 * df$z
df$rho2 <- df$t + (1/df$zeta2) * df$dist2


# Mean --------------------------------------------------------------------
df$mu <- (2 + sin(df$rho0)) * (-2 + sin(df$rho0) + 4) + 
  (2 + sin(df$rho1 + pi)) * (-2 + sin(df$rho1 + 2 * pi)) + 
  (1.5 + sin(df$rho2 + (2*pi))) * (sin(0.7 * df$rho2))


# Animation ---------------------------------------------------------------
anim <- ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = mu), show.legend = FALSE) +
  scale_fill_gradient(
    low = "#202123",
    high = "#00A68A",
    name = NULL
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#202123", color = NA),
    panel.background = element_rect(fill = "#202123", color = NA)
  ) +
  transition_time(t)

animate(anim, renderer = av_renderer("DeonRoos/media/mu_wave.mp4"), width = 600, height = 600, fps = 15)
