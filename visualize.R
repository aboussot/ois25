# visualize.R

# Charger les packages nécessaires
if (!require("plotly")) install.packages("plotly", dependencies = TRUE)
if (!require("MASS")) install.packages("MASS", dependencies = TRUE)

library(plotly)
library(MASS)

# Générer des données aléatoires en 3D (distribution normale)
set.seed(42)
n <- 1000
data <- mvrnorm(n, mu = c(0, 0, 0), Sigma = matrix(c(1,0.6,0.4, 0.6,1,0.5, 0.4,0.5,1), nrow=3))

# Estimer la densité locale (en 3D, ici on fait une approximation via 2D + Z)
dens2d <- kde2d(data[,1], data[,2], n = 100)
densities <- approx2D(dens2d, data[,1], data[,2])

# Fonction pour approximer une densité 2D à des points spécifiques
approx2D <- function(dens, x, y) {
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ix[ix < 1 | ix >= length(dens$x)] <- 1
  iy[iy < 1 | iy >= length(dens$y)] <- 1
  dens$z[cbind(ix, iy)]
}

# Tracer avec plotly en 3D
fig <- plot_ly(
  x = data[,1],
  y = data[,2],
  z = data[,3],
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = 3,
    color = densities,
    colorscale = "Viridis",
    colorbar = list(title = "Densité"),
    opacity = 0.8
  )
)

fig <- fig %>% layout(title = "Nuage de points 3D avec densité locale")

# Afficher le graphique dans le navigateur
fig
