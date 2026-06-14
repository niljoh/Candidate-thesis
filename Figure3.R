# Load library
library(ggplot2)

# Create trait range
z <- seq(0, 1, length.out = 500)

#Normalization.. so r(z=0) = 0
norm <- (tanh((0 - 0.5)/0.3) + 1)/2

# Define growth functions
# Baseline (no therapy)
g_base <- (tanh((z - 0.5)/0.3) + 1)/2 - norm

# Threshold therapy (kills clones above threshold z = 0.4)
g_cutoff <- g_base * (z < 0.4)

# Sigmoid therapy (smooth but steep threshold around z = 0.4)
g_sigmoid <- g_base * (1 / (1 + exp(10*(z - 0.4))))

df <- data.frame(
  z = rep(z, 3),
  growth = c(g_base, g_cutoff, g_sigmoid),
  type = rep(c("Baseline (No therapy)", "Threshold therapy", "Sigmoid therapy"),
             each = length(z))
)

# Plot
ggplot(df, aes(x = z, y = growth, color = type)) +
  geom_line(linewidth = 1) +
  scale_color_viridis_d(
    option = "D",
    name = "Model",
    labels = c(
      "Baseline (No therapy)",
      "Threshold therapy",
      "Sigmoid therapy"))+
  labs(
    x = "Trait value",
    y = "Growth rate",
  ) +
  theme_bw()
