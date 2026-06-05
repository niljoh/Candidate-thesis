library(ggplot2)

# Trait distance range
dz <- seq(-1, 1, length.out = 500)

# Define different competition widths
sigma_values <- c(0.1, 0.4, 0.7)

# Create table
df <- data.frame()

for (s in sigma_values) {
  a <- -(tanh((dz)/s) + 1)/2

  t <- data.frame(
    trait_distance = dz,
    competition = a,
    sigma = paste0("σ = ", s)
  )

  df <- rbind(df, t)
}

# Plot
ggplot(df, aes(x = trait_distance, y = competition, color = sigma)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c(
    "σ = 0.1" = "plum",
    "σ = 0.4" = "grey46",
    "σ = 0.7" = "plum4")) +

  labs(x = expression(z[i] - z[j]), y = expression(a(z[i], z[j])), color = expression(sigma)) +
  theme_bw()


