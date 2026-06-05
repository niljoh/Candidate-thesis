library(ggplot2)

# Create trait range
z <- seq(0, 1, length.out = 500)

# Define sigmoid therapy function
b <- (tanh((z - 0.5)/0.3) + 1)/2 - 0.006692851

# Create dataframe
df <- data.frame(
  z = z,
  therapy = b
)

# Plot
ggplot(df, aes(x = z, y = therapy)) +
  geom_line(linewidth = 1.2, color = "grey46") +
  labs(
    x = "Trait value (z)",
    y = "Therapy effect b(z)") +
  theme_bw()



