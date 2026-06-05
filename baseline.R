library(ecoevosimr)
library(ggplot2)

# Initialise Julia and load EcoEvoSim
eesSetup("C:/Users/johan/OneDrive - Linköpings universitet/Genetik/EcoEvoSim-main")

# Define the ecological model (functions as Julia code strings)
model <- lotkaVolterra(
  growthFn = "z -> ((tanh(sum(z .- 0.5) / 0.3) + 1) / 2 - (tanh((0 - 0.5)/0.3) + 1)/2)",
  kernelFn = "(zi, zj) -> -(tanh((zi[1] - zj[1]) / 0.15) + 1) / 2"
)
# Mutation generator
mutgen <- mutantGenerator(
  "weighted",
  invaderPopsize = 0.001,
  variance = 0.002^2
)

# Integration parameters
params <- integrationParams(Inf, "DynamicSS()", abstol = 1e-14, reltol = 1e-8)

# Full configuration
config <- ecoEvoConfig(model, mutgen, params, extThreshold = 0.003)

# Create initial community and run to ecological equilibrium
init_community <- community(1.0, 0.3) |> ecoDyn(config)

# Evolve for 1500 mutation events
lineage <- evolve(init_community, config, nSteps =3000, seed = 54321)

# Visualize result
lineage |>
  # Convert to tibble for analysis
  historyToTibble() |>
  # Create plot
  ggplot(aes(x = trait_1, y = mutNo, color = popsize_1)) +
  geom_point(alpha = 0.5) +
  scale_size_area(max_size = 2) +
  labs(x = "Trait value", y = "Mutation event", color = "Pop. size") +
  scale_color_gradient(low = "gray90", high = "navy") +
  theme_bw()

#
#filtered_1 <- filterHistory(lineage, indices = 1:100) |> historyToTibble()

