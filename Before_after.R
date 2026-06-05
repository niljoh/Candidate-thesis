library(ecoevosimr)
library(ggplot2)

eesSetup("C:/Users/johan/OneDrive - Linköpings universitet/Genetik/EcoEvoSim-main")

mutgen <- mutantGenerator("weighted", invaderPopsize = 0.001, variance = 0.002^2)
params <- integrationParams(Inf, "DynamicSS()", abstol = 1e-14, reltol = 1e-8)

#run 1 -
model <- lotkaVolterra(
  growthFn = "z -> ((tanh(sum(z .- 0.5) / 0.3) + 1) / 2 - (tanh((0 - 0.5)/0.3) + 1)/2)",
  kernelFn = "(zi, zj) -> -(tanh((zi[1] - zj[1]) / 0.15) + 1) / 2"
)


config <- ecoEvoConfig(model, mutgen, params, extThreshold = 0.003)
init_community <- community(1.0, 0.3) |> ecoDyn(config)
lineage <- evolve(init_community, config, nSteps = 3000, seed = 54321)

#run 2 - sigmoid therapy
model_S <- lotkaVolterra(
  growthFn = "z -> ((tanh(sum(z .- 0.5) / 0.3) + 1) / 2 - (tanh((0 - 0.5)/0.3) + 1)/2)* (1 / (1 + exp(10*(z[1] - 0.4))))",
  kernelFn = "(zi, zj) -> -(tanh((zi[1] - zj[1]) / 0.20) + 1) / 2"
)
config_S <- ecoEvoConfig(model_S, mutgen, params, extThreshold = 0.003)

lineage <- evolve(lineage, config_S, nSteps = 3000, seed = 54321)

#run 2 - threshold therapy
model_T <- lotkaVolterra(
  growthFn = "z -> ((tanh(sum(z .- 0.5) / 0.3) + 1) / 2 - (tanh((0 - 0.5)/0.3) + 1)/2) *(z[1] < 0.4)",
  kernelFn = "(zi, zj) -> -(tanh((zi[1] - zj[1]) / 0.15) + 1) / 2"
)

config_T <- ecoEvoConfig(model_T, mutgen, params, extThreshold = 0.003)

lineage <- evolve(lineage, config_T, nSteps = 3000, seed = 54321)


#
lineage |>
  historyToTibble() |>
  ggplot(aes(x = trait_1, y = mutNo, color = popsize_1)) +
  geom_point(alpha = 0.5) +
  scale_size_area(max_size = 2) +
  labs(x = "Trait value", y = "Mutation event", color = "Pop. size") +
  scale_color_gradient(low = "gray90", high = "navy") +
  theme_bw()
