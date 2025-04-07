library(ggplot2)

# Set seed
set.seed(123)

# Simulate data
n <- 100
x <- runif(n, min = 0, max = 100)
y <- rnorm(n, mean = 50, sd = 10)

# Data frame
df <- data.frame(
  'spec' = x,
  'success' = y
)

# Plot
p <- ggplot(df, aes(x = spec, y = success)) +
  geom_point(aes(color = spec), alpha = 0.7, size = 8) +
  geom_hline(yintercept = 50, color = "black", linetype = "dashed", linewidth=1.5) +
  scale_color_gradient(low = "#FF82AB", high = "#225ea8") +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )

path <- file.path(getwd(), "outputs", "04_outputs_figures")

# Export
ggsave(
  filename = file.path(path, "figure1c.png"),
  plot = p
)
