library(ggplot2)

# Define the response function
D <- function(x, y) {
  -58.99 + 3.22 * x + 0.25 * y
}

# Define the ranges of x and y
x <- seq(0, 28, length.out = 200)
y <- seq(0, 100, length.out = 200)

# Create grid of x and y combinations
df <- expand.grid(
  x = x,
  y = y
)


# Calculate D for every combination
df$D <- D(df$x, df$y)

# Define specific D bins
D_breaks <- seq(50, 400, by = 25)

#filled countour plot

ggplot(df, aes(x = x, y = y, z = D)) +
  geom_contour_filled(
  ) +
  theme_classic() +
  labs(
    x = "mean weekly temp (C)",
    y = "total rainfall (mm)",
    fill = "D",
    title = "Response surface for Darvas 2 model",
    subtitle = "D = -58.99 + 3.22x + 0.25y"
  )

library(plotly)

# Convert D to matrix for plotly
z <- matrix(
  df$D,
  nrow = length(x),
  ncol = length(y)
)


# Horizontal D = 0 plane
z_zero <- matrix(
  0,
  nrow = length(x),
  ncol = length(y)
)


# 3-D response surface
plot_ly(
  x = x,
  y = y,
  z = z,
  type = "surface"
) %>%
  layout(
    title = "3-D Response Surface",
    scene = list(
      xaxis = list(title = "mean weekly temp (C)"),
      yaxis = list(title = "total rainfall (mm)"),
      zaxis = list(title = "Z")
    )
  )
