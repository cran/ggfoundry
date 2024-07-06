## ----options, include = FALSE-------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)

## ----sets, message=FALSE, warning=FALSE, fig.height=7-------------------------
library(ggfoundry)
library(dplyr)
library(forcats)
library(stringr)

df <- shapes_cast() |> 
  filter(!str_ends(shape, "3|4|5|6")) |> 
  mutate(x = row_number(), shape = fct_inorder(shape), .by = set)

df |> 
  ggplot(aes(x, set)) +
  geom_text(aes(label = shape), nudge_y = -0.5, colour = "grey70", size = 3) +
  geom_casting(aes(shape = shape), size = 0.19, fill = "skyblue") +
  scale_shape_manual(values = as.character(df$shape)) +
  scale_x_continuous(expand = expansion(add = 0.5)) +
  scale_y_discrete(expand = expansion(add = 0.7)) +
  labs(x = NULL, y = NULL, caption = "sunflowers 1-8 available") +
  theme_minimal() +
  theme(
    text = element_text(colour = "grey70"),
    axis.text.y = element_text(angle = 90, hjust = 0.5),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
    )

## -----------------------------------------------------------------------------
# Toy Data
set.seed(123)

random_walk <- \(x, y, z) cumsum(rnorm(x, mean = y, sd = sqrt(z)))

df <- data.frame(
  x = rep(1:10, 2),
  y = c(
    random_walk(10, 1, 1),
    random_walk(10, 3, 1.3)
  ),
  group = factor(c(rep(1, 10), rep(2, 10)))
)

# Plot with geom_casting()
df |>
  ggplot(aes(x, y, shape = group, colour = group, fill = group)) +
  geom_line(show.legend = FALSE) +
  geom_casting() +
  scale_colour_manual(values = c("darkred", "darkgreen")) +
  scale_fill_manual(values = c("pink", "lightgreen")) +
  scale_shape_manual(values = c("cross1", "cross2")) +
  labs(title = "ggfoundry") +
  theme_bw() +
  theme(plot.subtitle = element_text(size = 10))

