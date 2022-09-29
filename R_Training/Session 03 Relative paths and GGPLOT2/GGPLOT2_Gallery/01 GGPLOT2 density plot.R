# 01 GGPLOT2 Density plot.R

# 103 Enhanced scatter plot with density plots
library(ggside)
library(tidyverse)
library(tidyquant)
# Loaded mpg data that comes pre-installed with ggplot2
mpg

# Enhanced 
# We include these new features
# scale_colour_tq() + scale_fill_tq() + theme_tq()+
# labs (title = "Fuel Economy by Vehicle Type"),subtitle = "ggside density", x = "highway MPG", y = "City MPG") +
# theme (ggside.panel.scale.x = 0.4, ggside.panel.scale.y = 0.4)

# Colour pallete used from tidyquant package
library(tidyquant)

density_plot_enhanced <- mpg %>% 
  ggplot(aes(hwy, cty, color = class)) +
  ggtitle("Standard ggplot2 scatterplot") +
  geom_point(size = 2, alpha = 0.3)   +
  # Adding X axis density plot
  geom_xsidedensity(
    aes(y = after_stat(density),fill = class),
    alpha = 0.5, size = 1,
    position = "stack")  +
  # Adding Y axis density plot
  geom_ysidedensity(
    aes(x = after_stat(density),fill = class),
    alpha = 0.5, size = 1,
    position = "stack")  +
  #  colour palette, axis labs and theme
  scale_colour_tq() + 
  scale_fill_tq() + 
  theme_tq()+
  # Add axis labels
  labs (title = "Fuel Economy by Vehicle Type",subtitle = "ggside density", x = "highway MPG", y = "City MPG") +
  # Add theme
  theme (ggside.panel.scale.x = 0.4, ggside.panel.scale.y = 0.4)
density_plot_enhanced

ggsave(paste0("Enhanced density plot",".jpeg"),width = 30, height = 20, dpi = 150, units = "cm")


# Add regression lines
geom_smooth(method=lm)


density_plot_regressionl <- mpg %>% 
                            ggplot(aes(hwy, cty, color = class)) +
                            ggtitle("Standard ggplot2 scatterplot") +
                            geom_point(size = 2, alpha = 0.3)   +
                            # Adding X axis density plot
                            geom_xsidedensity(
                              aes(y = after_stat(density),fill = class),
                              alpha = 0.5, size = 1,
                              position = "stack")  +
                            # Adding Y axis density plot
                            geom_ysidedensity(
                              aes(x = after_stat(density),fill = class),
                              alpha = 0.5, size = 1,
                              position = "stack")  +
                            #  colour palette, axis labs and theme
                            scale_colour_tq() + 
                            scale_fill_tq() + 
                            theme_tq()+
                            # Add axis labels
                            labs (title = "Fuel Economy by Vehicle Type",subtitle = "ggside density", x = "highway MPG", y = "City MPG") +
                            # Add theme
                            theme (ggside.panel.scale.x = 0.4, ggside.panel.scale.y = 0.4) +
                            geom_smooth(method=lm)
density_plot_regressionl

ggsave(paste0("Enhanced density plot regression line",".jpeg"),width = 30, height = 20, dpi = 150, units = "cm")