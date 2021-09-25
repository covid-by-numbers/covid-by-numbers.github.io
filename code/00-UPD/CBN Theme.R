## Add the required font
library(sysfonts)
library(showtext)

# You may need to change these paths based on your own computer
font_add(family = "Freight Text Pro",
         regular = "/cloud/project/fonts/FreightTextProMedium-Regular.ttf",
         italic = "/cloud/project/fonts/FreightTextProMedium-Italic.ttf",
         bold = "/cloud/project/fonts/FreightTextProBold-Regular.ttf")

## Setting the theme
theme_cbnsite <- theme_bw(base_family = "Freight Text Pro") +
  theme(legend.position = "top",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 20, face = "italic",
                                     margin = margin(b=12)),
        plot.caption = element_text(size = 20,
                                    vjust = -1),
        plot.margin = unit(c(.5,.5,.5,.5), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 20, face = "bold"))
theme_set(theme_cbnsite)

# Now, we ensure we show the text in the graphs
showtext_auto()
