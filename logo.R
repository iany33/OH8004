
pacman::p_load(
  hexSticker,
  tidyverse,
  sysfonts,
  magick,
  meta
)

data_logo <- tibble(n.e = c(50, 40, 70, 20, 30, 20),
                    mean.e = c(1, 3, 2, 1.5, 1, 2), 
                    sd.e = c(0.5, 2, 0.5, 0.5, 1, 1),
                    n.c = c(50, 40, 70, 20, 30, 20), 
                    mean.c = c(0.5, 2, 1.2, 1, 1, 1.75),  
                    sd.c = c(0.5, 2, 0.5, 0.5, 1, 0.75),  
                    studlab = rep(1:6, times = 1))

meta_logo <- data_logo |> 
  metacont(n.e = n.e ,
           mean.e = mean.e,
           sd.e = sd.e,
           n.c = n.c,
           mean.c = mean.c,
           sd.c = sd.c, 
           studlab = studlab,
           sm = "MD", 
           random = TRUE,
           fixed = FALSE, 
           method.tau = "REML",
           hakn = TRUE)

png("forest_plot.png", width=5, height=5, units="in", res=200)
meta_logo |> forest.meta(sortvar = TE,
            print.tau2 = FALSE,
            leftcols = FALSE, rightcols = FALSE,
            smlab = "", 
            col.square = "#2a788e",
            col.diamond = "#440154") -> forest_plot
dev.off()

font_data <- font_files()

font_add("Century", "CENTURY.TTF")

sticker(
  subplot = "forest.png",
  package = "OH8004",
  p_color = "#414487",
  s_width = 0.82,
  s_height = 0.82,
  s_x = 1,
  s_y = 0.9,
  p_size = 13,
  p_y = 1.68,
  h_fill = "white",
  h_color = "black",
  h_size = 2,
  spotlight = T,
  l_y = 1.4,
  l_x = 1, 
  l_width = 3,
  l_height = 3,
  l_alpha = 0.4,
  p_family = "sans",
  p_fontface = "bold"
) |> print()

