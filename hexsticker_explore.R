circle_points <- function(center = c(0, 0), diameter = 1, npoints = 360)  {

  r <- diameter / 2
  tt <- seq(0, 2*pi, length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))

}

library(tidyverse)
circle1 <- circle_points(diameter = 1)
circle2 <- circle_points(diameter = 0.5)
line1 <- data.frame(x = c(-0.75, -0.2), y = c (0, 0))
line2 <- data.frame(x = c(0.2, 0.75), y = c (0, 0))
line3 <- data.frame(x = c(0,0), y = c(-0.75, -0.2))
line4 <- data.frame(x = c(0,0), y = c(0.2, 0.75))
text1 <- data.frame(x = 0, y = -0.5, text = "0 0 0 0 0 0 0")
text2 <- data.frame(x = 0, y = 0, text = "0 0 0 0 1 0 0")
text3 <- data.frame(x = 0, y = 0.5, text = "0 0 0 0 0 0 0")

x_shift = 0.5

p <- ggplot() +

  geom_text(data = text1, aes(x, y, label = text), color = "white", size = 18) +
  geom_text(data = text2, aes(x, y, label = text), color = "white", size = 18) +
  geom_text(data = text3, aes(x, y, label = text), color = "white", size = 18) +

  geom_path(data = circle1[6:84, ], aes(x+x_shift,y), color = "red", size = 0.6, alpha = 0.75) +
  geom_path(data = circle1[96:174, ], aes(x+x_shift,y), color = "red", size = 0.6, alpha = 0.75) +
  geom_path(data = circle1[186:264, ], aes(x+x_shift,y), color = "red", size = 0.6, alpha = 0.75) +
  geom_path(data = circle1[276:354, ], aes(x+x_shift,y), color = "red", size = 0.6, alpha = 0.75) +

  geom_path(data = circle2[6:84, ], aes(x+x_shift,y), color = "red", size = 0.5, alpha = 0.75) +
  geom_path(data = circle2[96:174, ], aes(x+x_shift,y), color = "red", size = 0.5, alpha = 0.75) +
  geom_path(data = circle2[186:264, ], aes(x+x_shift,y), color = "red", size = 0.5, alpha = 0.75) +
  geom_path(data = circle2[276:354, ], aes(x+x_shift,y), color = "red", size = 0.5, alpha = 0.75) +

  geom_path(data = line1, aes(x+x_shift, y), color = "red", size = 0.7, alpha = 0.75) +
  geom_path(data = line2, aes(x+x_shift, y), color = "red", size = 0.7, alpha = 0.75) +
  geom_path(data = line3, aes(x+x_shift, y), color = "red", size = 0.7, alpha = 0.75) +
  geom_path(data = line4, aes(x+x_shift, y), color = "red", size = 0.7, alpha = 0.75) +

  xlim(c(-1.5,1.5)) +
  ylim(c(-1,1)) +

  theme_void()

library(hexSticker)
file_out <- "hex_explore.png"
sticker(p,                      # ggplot
        package = "explore",    # name of package
        p_size = 29,            # size package name
        p_color = "darkblue",    # color package name
        p_x = 1, p_y = 0.65,     # x/y package name
        s_x = 1, s_y = 1.17,    # x/y subplot
        s_width = 1.3, s_height = 1,
        h_fill = "lightblue", # "#ECEFF1",
        h_color = "#90A4AE",
        filename = file_out)
