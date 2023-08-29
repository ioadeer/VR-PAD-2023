
dimensions.raw  <- read.csv('./analisis-pad-main/data/dimensiones_de_sala_visual_1_32_sin_outliers.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

for (i in 1: 1:nrow(dimensions.raw)) {
 # obj = objs$shapes[[i]];
  width = dimensions.raw$SG_RV_width[i]
  depth = dimensions.raw$SG_RV_depth[i]
  height = dimensions.raw$SG_RV_height[i]
  
  if (i==1) {
    p = plot_ly(type = "mesh3d",
                x = c(0, 0, depth, depth, 0, 0, depth, depth),
                y = c(0, width, width, 0, 0, width, width, 0),
                z = c(0, 0, 0, 0, height, height, height, height),
                i = c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2),
                j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
                k = c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6),
                opacity = 0.1,
                color = I("red"),
                intensity = 10
    )
  } else {
    p = add_mesh(p = p,
                 x = c(0, 0, depth, depth, 0, 0, depth, depth),
                 y = c(0, width, width, 0, 0, width, width, 0),
                 z = c(0, 0, 0, 0, height, height, height, height),
                 i = c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2),
                 j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
                 k = c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6),
                 opacity = 0.1,
                 color = I("red"),
                 intensity = 10
    )
  }
}

p

for (i in 1:nrow(dimensions.raw)) {
  print(i)
}

dimensions.raw$SG_RV_width[1]
