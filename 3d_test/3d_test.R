install.packages("plotly")

library(plotly)


# de documentacion --------------------------------------------------------


fig <- plot_ly(type = 'mesh3d',
               x = c(0, 0, 1, 1, 0, 0, 1, 1),
               y = c(0, 1, 1, 0, 0, 1, 1, 0),
               z = c(0, 0, 0, 0, 1, 1, 1, 1),
               i = c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2),
               j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
               k = c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6),
               intensity = seq(0, 1, length = 8),
               color = seq(0, 1, length = 8),
               colors = colorRamp(rainbow(8))
)


fig



# ejemplo internet --------------------------------------------------------

library(plotly)
library(readobj)

install.packages("readobj")

objs <- read.obj("./3d_test/cube.obj")

for (i in range(5)) {
}

for (i in 1:length(objs$shapes)) {
  obj = objs$shapes[[i]];
  if (i==1) {
    p = plot_ly(type = "mesh3d",
                x = obj$positions[1,],
                y = obj$positions[2,],
                z = obj$positions[3,],
                i = obj$indices[1,],
                j = obj$indices[2,],
                k = obj$indices[3,])
  } else {
    p = add_mesh(p = p,
                 x = obj$positions[1,],
                 y = obj$positions[2,],
                 z = obj$positions[3,],
                 i = obj$indices[1,],
                 j = obj$indices[2,],
                 k = obj$indices[3,])
  }
}
p

p = add_mesh(p = p,
             x = obj$positions[1,],
             y = obj$positions[2,],
             z = obj$positions[3,],
             i = obj$indices[1,]/10,
             j = obj$indices[2,]/10,
             k = obj$indices[3,]/10,
             #intensity = seq(0, 1, length = 8),
             color = seq(0, 1, length = 8),
             colors = colorRamp(rainbow(8)))
p             



# otro intento ------------------------------------------------------------

p = none
p = plot_ly(type = 'mesh3d',
               # positions
               x = c(0.2, 0.2, 0.45, 0.45, 0.2, 0.2, 0.45, 0.45),
               y = c(0, 0.25, 0.25, 0, 0, 0.25, 0.25, 0),
               z = c(0, 0, 0, 0, 
                     0.25, 
                     0.25, 
                     0.25, 
                     0.25),
               # figuras?
               i = c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2),
               j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
               k = c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6),
               
               # intensity = seq(0, 1, length = 8),
                color = seq(0, 1, length = 8),)
               # colors = colorRamp(rainbow(8))



p = add_mesh(p = p,
                x = c(0.25, 0.25, 0.75, 0.75, 0.25, 0.25, 0.75, 0.75),
                y = c(0.15, 0.65, 0.65, 0.15, 0.15, 0.65, 0.65, 0.15),
                z = c(0, 0, 0, 0, 
                      0.25, 
                      0.25, 
                      0.25, 
                      0.25),
                i = c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2),
                j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
                k = c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6),
                intensity = c(0, 0.33, 0.66, 1),
                color = c(0, 1, 1, 1),
                colors = colorRamp(c("red", "green", "blue")))
                #intensity = seq(0, 1, length = 8),
                #color = 'seq(0, 1, length = 8)',
                #colors = colorRamp(rainbow(8)))


p = add_mesh(p = p,
             x = c(0.25, 0.25, 0.5, 0.5, 0.25, 0.25, 0.5, 0.5),
             y = c(0, 0.25, 0.25, 0, 0, 0.25, 0.25, 0),
             z = c(0, 0, 0, 0, 
                   0.25, 
                   0.25, 
                   0.25, 
                   0.25),
             i = c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2),
             j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
             k = c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6),
             #intensity = c(0, 0.33, 0.66, 1),
             color = c(0, 255, 255, 0.8))
             #colors = colorRamp(c("red", "green", "blue")))

p





library(plotly)


# first -------------------------------------------------------------------


p = plot_ly(type = 'mesh3d',
               x = c(0, 0, 1, 1, 0, 0, 1, 1),
               y = c(0, 1, 1, 0, 0, 1, 1, 0),
               z = c(0, 0, 0, 0, 1, 1, 1, 1),
               i = c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2),
               j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
               k = c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6),
               opacity = 0.1,
               color = I("red"),
               intensity = 10,
               #colors = colorRamp(rainbow(8))
               
)


# second ------------------------------------------------------------------


p = add_mesh(p = p,
            x = c(0, 0, 0.25, 0.25, 0, 0, 0.25, 0.25),
            y = c(0, 0.25, 0.25, 0, 0, 0.25, 0.25, 0),
            z = c(0, 0, 0, 0, 0.25, 0.25, 0.25, 0.25),
            i = c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2),
            j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
            k = c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6),
            opacity = 0.1,
            #intensity = 1,
            color = I("red")
            #colors = colorRamp(rainbow(8))
)

p
