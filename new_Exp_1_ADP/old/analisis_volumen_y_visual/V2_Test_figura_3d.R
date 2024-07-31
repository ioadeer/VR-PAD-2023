
dimensions.raw  <- read.csv('./analisis-pad-main/data/dimensiones_de_sala_visual_1_32_sin_outliers.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

for (i in 1: 1:nrow(dimensions.raw)) {
  # obj = objs$shapes[[i]];
  # Sala real
  
  width = dimensions.raw$SR_width[i]
  depth = dimensions.raw$SR_depth[i]
  height = dimensions.raw$SR_height[i]
  
  if (i==1) {
    p = plot_ly(type = "mesh3d",
                scene = 'scene1',
                x = c(0, 0, depth, depth, 0, 0, depth, depth),
                y = c(0, width, width, 0, 0, width, width, 0),
                z = c(0, 0, 0, 0, height, height, height, height),
                i = c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2),
                j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
                k = c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6),
                opacity = 0.1,
                color = I("red"),
                intensity = 10,
                showlegend= FALSE,
                showscale = FALSE
    )
  } 
  else {
    p = add_mesh(p = p,
                 scene = 'scene1',
                 x = c(0, 0, depth, depth, 0, 0, depth, depth),
                 y = c(0, width, width, 0, 0, width, width, 0),
                 z = c(0, 0, 0, 0, height, height, height, height),
                 i = c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2),
                 j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
                 k = c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6),
                 opacity = 0.1,
                 color = I("red"),
                 intensity = 10,
                 showlegend= FALSE,
                 showscale = FALSE
    )
  }
  fig1 <-  p %>% 
    hide_colorbar()
  
  # Sala grande realidad virtual
  width = dimensions.raw$SG_RV_width[i]
  depth = dimensions.raw$SG_RV_depth[i]
  height = dimensions.raw$SG_RV_height[i]
  
  if (i==1) {
    p = plot_ly(type = "mesh3d",
                scene = 'scene2',
                x = c(0, 0, depth, depth, 0, 0, depth, depth),
                y = c(0, width, width, 0, 0, width, width, 0),
                z = c(0, 0, 0, 0, height, height, height, height),
                i = c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2),
                j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
                k = c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6),
                opacity = 0.1,
                color = I("blue"),
                intensity = 10,
                showlegend= FALSE,
                showscale = FALSE
    )
    
  } 
  else {
    p = add_mesh(p = p,
                 scene = 'scene2',
                 x = c(0, 0, depth, depth, 0, 0, depth, depth),
                 y = c(0, width, width, 0, 0, width, width, 0),
                 z = c(0, 0, 0, 0, height, height, height, height),
                 i = c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2),
                 j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
                 k = c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6),
                 opacity = 0.1,
                 color = I("blue"),
                 intensity = 10,
                 showlegend= FALSE,
                 showscale = FALSE
    )
  }
  fig2 <-  p %>% 
    hide_colorbar()
}

# https://plotly.com/r/reference/layout/
# https://plotly.com/r/reference/layout/scene/
axx <- list(
  gridcolor='rgb(255, 255, 255)',
  zerolinecolor='rgb(255, 255, 255)',
  showbackground=TRUE,
  backgroundcolor='rgb(230, 230,230)'
)



fig <- subplot(fig1, fig2) %>% 
  layout(title = "3D Subplots", 
         scene1 = list(#domain = list(x = c(0,0.5),y = c(0.5,1)),
           title="Sala real",
           #xaxis = axx, yaxis = axx, zaxis = axx,
           showscale = FALSE,
           aspectmode = 'auto'),
         scene2 = list(#domain = list(x = c(0.5,1),y = c(0.5,1)),
           title="Sala virtual",
           #xaxis = axx, yaxis = axx, zaxis = axx,
           showscale = FALSE,
           aspectmode = 'auto')
  )

# fig
# fig$x$layout$showlegend = FALSE
# fig$x$layout$scene1

fig <- plotly_build(fig)
fig
#orca(fig, "test.svg", args=c('--disable-gpu'))

# En este miniconda se instalo plotly y kaleido
reticulate::use_miniconda('r-reticulate')
save_image(fig, "imgs/two_rooms.png", scale = 0.9, width = 500, height = 500,)


fig$x$layout
