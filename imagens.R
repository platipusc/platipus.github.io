require(ggplot2)
require(png)
require(grid)

## plataforma
p <- qplot(1:10, 1:10, geom="blank") +
  annotation_raster(img, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  theme_minimal()
ggsave('img/portfolio/visualizacao.png', p, width=9, height=6.5, dpi=100)

## forecasting

## estrategia

## prova

## desempenho

## inteligencia
