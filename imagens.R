require(ggplot2)
require(png)
require(grid)

## plataforma

# IMG SEPARADA

## forecasting

library(ggplot2)
library(reshape2)


HWplot<-function(ts_object,  n.ahead=4,  CI=.95,  error.ribbon='green', line.size=1){
  hw_object<-HoltWinters(ts_object)
  forecast<-predict(hw_object,  n.ahead=n.ahead,  prediction.interval=T,  level=CI)
  for_values<-data.frame(time=round(time(forecast),  3),  value_forecast=as.data.frame(forecast)$fit,  dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
  fitted_values<-data.frame(time=round(time(hw_object$fitted),  3),  value_fitted=as.data.frame(hw_object$fitted)$xhat)
  actual_values<-data.frame(time=round(time(hw_object$x),  3),  Actual=c(hw_object$x))
  graphset<-merge(actual_values,  fitted_values,  by='time',  all=TRUE)
  graphset<-merge(graphset,  for_values,  all=TRUE,  by='time')
  graphset[is.na(graphset$dev),  ]$dev<-0
  graphset$Fitted<-c(rep(NA,  NROW(graphset)-(NROW(for_values) + NROW(fitted_values))),  fitted_values$value_fitted,  for_values$value_forecast)
  graphset.melt<-melt(graphset[, c('time', 'Actual', 'Fitted')], id='time')
  p <- ggplot(graphset.melt,  aes(x=time,  y=value)) +
    geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev,  ymax=Fitted + dev),  alpha=.2,  fill=error.ribbon) +
    geom_line(aes(colour=variable), size=line.size) + geom_vline(x=max(actual_values$time),  lty=2) +
    xlab('Time') +
    ylab('Value') +
    theme(legend.position='bottom') +
    scale_colour_hue('')
  return(p)
}
demand <- ts(BJsales, start = c(2000, 1), frequency = 12)
hw <- HoltWinters(demand)
forecast <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95)

graph <- HWplot(demand, n.ahead = 12, error.ribbon = "red")
# add a title
# change the x scale a little
graph <- graph + scale_x_continuous(breaks = seq(1998, 2015))
# change the y-axis title
graph <- graph + ylab("Demanda (R$)") + xlab("Ano")
# change the colour of the lines
graph <- graph + scale_colour_brewer("Legend", palette = "Set1")
# the result:
p <- graph + guides(colour=F) + theme_bw() + theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1, size = 13),
                                                   axis.text.y=element_text(size=13))

ggsave('img/portfolio/forecasting.png', p, width=9, height=6.5, dpi=100)

## estrategia

s <- 1
p <- ggplot() +
  geom_blank(aes(x=seq(-1.38, 1.38, length.out = 100), y=seq(-1,1, length.out = 100)), size=s) +
  geom_segment(aes(x=0, xend=0, y=-.8, yend=1), size=s) +
  geom_segment(aes(x=-.625, xend=.625, y=.5, yend=.5), size=s) +
  geom_polygon(aes(x=c(-.375, 0, .375), y=c(-1, -.8, -1)), fill='transparent', colour='black', size=s) +
  geom_polygon(aes(x=c(-.875, -.625, -.375), y=c(.2, .5, .2)), fill='transparent', colour='black', size=s) +
  geom_polygon(aes(x=c(.375, .625, .875), y=c(.2, .5, .2)), fill='transparent', colour='black', size=s) +
  theme(axis.text=element_blank())
ggsave('img/portfolio/estrategia.png', p, width=9, height=6.5, dpi=100)


## prova

library(CausalImpact)
x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 100)
y <- 1.2 * x1 + rnorm(100)
y[71:100] <- y[71:100] + 10
data <- cbind(y, x1)
pre.period <- c(1, 70)
post.period <- c(71, 100)
impact <- CausalImpact(data, pre.period, post.period)
p <- plot(impact, c("original", "pointwise"))
p
ggsave('img/portfolio/prova.png', p, width=9, height=6.5, dpi=100)

## desempenho

### IMAGEM SEPARADA

## inteligencia
