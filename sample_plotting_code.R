library(rmcorr)
library(ggplot2)
library(RColorBrewer)


## Bland Altman 1995 data
my.rmc <- rmcorr(participant = Subject, measure1 = PaCO2, measure2 = pH,
                 dataset = bland1995)

plot(my.rmc)

#using ggplot instead
ggplot2::ggplot(bland1995, ggplot2::aes(x = PaCO2, y = pH,
                                        group = factor(Subject), color = factor(Subject))) +
  ggplot2::geom_point(ggplot2::aes(colour = factor(Subject))) +
  ggplot2::geom_line(ggplot2::aes(y = my.rmc$model$fitted.values),
                     linetype = 1)

############################
## Raz et al. 2005 data
my.rmc <- rmcorr(participant = Participant, measure1 = Age, measure2 =
                   Volume, dataset = raz2005)

blueset <- brewer.pal(8, 'Blues')
pal <- colorRampPalette(blueset)

plot(my.rmc, overall = TRUE, palette = pal, overall.col = 'black')

############################
## Gilden et al. 2010 data
my.rmc <- rmcorr(participant = sub, measure1 = rt, measure2 = acc,
                 dataset = gilden2010)
plot(my.rmc, overall = FALSE, lty = 2, xlab = "Reaction Time",
     ylab = "Accuracy")
