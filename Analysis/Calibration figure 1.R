

library(tidyverse)


teg <- data.frame(pred = cont.lm$fitted.values, resid = cont.lm$residuals, 
                  World_bank_region = cont.lm$model$WB_region)

ggplot(teg, aes(x = pred, y = pred+resid, colour = World_bank_region)) + 
  geom_point() + theme_classic() + scale_y_continuous(breaks = c(0.3, 0.5, 0.7, 0.9)) +
   scale_x_continuous(breaks = c(0.3, 0.5, 0.7, 0.9)) +
   geom_abline(intercept = 0, slope = 1,colour = 'black', size = 1) +
    ylab('Education index') + xlab('Modelled education index') + 
     scale_colour_viridis_d() + theme(text = element_text(size= 15))

teg <- data.frame(pred = health.lm$fitted.values, resid = health.lm$residuals)

ggplot(teg, aes(x = pred, y = pred+resid)) + 
  geom_point() + theme_classic() + scale_y_continuous(breaks = c(0.3, 0.5, 0.7, 0.9)) +
  scale_x_continuous(breaks = c(0.3, 0.5, 0.7, 0.9)) +
  geom_abline(intercept = 0, slope = 1,colour = 'black', size = 1) +
  ylab('Health index') + xlab('Modelled Health index') +
   theme(text = element_text(size= 15))
