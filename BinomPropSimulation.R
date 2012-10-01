library(random)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(xtable)
library(gridExtra)
plotCols <- c('#FFC4E9','#1F78B4')
# General Bayesian Binom Prop Inference Functions
source('InferenceFunctions.R') 

###################################################
### Plot possible values
###################################################
ds <- data.frame(x=c(1,2), y=c(1,1), class = factor(c(1,0)))
p <- ggplot(ds, aes(x=factor(x), y=y, color=factor(class), shape=factor(class))) 
p <- p + geom_point(size = 60)
p <- p + theme_bw()
p <- p + scale_shape_manual(values=c('B','A'), guide=FALSE) + scale_color_manual(values=plotCols, guide=FALSE)
p <- p + opts(axis.text.y = theme_blank())
p <- p + opts(axis.text.x = theme_blank())
p <- p + opts(axis.ticks = theme_blank())
p <- p + opts(panel.border = theme_blank())
p <- p + xlab('') + ylab('')
p <- p + opts(legend.position = 'bottom', 
legend.title=theme_blank())
p <- p + opts(panel.grid.major = theme_blank(), 
panel.grid.minor = theme_blank())
p <- p + opts(title='Possible Outcomes')
p <- p + opts(plot.margin = unit(c(1,1,-1,-1),"lines"))
#ggsave('PossibleOutcomes.png', width=5, height=3, scale=1.2)


###################################################
### Plot theta value examples
###################################################

thetaSamplePlot(0.2, 'thetaPoint2', save.png=F)
thetaSamplePlot(0.5, 'thetaPoint5', save.png=F)
thetaSamplePlot(0.8, 'thetaPoint8', save.png=F)

###################################################
### Generate theta: Unknown Probability of Success 
###################################################
## Using built-in R pseudo-random number generator
theta_true <- runif(1,0,1)

###################################################
### Generate population
###################################################
N = sample(seq(100000,400000),1)
A = round(theta_true*N)
B = N - A
Zpop <- sample(c(rep(1,A),rep(0,B)))

###################################################
## Plot simulated population
##################################################
Zpop_df <- data.frame(Zpop, 
'x'=sample(seq(length(Zpop))),
'y'=sample(seq(length(Zpop))))
## Plot 'population'
p <- ggplot(Zpop_df, aes(x=x,y=y,
color=factor(Zpop),
shape=factor(Zpop)))
p <- p + opts(title='Z: The "Population"')
p <- p + theme_bw()
p <- p + geom_point(size=2) 
p <- p + scale_shape_manual(values=c('B','A'),guide='none')
p <- p + scale_color_manual(values=plotCols,guide='none')
p <- p + opts(axis.text.y = theme_blank())
p <- p + opts(axis.text.x = theme_blank())
p <- p + opts(axis.ticks = theme_blank())
p <- p + opts(panel.border = theme_blank())
p <- p + xlab('') + ylab('')
p <- p + opts(legend.position = 'bottom', 
legend.title=theme_blank())
p <- p + coord_polar(theta='y')
p <- p + opts(panel.grid.major = theme_blank(), 
panel.grid.minor = theme_blank())
p <- p + opts(plot.margin = unit(c(1,-1,-1,-1),"lines"))
#ggsave('PopulationPlot.png', width=5, height=5, scale=.9)

###################################################
### Pull random sample from population
##################################################
N_samp <- 500
Zsamp <- sample(Zpop,N_samp)

###################################################
### Visualize Sample
##################################################
Zsamp_df <- data.frame('Z'=Zsamp, 
'x'=sample(seq(length(Zsamp))),
'y'=sample(seq(length(Zsamp))))
plotThetaDf(Zsamp_df, 'SamplePlot')

###################################################
### Count Successes
###################################################
Y_samp <- sum(Zsamp)

###################################################
### Explore beta distributions as
### possible representations of our beliefs
###################################################
## Uniform
p <- betaplot(1,1)
p <- p + opts(title = 'Uniform Beta, alpha = 1, beta = 1')
#ggsave('UniformBeta.png', width = 5, height = 3, scale = 1.1)

## Bimodal
p <- betaplot(0.5,0.5)
p <- p + opts(title = 'Bimodal Beta, a = 0.5, b = 0.5')
#ggsave('BimodalBeta.png',  width = 5, height = 3, scale = 1.1)

## Strong Central
p <- betaplot(50,50)
p <- p + opts(title = 'Strong Central Value Beta, a = 25, b = 25')
#ggsave('StrongCentralBeta.png',  width = 5, height = 3, scale = 1.1)

## Strong High
p <- betaplot(45,5)
p <- p + opts(title = 'Strong High Value Beta, a = 45, b = 5')
#ggsave('StrongHighBeta.png', width = 5, height = 3, scale = 1.2)

## Strong Low
p <- betaplot(5,45)
p <- p + opts(title = 'Strong, Low Value Beta, a = 5, b = 45')
#ggsave('StrongLowBeta.png', width = 5, height = 3, scale = 1.2)

###################################################
### Fit models with different priors
###################################################
N_samp=500
Y_samp=26
un <- getModelValues(.5,2,N_samp,Y_samp)
plotModel(un, 'Uniform Prior, m = .5, n = 2')

bm <- getModelValues(.5,1,N_samp,Y_samp)
plotModel(bm, 'Bimodal (Jeffreys) Prior, m = .5, n = 1')

wh <- getModelValues(.95,10,N_samp,Y_samp)
plotModel(wh, 'Weak, High Success Prior, m = .2, n = 100')

we <- getModelValues(.5,10,N_samp,Y_samp)
plotModel(we, 'Weak, Equal Success/Failure Prior, m = .2, n = 100')

wl <- getModelValues(.05,10,N_samp,Y_samp)
plotModel(wl, 'Weak, Low Success Prior, m = .2, n = 100')

sh <- getModelValues(.95,100,N_samp,Y_samp)
plotModel(sh, 'Strong, High Success Prior, m = .2, n = 100')

se <- getModelValues(.5,100,N_samp,Y_samp)
plotModel(se, 'Strong, Equal Success/Failure Prior, m = .2, n = 100')

sl <- getModelValues(.05,100,N_samp,Y_samp)
plotModel(sl, 'Strong, Low Success Prior, m = .2, n = 100')

##################################################
## Get model summary statistics
##################################################
models_df <- rbind(un[['InfDf']],bm[['InfDf']],wh[['InfDf']],
we[['InfDf']],wl[['InfDf']],sh[['InfDf']],se[['InfDf']],sl[['InfDf']])

rownames(models_df) <- c('Uniform','Bimodal',
'Weak High','Weak Equal',
'Weak Low','Strong High',
'Strong Equal','Strong Low')

models_df$Error <- models_df$mean_posterior - theta_true

print(paste('The true value of theta is',theta_true))
print(models_df)

