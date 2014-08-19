# depmixs4 help file
# http://cran.r-project.org/web/packages/depmixS4/depmixS4.pdf
# create a 2 state model with one continuous and one binary response
# The multinorminal is the generalisation of the logistic regression to two or
# more classes
data(speed)
head(speed)
mod <- depmix(list(rt~1,corr~1),data=speed,nstates=2,family=list(gaussian(),multinomial()))
# print the model, formulae and parameter values (ie the starting values)
mod
data(balance)
# depmix
# create a 2 state model with one continuous and one binary response
# ntimes is used to specify the lengths of 3 separate series
data(speed)
mod <- depmix(list(rt~1,corr~1),data=speed,nstates=2,
              family=list(gaussian(),multinomial("identity")),ntimes=c(168,134,137))
# print the model, formulae and parameter values
mod
set.seed(1)
# fit the model by calling fit
fm <- fit(mod)
summary(fm)
# Volatility of S & P 500 returns
# (thanks to Chen Haibo for providing this example)
data(sp500)
# fit some models
msp <- depmix(logret~1,nstates=2,data=sp500)
set.seed(1)
fmsp <- fit(msp)
# plot posterior state sequence for the 2-state model
plot(ts(posterior(fmsp)[,2], start=c(1950,2),deltat=1/12),ylab="probability",
     main="Posterior probability of state 1 (volatile, negative markets).",
     frame=FALSE)
## Not run:
# this creates data with a single change point with Poisson data
set.seed(3)
y1 <- rpois(50,1)
y2 <- rpois(50,2)
ydf <- data.frame(y=c(y1,y2))
# fit models with 1 to 3 states
m1 <- depmix(y~1,ns=1,family=poisson(),data=ydf)
set.seed(1)
fm1 <- fit(m1)
m2 <- depmix(y~1,ns=2,family=poisson(),data=ydf)
set.seed(1)
fm2 <- fit(m2)
m3 <- depmix(y~1,ns=3,family=poisson(),data=ydf)
set.seed(1)
fm3 <- fit(m3,em=em.control(maxit=500))
# plot the BICs to select the proper model
plot(1:3,c(BIC(fm1),BIC(fm2),BIC(fm3)),ty="b")
## End(Not run)

## Not run:
# similar to the binomial model, data may also be entered in
# multi-column format where the n for each row can be different
dt <- data.frame(y1=c(0,1,1,2,4,5),y2=c(1,0,1,0,1,0),y3=c(4,4,3,2,1,1))
dt
# specify a mixture model ...
m2 <- mix(cbind(y1,y2,y3)~1,data=dt,ns=2,family=multinomial("identity"))
set.seed(1)
fm2 <- fit(m2)
# ... or dependent mixture model
dm2 <- depmix(cbind(y1,y2,y3)~1,data=dt,ns=2,family=multinomial("identity"))
set.seed(1)
fdm2 <- fit(dm2)
## End(Not run)
summary(fm2)
summary(fm3)
# More examples
data(speed)
# 2-state model on rt and corr from speed data set
# with Pacc as covariate on the transition matrix
# ntimes is used to specify the lengths of 3 separate series
mod1 <- depmix(list(rt~1,corr~1),data=speed,transition=~Pacc,nstates=2,
               family=list(gaussian(),multinomial("identity")),ntimes=c(168,134,137))
# fit the model
set.seed(3)
fmod1 <- fit(mod1)
fmod1 # to see the logLik and optimization information
# to see the parameters
summary(fmod1)
# same model, now with missing data
## Not run:
speed[2,1] <- NA
speed[3,2] <- NA
# 2-state model on rt and corr from speed data set
# with Pacc as covariate on the transition matrix
# ntimes is used to specify the lengths of 3 separate series
mod1ms <- depmix(list(rt~1,corr~1),data=speed,transition=~Pacc,nstates=2,
                 family=list(gaussian(),multinomial("identity")),ntimes=c(168,134,137))
# fit the model
set.seed(3)
fmod1ms <- fit(mod1ms)
## End(Not run)
# instead of the normal likelihood, we can also maximise the "classification" likelihood
# this uses the maximum a posteriori state sequence to assign observations to states
# and to compute initial and transition probabilities.fit 19
fmod1b <- fit(mod1,emcontrol=em.control(classification="hard"))
fmod1b # to see the logLik and optimization information
# FIX SOME PARAMETERS
# get the starting values of this model to the optimized
# values of the previously fitted model to speed optimization
pars <- c(unlist(getpars(fmod1)))
# constrain the initial state probs to be 0 and 1
# also constrain the guessing probs to be 0.5 and 0.5
# (ie the probabilities of corr in state 1)
# change the ones that we want to constrain
pars[1]=0
pars[2]=1 # this means the process will always start in state 2
pars[13]=0.5
pars[14]=0.5 # the corr parameters in state 1 are now both 0, corresponding the 0.5 prob
mod2 <- setpars(mod1,pars)
# fix the parameters by setting:
free <- c(0,0,rep(c(0,1),4),1,1,0,0,1,1,1,1)
# fit the model
fmod2 <- fit(mod2,fixed=!free)
# likelihood ratio insignificant, hence fmod2 better than fmod1
llratio(fmod1,fmod2)
# ADDING SOME GENERAL LINEAR CONSTRAINTS
# set the starting values of this model to the optimized
# values of the previously fitted model to speed optimization
## Not run:
pars <- c(unlist(getpars(fmod2)))
pars[4] <- pars[8] <- -4
pars[6] <- pars[10] <- 10
mod3 <- setpars(mod2,pars)
# start with fixed and free parameters
conpat <- c(0,0,rep(c(0,1),4),1,1,0,0,1,1,1,1)
# constrain the beta's on the transition parameters to be equal
conpat[4] <- conpat[8] <- 2
conpat[6] <- conpat[10] <- 3
fmod3 <- fit(mod3,equal=conpat)
llratio(fmod2,fmod3)
# above constraints can also be specified using the conrows argument as follows20 forwardbackward
conr <- matrix(0,2,18)
# parameters 4 and 8 have to be equal, otherwise stated, their diffence should be zero,
# and similarly for parameters 6 & 10
conr[1,4] <- 1
conr[1,8] <- -1
conr[2,6] <- 1
conr[2,10] <- -1
# note here that we use the fitted model fmod2 as that has appropriate
# starting values
fmod3b <- fit(mod3,conrows=conr,fixed=!free) # using free defined above
## End(Not run)
data(balance)
# four binary items on the balance scale task
mod4 <- mix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
            family=list(multinomial("identity"),multinomial("identity"),
                        multinomial("identity"),multinomial("identity")))
set.seed(1)
fmod4 <- fit(mod4)
## Not run:
# add age as covariate on class membership by using the prior argument
mod5 <- mix(list(d1~1,d2~1,d3~1,d4~1), data=balance, nstates=2,
            family=list(multinomial("identity"),multinomial("identity"),
                        multinomial("identity"),multinomial("identity")),
            prior=~age, initdata=balance)
set.seed(1)
fmod5 <- fit(mod5)
# check the likelihood ratio; adding age significantly improves the goodness-of-fit
llratio(fmod5,fmod4)
## End(Not run)
set.seed(1)
mod <- depmix(list(rt ~ 1,corr ~ 1), data = speed, nstates = 2,
                 family = list(gaussian(), multinomial("identity")),
                 transition = ~ scale(Pacc), instart = runif(2))
fm <- fit(mod, verbose = FALSE, emc=em.control(rand=FALSE))
summary(fm)
##Fit example
Examples
data(speed)
# 2-state model on rt and corr from speed data set
# with Pacc as covariate on the transition matrix
# ntimes is used to specify the lengths of 3 separate series
mod1 <- depmix(list(rt~1,corr~1),data=speed,transition=~Pacc,nstates=2,
               family=list(gaussian(),multinomial("identity")),ntimes=c(168,134,137))
# fit the model
set.seed(3)
fmod1 <- fit(mod1)
fmod1 # to see the logLik and optimization information
# to see the parameters
summary(fmod1)
# same model, now with missing data
## Not run:
speed[2,1] <- NA
speed[3,2] <- NA
# 2-state model on rt and corr from speed data set
# with Pacc as covariate on the transition matrix
# ntimes is used to specify the lengths of 3 separate series
mod1ms <- depmix(list(rt~1,corr~1),data=speed,transition=~Pacc,nstates=2,
                 family=list(gaussian(),multinomial("identity")),ntimes=c(168,134,137))
# fit the model
set.seed(3)
fmod1ms <- fit(mod1ms)
## End(Not run)
# instead of the normal likelihood, we can also maximise the "classification" likelihood
# this uses the maximum a posteriori state sequence to assign observations to states
# and to compute initial and transition probabilities.