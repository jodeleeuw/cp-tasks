# load required libraries ####
require(BayesFactor)

# load data for dimension analysis ####
bf_dimension <- read.csv("data & analysis/export-data/bf-dimension.csv")
bf_dimension$mturk_id <- factor(bf_dimension$mturk_id)

# model comparison for CP effect ####
main.effects <- 'z ~ stim_type + train_type + dimension + varying_dimension + trial_type + mturk_id'
main.effects.cp <- 'z ~ stim_type + train_type + dimension + varying_dimension + trial_type + mturk_id + train_type:varying_dimension'

bf.main <- lmBF(as.formula(main.effects), whichRandom = c('mturk_id'), data=bf_dimension)
bf.cp <- lmBF(as.formula(main.effects.cp), whichRandom = c('mturk_id'), data=bf_dimension)

bf.cp.effect <- bf.cp / bf.main
summary(bf.cp.effect) # CP model is preferred 200:1 over main effects model.

# testing main effects
bf.stim_type <- lmBF(z ~ stim_type + mturk_id, whichRandom = c('mturk_id'), data=bf_dimension)
bf.train_type <- lmBF(z ~ train_type + mturk_id, whichRandom = c('mturk_id'), data=bf_dimension)
bf.dimension <- lmBF(z ~ dimension + mturk_id, whichRandom = c('mturk_id'), data=bf_dimension)
bf.varying_dimension <- lmBF(z ~ varying_dimension + mturk_id, whichRandom = c('mturk_id'), data=bf_dimension)
bf.intercept.only <- lmBF(z ~ mturk_id, whichRandom = c('mturk_id'), data=bf_dimension)
bf.main.effects.only <- c(bf.stim_type/bf.intercept.only,bf.train_type/bf.intercept.only,bf.dimension/bf.intercept.only,bf.varying_dimension/bf.intercept.only)
summary(bf.main.effects.only)

# model comparisons for trial_type effect ####
# the main goal of this analysis is to see if models with trial_type interacting with the CP interaction (train_type:varying_dimension)
# are ever favored over the model with just the main effects and the CP interaction.
a <- 'trial_type:train_type:varying_dimension'
b <- 'trial_type:train_type:varying_dimension:dimension'
c <- 'trial_type:train_type:varying_dimension:stim_type'
d <- 'trial_type:train_type:varying_dimension:stim_type:dimension'

models.trial_type = c(
  paste(main.effects.cp, a, sep = ' + '),
  paste(main.effects.cp, b, sep = ' + '),
  paste(main.effects.cp, c, sep = ' + '),
  paste(main.effects.cp, d, sep = ' + '),
  paste(main.effects.cp, a, b, sep = ' + '),
  paste(main.effects.cp, a, c, sep = ' + '),
  paste(main.effects.cp, a, d, sep = ' + '),
  paste(main.effects.cp, b, c, sep = ' + '),
  paste(main.effects.cp, b, d, sep = ' + '),
  paste(main.effects.cp, c, d, sep = ' + '),
  paste(main.effects.cp, a, b, c, sep = ' + '),
  paste(main.effects.cp, a, b, d, sep = ' + '),
  paste(main.effects.cp, a, c, d, sep = ' + '),
  paste(main.effects.cp, b, c, d, sep = ' + '),
  paste(main.effects.cp, a, b, c, d, sep = ' + ')
)

bf.results.trial_type <- lapply(models.trial_type, function(x){
  bf <- lmBF(as.formula(x), whichRandom = c('mturk_id'), data=bf_dimension)
  return(bf)
})

bf.trial_type.against.cp <- lapply(bf.results.trial_type, function(x){
  bf <- x / bf.cp
  return(bf)
})

bf.combined.trial_type <- bf.trial_type.against.cp[[1]]
for(i in 2:length(bf.trial_type.against.cp)){
  bf.combined.trial_type <- c(bf.combined.trial_type, bf.trial_type.against.cp[[i]])
}

plot(bf.combined.trial_type, pars=par(cex.axis=0.1),marginExpand = 0.1)

# model comparisons for stim_type effect ####

a <- 'stim_type:train_type:varying_dimension'
b <- 'stim_type:train_type:varying_dimension:dimension'
c <- 'stim_type:train_type:varying_dimension:trial_type'
d <- 'stim_type:train_type:varying_dimension:trial_type:dimension'

models.stim_type = c(
  paste(main.effects.cp, a, sep = ' + '),
  paste(main.effects.cp, b, sep = ' + '),
  paste(main.effects.cp, c, sep = ' + '),
  paste(main.effects.cp, d, sep = ' + '),
  paste(main.effects.cp, a, b, sep = ' + '),
  paste(main.effects.cp, a, c, sep = ' + '),
  paste(main.effects.cp, a, d, sep = ' + '),
  paste(main.effects.cp, b, c, sep = ' + '),
  paste(main.effects.cp, b, d, sep = ' + '),
  paste(main.effects.cp, c, d, sep = ' + '),
  paste(main.effects.cp, a, b, c, sep = ' + '),
  paste(main.effects.cp, a, b, d, sep = ' + '),
  paste(main.effects.cp, a, c, d, sep = ' + '),
  paste(main.effects.cp, b, c, d, sep = ' + '),
  paste(main.effects.cp, a, b, c, d, sep = ' + ')
)

bf.results.stim_type <- lapply(models.stim_type, function(x){
  bf <- lmBF(as.formula(x), whichRandom = c('mturk_id'), data=bf_dimension)
  return(bf)
})

bf.stim_type.against.cp <- lapply(bf.results.stim_type, function(x){
  bf <- x / bf.cp
  return(bf)
})

bf.combined.stim_type <- bf.stim_type.against.cp[[1]]
for(i in 2:length(bf.stim_type.against.cp)){
  bf.combined.stim_type <- c(bf.combined.stim_type, bf.stim_type.against.cp[[i]])
}

plot(bf.combined.stim_type, pars=par(cex.axis=0.1), marginExpand = 0.1)

### check if dimension interacted with CP ####
bf.cp.dimension <- lmBF(z ~ stim_type + train_type + dimension + varying_dimension + trial_type + mturk_id + train_type:varying_dimension + train_type:varying_dimension:dimension, whichRandom = c('mturk_id'),data=bf_dimension)
summary(bf.cp.dimension/bf.cp)
