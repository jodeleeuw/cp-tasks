# load required libraries ####
require(BayesFactor)

# load data for dimension analysis ####
bf.dimension.data <- read.csv("data & analysis/export-data/bf-dimension.csv")
bf.dimension.data$mturk_id <- factor(bf.dimension.data$mturk_id)

# main effects ####
bf.dimension.stim_type <- lmBF(z ~ stim_type + mturk_id, whichRandom = c('mturk_id'), data=bf.dimension.data)
bf.dimension.train_type <- lmBF(z ~ train_type + mturk_id, whichRandom = c('mturk_id'), data=bf.dimension.data)
bf.dimension.dimension <- lmBF(z ~ dimension + mturk_id, whichRandom = c('mturk_id'), data=bf.dimension.data)
bf.dimension.varying_dimension <- lmBF(z ~ varying_dimension + mturk_id, whichRandom = c('mturk_id'), data=bf.dimension.data)
bf.dimension.intercept.only <- lmBF(z ~ mturk_id, whichRandom = c('mturk_id'), data=bf.dimension.data)
bf.dimension.main.effects.only <- c(
  bf.dimension.stim_type/bf.dimension.intercept.only,
  bf.dimension.train_type/bf.dimension.intercept.only,
  bf.dimension.dimension/bf.dimension.intercept.only,
  bf.dimension.varying_dimension/bf.dimension.intercept.only
)
summary(bf.dimension.main.effects.only)

# model comparison for CP effect ####
dimension.main.effects <- 'z ~ stim_type + train_type + dimension + varying_dimension + trial_type + mturk_id'
dimension.main.effects.cp <- 'z ~ stim_type + train_type + dimension + varying_dimension + trial_type + mturk_id + train_type:varying_dimension'

bf.dimension.main <- lmBF(as.formula(dimension.main.effects), whichRandom = c('mturk_id'), data=bf.dimension.data)
bf.dimension.cp <- lmBF(as.formula(dimension.main.effects.cp), whichRandom = c('mturk_id'), data=bf.dimension.data)
bf.dimension.cp.effect <- bf.dimension.cp / bf.dimension.main
summary(bf.dimension.cp.effect)

# model comparisons for trial_type X CP interactions ####
# the main goal of this analysis is to see if models with trial_type interacting with the CP interaction (train_type:varying_dimension)
# are ever favored over the model with just the main effects and the CP interaction.
dimension.trial_type.a <- 'trial_type:train_type:varying_dimension'
dimension.trial_type.b <- 'trial_type:train_type:varying_dimension:dimension'
dimension.trial_type.c <- 'trial_type:train_type:varying_dimension:stim_type'
dimension.trial_type.d <- 'trial_type:train_type:varying_dimension:stim_type:dimension'

dimension.models.trial_type = c(
  paste(dimension.main.effects.cp, dimension.trial_type.a, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.trial_type.b, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.trial_type.c, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.trial_type.d, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.trial_type.a, dimension.trial_type.b, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.trial_type.a, dimension.trial_type.c, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.trial_type.a, dimension.trial_type.d, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.trial_type.b, dimension.trial_type.c, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.trial_type.b, dimension.trial_type.d, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.trial_type.c, dimension.trial_type.d, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.trial_type.a, dimension.trial_type.b, dimension.trial_type.c, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.trial_type.a, dimension.trial_type.b, dimension.trial_type.d, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.trial_type.a, dimension.trial_type.c, dimension.trial_type.d, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.trial_type.b, dimension.trial_type.c, dimension.trial_type.d, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.trial_type.a, dimension.trial_type.b, dimension.trial_type.c, dimension.trial_type.d, sep = ' + ')
)

bf.dimension.results.trial_type <- lapply(dimension.models.trial_type, function(x){
  bf <- lmBF(as.formula(x), whichRandom = c('mturk_id'), data=bf.dimension.data)
  return(bf)
})

bf.dimension.trial_type.against.cp <- lapply(bf.dimension.results.trial_type, function(x){
  bf <- x / bf.dimension.cp
  return(bf)
})

bf.dimension.combined.trial_type <- bf.dimension.trial_type.against.cp[[1]]
for(i in 2:length(bf.dimension.trial_type.against.cp)){
  bf.dimension.combined.trial_type <- c(bf.dimension.combined.trial_type, bf.dimension.trial_type.against.cp[[i]])
}

# model comparisons for stim_type X CP interactions ####

dimension.stim_type.a <- 'stim_type:train_type:varying_dimension'
dimension.stim_type.b <- 'stim_type:train_type:varying_dimension:dimension'
dimension.stim_type.c <- 'stim_type:train_type:varying_dimension:trial_type'
dimension.stim_type.d <- 'stim_type:train_type:varying_dimension:trial_type:dimension'

dimension.models.stim_type = c(
  paste(dimension.main.effects.cp, dimension.stim_type.a, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.stim_type.b, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.stim_type.c, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.stim_type.d, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.stim_type.a, dimension.stim_type.b, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.stim_type.a, dimension.stim_type.c, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.stim_type.a, dimension.stim_type.d, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.stim_type.b, dimension.stim_type.c, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.stim_type.b, dimension.stim_type.d, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.stim_type.c, dimension.stim_type.d, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.stim_type.a, dimension.stim_type.b, dimension.stim_type.c, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.stim_type.a, dimension.stim_type.b, dimension.stim_type.d, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.stim_type.a, dimension.stim_type.c, dimension.stim_type.d, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.stim_type.b, dimension.stim_type.c, dimension.stim_type.d, sep = ' + '),
  paste(dimension.main.effects.cp, dimension.stim_type.a, dimension.stim_type.b, dimension.stim_type.c, dimension.stim_type.d, sep = ' + ')
)

bf.dimension.results.stim_type <- lapply(dimension.models.stim_type, function(x){
  bf <- lmBF(as.formula(x), whichRandom = c('mturk_id'), data=bf.dimension.data)
  return(bf)
})

bf.dimension.stim_type.against.cp <- lapply(bf.dimension.results.stim_type, function(x){
  bf <- x / bf.dimension.cp
  return(bf)
})

bf.dimension.combined.stim_type <- bf.dimension.stim_type.against.cp[[1]]
for(i in 2:length(bf.dimension.stim_type.against.cp)){
  bf.dimension.combined.stim_type <- c(bf.dimension.combined.stim_type, bf.dimension.stim_type.against.cp[[i]])
}

# check if dimension interacted with CP ####
bf.dimension.cp.dimension <- lmBF(z ~ stim_type + train_type + dimension + varying_dimension + trial_type + mturk_id + train_type:varying_dimension + train_type:varying_dimension:dimension, whichRandom = c('mturk_id'),data=bf.dimension.data)/bf.dimension.cp

# save output! ####
save(bf.dimension.main.effects.only, bf.dimension.cp.dimension, bf.dimension.combined.stim_type, bf.dimension.combined.trial_type, bf.dimension.cp.effect, file="data & analysis/export-data/bayes-factor-analysis-results-dimension.Rdata")

