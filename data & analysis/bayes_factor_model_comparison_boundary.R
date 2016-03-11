# load required libraries ####
require(BayesFactor)

# load data for dimension analysis ####
bf.boundary.data <- read.csv("data & analysis/export-data/bf-boundary.csv")
bf.boundary.data$mturk_id <- factor(bf.boundary.data$mturk_id)

# main effects ####
bf.boundary.stim_type <- lmBF(z ~ stim_type + mturk_id, whichRandom = c('mturk_id'), data=bf.boundary.data)
bf.boundary.train_type <- lmBF(z ~ train_type + mturk_id, whichRandom = c('mturk_id'), data=bf.boundary.data)
bf.boundary.dimension <- lmBF(z ~ dimension + mturk_id, whichRandom = c('mturk_id'), data=bf.boundary.data)
bf.boundary.category_type <- lmBF(z ~ category_type + mturk_id, whichRandom = c('mturk_id'), data=bf.boundary.data)
bf.boundary.intercept.only <- lmBF(z ~ mturk_id, whichRandom = c('mturk_id'), data=bf.boundary.data)
bf.boundary.main.effects.only <- c(
  bf.boundary.stim_type/bf.boundary.intercept.only,
  bf.boundary.train_type/bf.boundary.intercept.only,
  bf.boundary.dimension/bf.boundary.intercept.only,
  bf.boundary.category_type/bf.boundary.intercept.only
)
summary(bf.boundary.main.effects.only)

# model comparison for CP effect ####
boundary.main.effects <- 'z ~ stim_type + train_type + dimension + category_type + trial_type + mturk_id'
boundary.main.effects.cp <- 'z ~ stim_type + train_type + dimension + category_type + trial_type + mturk_id + train_type:category_type'

bf.boundary.main <- lmBF(as.formula(boundary.main.effects), whichRandom = c('mturk_id'), data=bf.boundary.data)
bf.boundary.cp <- lmBF(as.formula(boundary.main.effects.cp), whichRandom = c('mturk_id'), data=bf.boundary.data)
bf.boundary.cp.effect <- bf.boundary.cp / bf.boundary.main
summary(bf.boundary.cp.effect) # CP model is preferred 63:1 over main effects model.

# model comparisons for trial_type X CP interactions ####
# the main goal of this analysis is to see if models with trial_type interacting with the CP interaction (train_type:category_type)
# are ever favored over the model with just the main effects and the CP interaction.
boundary.trial_type.a <- 'trial_type:train_type:category_type'
boundary.trial_type.b <- 'trial_type:train_type:category_type:dimension'
boundary.trial_type.c <- 'trial_type:train_type:category_type:stim_type'
boundary.trial_type.d <- 'trial_type:train_type:category_type:stim_type:dimension'

boundary.models.trial_type = c(
  paste(boundary.main.effects.cp, boundary.trial_type.a, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.trial_type.b, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.trial_type.c, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.trial_type.d, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.trial_type.a, boundary.trial_type.b, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.trial_type.a, boundary.trial_type.c, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.trial_type.a, boundary.trial_type.d, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.trial_type.b, boundary.trial_type.c, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.trial_type.b, boundary.trial_type.d, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.trial_type.c, boundary.trial_type.d, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.trial_type.a, boundary.trial_type.b, boundary.trial_type.c, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.trial_type.a, boundary.trial_type.b, boundary.trial_type.d, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.trial_type.a, boundary.trial_type.c, boundary.trial_type.d, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.trial_type.b, boundary.trial_type.c, boundary.trial_type.d, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.trial_type.a, boundary.trial_type.b, boundary.trial_type.c, boundary.trial_type.d, sep = ' + ')
)

bf.boundary.results.trial_type <- lapply(boundary.models.trial_type, function(x){
  bf <- lmBF(as.formula(x), whichRandom = c('mturk_id'), data=bf.boundary.data)
  return(bf)
})

bf.boundary.trial_type.against.cp <- lapply(bf.boundary.results.trial_type, function(x){
  bf <- x / bf.boundary.cp
  return(bf)
})

bf.boundary.combined.trial_type <- bf.boundary.trial_type.against.cp[[1]]
for(i in 2:length(bf.boundary.trial_type.against.cp)){
  bf.boundary.combined.trial_type <- c(bf.boundary.combined.trial_type, bf.boundary.trial_type.against.cp[[i]])
}

# model comparisons for stim_type X CP interactions ####

boundary.stim_type.a <- 'stim_type:train_type:category_type'
boundary.stim_type.b <- 'stim_type:train_type:category_type:dimension'
boundary.stim_type.c <- 'stim_type:train_type:category_type:trial_type'
boundary.stim_type.d <- 'stim_type:train_type:category_type:trial_type:dimension'

boundary.models.stim_type = c(
  paste(boundary.main.effects.cp, boundary.stim_type.a, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.stim_type.b, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.stim_type.c, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.stim_type.d, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.stim_type.a, boundary.stim_type.b, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.stim_type.a, boundary.stim_type.c, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.stim_type.a, boundary.stim_type.d, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.stim_type.b, boundary.stim_type.c, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.stim_type.b, boundary.stim_type.d, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.stim_type.c, boundary.stim_type.d, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.stim_type.a, boundary.stim_type.b, boundary.stim_type.c, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.stim_type.a, boundary.stim_type.b, boundary.stim_type.d, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.stim_type.a, boundary.stim_type.c, boundary.stim_type.d, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.stim_type.b, boundary.stim_type.c, boundary.stim_type.d, sep = ' + '),
  paste(boundary.main.effects.cp, boundary.stim_type.a, boundary.stim_type.b, boundary.stim_type.c, boundary.stim_type.d, sep = ' + ')
)

bf.boundary.results.stim_type <- lapply(boundary.models.stim_type, function(x){
  bf <- lmBF(as.formula(x), whichRandom = c('mturk_id'), data=bf.boundary.data)
  return(bf)
})

bf.boundary.stim_type.against.cp <- lapply(bf.boundary.results.stim_type, function(x){
  bf <- x / bf.boundary.cp
  return(bf)
})

bf.boundary.combined.stim_type <- bf.boundary.stim_type.against.cp[[1]]
for(i in 2:length(bf.boundary.stim_type.against.cp)){
  bf.boundary.combined.stim_type <- c(bf.boundary.combined.stim_type, bf.boundary.stim_type.against.cp[[i]])
}

# check if dimension interacted with CP ####
bf.boundary.cp.dimension <- lmBF(z ~ stim_type + train_type + dimension + category_type + trial_type + mturk_id + train_type:category_type + train_type:category_type:dimension, whichRandom = c('mturk_id'),data=bf.boundary.data)/bf.boundary.cp

# save output! ####
save(bf.boundary.main.effects.only, bf.boundary.cp.dimension, bf.boundary.combined.stim_type, bf.boundary.combined.trial_type, bf.boundary.cp.effect, file="bayes-factor-analysis-results.Rdata")

