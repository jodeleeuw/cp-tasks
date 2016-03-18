## RUN data-manipulation-and-plotting.R first to get the right data objects

require(BayesFactor)

#### t-tests for specific CP effects

rscale = 1

t.test.compression.data <- subset(boundary_test, category_type == 'WITHIN')
t.test.compression.bf <- ttestBF(formula=z ~ train_type, data=t.test.compression.data, rscale=rscale, nullInterval = c(0,Inf))
1/t.test.compression.bf

t.test.expansion.data <- subset(boundary_test, category_type == 'BETWEEN')
t.test.expansion.bf <- ttestBF(formula=z ~ train_type, data=t.test.expansion.data, rscale=rscale,nullInterval=c(-Inf,0))
t.test.expansion.bf

t.test.acquired.distinctiveness.data <- subset(dimension_test, varying_dimension=='relevant')
t.test.acquired.distinctiveness.bf <- ttestBF(formula= z ~ train_type, data=t.test.acquired.distinctiveness.data, rscale=rscale, nullInterval = c(-Inf,0))
t.test.acquired.distinctiveness.bf

t.test.acquired.equivalence.data <- subset(dimension_test, varying_dimension=='irrelevant')
t.test.acquired.equivalence.bf <- ttestBF(formula= z ~ train_type, data=t.test.acquired.equivalence.data, rscale=rscale, nullInterval = c(0,Inf))
t.test.acquired.equivalence.bf

