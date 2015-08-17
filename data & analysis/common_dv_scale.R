#### required packages ####

require(plyr)
require(sciplot)
require(gridExtra)
require(ggplot2)
require(ggthemes)
source('data & analysis/helpers/withinSE.R')

#### load data ####

alldata <- read.csv2('data & analysis/raw-data/filtered-data-no-mturk-id.csv')

#### count subjects per condition ####
subcount <- ddply(alldata, .(mturk_id, exp_condition), function(subset){return(1)})
table(subcount$exp_condition)

#### adding columns for relevant_distance and irrelevant_distance
alldata$relevant_distance <- mapply(function(x,y,relevant){
  if(relevant == 'shape-relevant'){ return(x) }
  if(relevant == 'tail-relevant'){ return(y) }
}, alldata$xdist, alldata$ydist, alldata$dimension)

alldata$irrelevant_distance <- mapply(function(x,y,relevant){
  if(relevant == 'shape-relevant'){ return(y) }
  if(relevant == 'tail-relevant'){ return(x) }
}, alldata$xdist, alldata$ydist, alldata$dimension)

alldata$common_score <- mapply(function(s,c){
  if(!is.na(s)){ return(-s) }
  else { return(c) }
},alldata$sim_score, alldata$correct)

testdata <- subset(alldata, trial_type%in%c('same-different','xab','similarity'))
testdata$trial_type <- factor(as.character(testdata$trial_type))
levels(testdata$trial_type) <- c('Same-Different', 'Similarity', 'XAB')

#### different z-score means and SDs ####
z_onedimonly <- ddply(subset(testdata, (relevant_distance==0 | irrelevant_distance == 0) & distance > 0), .(trial_type), function(s){
  return(c(mean=mean(s$common_score), sd=sd(s$common_score)))
})

z_boundaryonly <- ddply(subset(testdata, irrelevant_distance==0 & distance > 0 & relevant_distance < 3), .(trial_type), function(s){
  return(c(mean=mean(s$common_score), sd=sd(s$common_score)))
})

#### basic descriptives on training data ####

trainingdata <- subset(alldata, trial_type == 'adaptive_t' | trial_type == 'adaptive_train')
train_trial_count <- ddply(trainingdata, .(mturk_id), function(s){
  len <- nrow(s)
  return(c(N=len))
})
mean(train_trial_count$N)
sd(train_trial_count$N)
max(train_trial_count$N)
min(train_trial_count$N)

#### testing data ####

## DIMENSION ANALYSIS

# remove testing items where both dimensions varied
onedimensiononly <- subset(testdata, (relevant_distance==0 | irrelevant_distance == 0) & distance > 0)

onedimensiononly$varying_dimension <- sapply(onedimensiononly$relevant_distance, function(d){
  if(d > 0) { return("relevant") }
  return("irrelevant")
})

dimension_test <- ddply(onedimensiononly, .(mturk_id, trial_type, stim_type, train_type, varying_dimension), function(s){
  m <- mean(s$common_score)

  test_m_one <- subset(z_onedimonly, trial_type==s$trial_type[1])$mean
  test_s_one <- subset(z_onedimonly, trial_type==s$trial_type[1])$sd
  z_one <- (m - test_m_one) / test_s_one
  
  return(c(z=z_one))
})

dimension_summary <- summarySEwithin(dimension_test, measurevar = "z", idvar="mturk_id", withinvars = "varying_dimension", betweenvars = c("train_type"))

dimension_main_plot <- ggplot(dimension_summary, aes(x=varying_dimension,y=z,ymin=z-se,ymax=z+se,colour=train_type,group=train_type))+
  geom_line()+
  geom_errorbar(width=0.2)+
  geom_point(size=6, fill="white", shape=21)+
  scale_colour_grey(start=0.1, end=0.5,labels=c("Control","Training"), guide=FALSE)+
  scale_x_discrete(labels=c("Irrelevant", "Relevant"))+
  labs(x="\nVarying Dimension", y="Score on Common Transformed Scale (±SEM)\n",colour="Training Condition")+
  theme_light()+
  theme(panel.grid=element_blank())

# breakout by task

dimension_task_summary <- summarySEwithin(dimension_test, measurevar = "z", idvar="mturk_id", withinvars = "varying_dimension", betweenvars = c("train_type", "trial_type"))

dimension_task_plot <- ggplot(dimension_task_summary, aes(x=varying_dimension,y=z,ymin=z-se,ymax=z+se,colour=train_type,group=train_type))+
  geom_line(position=position_dodge(width=0.2))+
  geom_errorbar(width=0.2, position=position_dodge(width=0.2))+
  geom_point(size=3, fill="white", shape=21,position=position_dodge(width=0.2))+
  scale_colour_grey(start=0.1, end=0.5,labels=c("Control","Training"))+
  scale_x_discrete(labels=c("Irrelevant", "Relevant"))+
  labs(x="\nVarying Dimension", y="",colour="Training Condition")+
  facet_grid(trial_type ~ .)+
  theme_light()+
  theme(panel.grid=element_blank(), strip.text.y = element_text(colour='grey30'), strip.background = element_rect(colour="grey70",fill=rgb(0.95, 0.95, 0.95)))

# make one figure

grid.arrange(dimension_main_plot, dimension_task_plot, ncol=2)


## BETWEEN V WITHIN

boundary_test <- ddply(subset(onedimensiononly, varying_dimension=="relevant" & distance < 3), .(mturk_id, trial_type, stim_type, train_type, category_type), function(s){
  m <- mean(s$common_score)
  test_m <- subset(z_boundaryonly, trial_type==s$trial_type[1])$mean
  test_s <- subset(z_boundaryonly, trial_type==s$trial_type[1])$sd
  z <- (m - test_m) / test_s
  return(c(z=z))
})

boundary_test$trial_type <- factor(as.character(boundary_test$trial_type))

boundary_summary <- summarySEwithin(boundary_test, measurevar = 'z', idvar="mturk_id", withinvars = 'category_type', betweenvars = c('train_type'))

boundary_main_plot <- ggplot(boundary_summary, aes(x=category_type,y=z,ymin=z-se,ymax=z+se,colour=train_type,group=train_type))+
  geom_line(position=position_dodge(width=0.2))+
  geom_errorbar(width=0.2, position=position_dodge(width=0.2))+
  geom_point(size=6, fill="white", shape=21,position=position_dodge(width=0.2))+
  scale_colour_grey(start=0.1, end=0.5,labels=c("Control","Training"), guide=FALSE)+
  scale_x_discrete(labels=c("Between Category", "Within Category"))+
  labs(x="\nPair Type", y="Score on Common Transformed Scale (±SEM)\n",colour="Training Condition")+
  theme_light()+
  theme(panel.grid=element_blank())

# by task

boundary_task_summary <- summarySEwithin(boundary_test, measurevar = 'z', idvar="mturk_id", withinvars = 'category_type', betweenvars = c('train_type', 'trial_type'))

boundary_task_plot <- ggplot(boundary_task_summary, aes(x=category_type,y=z,ymin=z-se,ymax=z+se,colour=train_type,group=train_type))+
  geom_line(position=position_dodge(width=0.2))+
  geom_errorbar(width=0.2, position=position_dodge(width=0.2))+
  geom_point(size=3, fill="white", shape=21,position=position_dodge(width=0.2))+
  scale_colour_grey(start=0.1, end=0.5,labels=c("Control","Training"))+
  scale_x_discrete(labels=c("Between Category", "Within Category"))+
  labs(x="\nPair Type", y="",colour="Training Condition")+
  facet_grid(trial_type ~ .)+
  theme_light()+
  theme(panel.grid=element_blank(), strip.text.y = element_text(colour='grey30'), strip.background = element_rect(colour="grey70",fill=rgb(0.95, 0.95, 0.95)))

# make one figure

grid.arrange(boundary_main_plot, boundary_task_plot, ncol=2)

### combine both figures into one

grid.arrange(
  textGrob('(a)'),
  arrangeGrob(boundary_main_plot, boundary_task_plot,ncol=2),
  textGrob('(b)'),
  arrangeGrob(dimension_main_plot, dimension_task_plot,ncol=2),
  ncol=1,
  heights=c(0.03,0.47,0.03,0.47)
)

### descriptive stats ###

descriptiveSummary <- function(data, factors){
  subjectlevel <- ddply(data, c(factors, .(mturk_id)), function(s){
    return(c(m=mean(s$z)))
  })
  
  factorlevel <- ddply(subjectlevel, factors, function(s){
    return(c(mean=mean(s$m), sd=sd(s$m)))
  })
  
  return(factorlevel)
}

## dimension data

# HD / LD
descriptiveSummary(dimension_test, .(stim_type))

# Relevant / Irrelevant
descriptiveSummary(dimension_test, .(varying_dimension))

# Train x Dim interaction
descriptiveSummary(dimension_test, .(train_type, varying_dimension))

## pairtype data

# HD / LD
descriptiveSummary(boundary_test, .(stim_type))

# Training Type
descriptiveSummary(boundary_test, .(train_type))

# Pair Type
descriptiveSummary(boundary_test, .(category_type))

# Train x Pair Type interaction
descriptiveSummary(boundary_test, .(train_type, category_type))
