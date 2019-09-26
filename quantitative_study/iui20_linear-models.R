library(car)
library("ggplot2")
library(lme4)



# Set working dir to source file location
setwd("PATH_TO_REPO/quantitative_study")


#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      se = sd(x[[col]], na.rm=TRUE)/sqrt(length(x[[col]])))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}




# This includes participants that did not complete session 2
# For them, the column session2_condition is NaN
data <- read.csv('./iui20_full-session-data.csv', header = TRUE) 

# This excludes UNMOTIVATED or UNDETERMINED
only_seeker_or_avoider <- data[data['session1_ideatorType'] == 'SEEKER' | data['session1_ideatorType'] == 'AVOIDER',]
only_seeker_or_avoider$session1_ideatorType <- as.character(only_seeker_or_avoider$session1_ideatorType)
only_seeker_or_avoider$session1_ideatorType <- as.factor(only_seeker_or_avoider$session1_ideatorType)
only_seeker_or_avoider$session2_condition <- as.character(only_seeker_or_avoider$session2_condition)
only_seeker_or_avoider$session2_condition <- as.factor(only_seeker_or_avoider$session2_condition)

wihtout_unmotivated <- data[data['session1_ideatorType'] != 'UNMOTIVATED',]
wihtout_unmotivated$session1_ideatorType <- as.character(wihtout_unmotivated$session1_ideatorType)
wihtout_unmotivated$session1_ideatorType <- as.factor(wihtout_unmotivated$session1_ideatorType)
wihtout_unmotivated$session2_condition <- as.character(wihtout_unmotivated$session2_condition)
wihtout_unmotivated$session2_condition <- as.factor(wihtout_unmotivated$session2_condition)

# This includes only participants that completed session 2
session2_data <- data[data$session2_condition != '', ]
session2_data <-session2_data[!is.na(session2_data$novelty_max), ]
session2_data$session1_ideatorType <- as.character(session2_data$session1_ideatorType)
session2_data$session1_ideatorType <- as.factor(session2_data$session1_ideatorType)
session2_data$session2_condition <- as.character(session2_data$session2_condition)
session2_data$session2_condition <- as.factor(session2_data$session2_condition)

##### SESSION 1 #####

# ANOVA for idea submits
res <- aov(session1_numberOfSubmits ~ c(session1_ideatorType), data = only_seeker_or_avoider)
summary(res)


##### SESSION 2 #####


df <-session2_data
graph_data <- data_summary(df, varname='novelty_max', groupnames=c('session1_ideatorType', 'session2_condition'))
ggplot(data = graph_data, aes(x=session2_condition, y =novelty_max, group=session1_ideatorType)) + 
  geom_errorbar(size=1, aes(ymin=novelty_max-se, ymax=novelty_max+se, color=session1_ideatorType),width=.4, position=position_dodge(0.03)) +
  geom_line(size = 1, aes(color=session1_ideatorType)) +
  geom_point(size = 2,aes(color=session1_ideatorType))+ scale_color_manual(values=c('#8CBCB9', '#4F759B', '#B6A6CA', '#A9A9A9'))

df <-session2_data
# df <- df[df$session2_numberOfSubmits > 2, ]
# df <- df[!(df$session1_ideatorType == 'AVOIDER' & df$session2_condition == 'on-demand' & df$session2_numberOfRequests > 2), ]
graph_data <- data_summary(df, varname='novelty_max', groupnames=c('session1_ideatorType', 'session2_condition'))
ggplot(data = graph_data, aes(x=session2_condition, y =novelty_max, group=session1_ideatorType)) + 
  geom_errorbar(size=1, aes(ymin=novelty_max-se, ymax=novelty_max+se, color=session1_ideatorType),width=.4, position=position_dodge(0.03)) +
  geom_line(size = 1, aes(color=session1_ideatorType)) +
  geom_point(size = 2,aes(color=session1_ideatorType))+
  scale_color_manual(values=c('#8CBCB9', '#4F759B', '#B6A6CA', '#A9A9A9'), labels = c("Avoider", "Seeker")) +
  labs(x = "Condition", y = "Maximum Novelty", color = "Ideator Type") +
  theme_gray() + theme(text = element_text(size=25))


#ggplot(df, aes(x=session1_ideatorType, y=novelty_max)) + geom_point(shape=1) + facet_grid(. ~ session2_condition)
# boxplot(novelty_max ~ session1_ideatorType * session2_condition, data=session2_data, frame = FALSE, 
#         col = c("#00AFBB", "#E7B800"), ylab="Max Novelty")
# interaction.plot(x.factor = session2_data$session2_condition,
#                  trace.factor = session2_data$session1_ideatorType, 
#                  response = session2_data$novelty_max, fun = mean, 
#                  type = "b", legend = TRUE, 
#                  xlab = "Ideator Type", ylab="Max Novelty",
#                  pch=c(1,19),
#                  col = c("#00AFBB", "#E7B800"))

# Number of submits from first session 1 predicts submits inf session 2 (***)
res <- aov(session2_numberOfSubmits ~ session1_numberOfSubmits, data = session2_data)
summary(res)

# ANOVA for idea submits
res <- aov(session2_numberOfSubmits ~ C(session1_ideatorType) * C(session2_condition), data = session2_data)
summary(res)

# ANOVA for idea max novelty (.)
df <-session2_data
# df <- df[!(df$session2_condition == 'on-idle' & df$session2_numberOfRequests < 3), ]
# df <- df[df$session2_numberOfSubmits > 2, ]
# df <- df[!(df$session1_ideatorType == 'AVOIDER' & df$session2_condition == 'on-demand' & df$session2_numberOfRequests > 2), ]
# df <- df[df$session1_numberOfSessionBlurred < 1 | df$session2_numberOfSessionBlurred < 1, ]
res <- aov(novelty_max ~ session1_ideatorType * session2_condition, data = df)
summary(res)
plot(res, 1)
plot(res, 2)
plot(res, 3)
TukeyHSD(res)
Anova(res, type = "III")
res <- lm(novelty_max ~ session1_ideatorType * session2_condition, data = df)
summary(res)
durbinWatsonTest(res)

# ANOVA for idea max value
res <- aov(value_max ~ C(session1_ideatorType) * C(session2_condition), data = session2_data)
summary(res)

# ANOVA for idea mean novelty
res <- aov(novelty_mean ~ C(session1_ideatorType) * C(session2_condition), data = session2_data)
summary(res)

# ANOVA for idea mean value
res <- aov(value_mean ~ C(session1_ideatorType) * C(session2_condition), data = session2_data)
summary(res)

# ANOVA for idea min novelty
res <- aov(novelty_min ~ C(session1_ideatorType) * C(session2_condition), data = session2_data)
summary(res)

# ANOVA for idea min value
res <- aov(value_mean ~ C(session1_ideatorType) * C(session2_condition), data = session2_data)
summary(res)


res <- aov(novelty_max ~ tlx_frustration, data = session2_data)
summary(res)

pairwise.t.test(session2_data$novelty_max, session2_data$session2_condition : session2_data$session1_ideatorType,
                p.adjust.method = "BH")

#### Signifikanzen
# session1_submits => gender(*)
# novelty_max => tlx.performance(*), tlx.mental(*), tlx.frustration(.), novelty_min(*), session1_numberOfRequests(.)



### Mixed Linear Model

lmm <- lmer(session1_numberOfSubmits ~ session1_numberOfRequests + 
              # (1 | gender) +
              # (1 | inspirations.were.distracting) +
              # (1 | inspirations.were.diverse) +
              # (1 | better.ideas.with.inspirations) +
              # (1 | session2_numberOfRequests) +
              # (1 | age.group) +
              # good
              (1 | tlx.performance) +
              # good
              # (1 | tlx.frustration) +
              # (1 | tlx.effort) +
              # (1 | tlx.temporal) +
              # (1 | session1_numberOfRequests) +
              # good
              (1 | session1_numberOfSessionBlurred),# +
              # (1 | session2_numberOfSessionBlurred) +
              # (1 | age.group) +
              # (1 | tlx.mental),
            data = wihtout_unmotivated,
            REML = FALSE)
#summary(lmm)
lmm
Anova(lmm)
plot(lmm)
# library(multcomp)
# summary(glht(lmm, test = adjusted("holm")))

lmm <- lmer(session2_numberOfSubmits ~ session1_ideatorType 
              # + (1 | gender)
              # + (1 | inspirations.were.distracting)
              # + (1 | inspirations.were.diverse)
              # + (1 | better.ideas.with.inspirations)
              # + (1 | session2_numberOfRequests)
              # + (1 | request.timing)
              # + (1 | satisfied.with.inspirations)
              # good
              + (1 | age.group)
              # good
              + (1 | tlx.performance)
              # + (1 | tlx.frustration)
              + (1 | tlx.effort)
              # + (1 | tlx.temporal)
              # + (1 | tlx.mental)
              # + (1 | session1_numberOfRequests)
              # good
              + (1 | session1_numberOfSessionBlurred)
              # + (1 | session2_numberOfSessionBlurred)
              # + (1 | age.group)
            , data = only_seeker_or_avoider,
            REML = FALSE)
#summary(lmm)
#lmm
Anova(lmm)

