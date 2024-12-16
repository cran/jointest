## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------

data("oasis",package="jointest")

# from here on 0 means Age=77 (which is about the mean)
oasis$Age=oasis$Age-77

## -----------------------------------------------------------------------------
library(ggplot2)
p <- ggplot(oasis,aes(Group,nWBV,color=Group))
p+geom_point(size = 3) +geom_boxplot(alpha=.1) + theme_bw()

p <- ggplot(oasis,aes(Age,nWBV,color=Group))
p+geom_point(size = 3) +geom_smooth(method = lm) + theme_bw()

p=ggplot(oasis, aes(x = Age, y = nWBV, colour = Group, group = Subject.ID)) + geom_line(aes(linetype=Gender)) + geom_point()
p + theme_bw()


## -----------------------------------------------------------------------------
library(lme4)
library(ggplot2)


mod=lmer(nWBV ~ Age*Group+Gender*Group+ (1+Age|Subject.ID),data=oasis)
summary(mod)
anova(mod)



## -----------------------------------------------------------------------------
library(jointest)

mod=flip2sss(nWBV ~ Age*Group+Gender*Group, 
             cluster =oasis$Subject.ID,data=oasis)
summary(mod)

#ANOVA-like combination
#Overall
summary(combine(mod))
#by Variables
summary(combine_contrasts(mod))


# p <- ggplot(oasis1lev,aes(Group,Interc,color=Group))
# p+geom_point(size = 3) +geom_boxplot(alpha=.1) + theme_bw()
# 
# 
# p <- ggplot(oasis1lev,aes(Group,Slope,color=Group))
# p+geom_point(size = 3) +geom_boxplot(alpha=.1) + theme_bw()
# 


