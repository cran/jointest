% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combine-methods.R
\docType{methods}
\name{combine-methods}
\alias{combine-methods}
\alias{combine}
\alias{combine_contrasts}
\title{Nonparametric combination of \code{jointest} objects}
\usage{
combine(mods, comb_funct = "maxT", by = NULL, by_list=NULL, tail = 0)

combine_contrasts(mods, comb_funct = "Mahalanobis", tail = 0)
}
\arguments{
\item{mods}{a \code{jointest} object.}

\item{comb_funct}{combining function to be used. 
Several functions are implemented: "mean", "median", "Fisher", "Liptak", (equal to) "Stoufer", "Tippet", (equal to) "minp", "maxT", "Mahalanobis". 
Alternatively it can be a custom function that has a Tspace matrix as input. 
For \code{combine} the default is \code{comb_funct="maxT"},
while for \code{combines_contrasts} the default is \code{comb_funct="Mahalanobis"}.}

\item{by}{if \code{NULL} (default), it combines all test statistics.
If a characters, it refers to the column's name of \code{summary_table} (and printed by something like \code{summary(mods)}). 
The elements with the same value will be combined. If \code{by} is a vector, the values are defined by row-wise concatenation of the values of the columns in \code{by}.
The argument is inactive if \code{by_list} is not \code{NULL}.}

\item{by_list}{NULL (default) or a list of vectors. For each vector of the list it combines test statistics with position given by the element of the vector. 
If the vectors in the list are characters, these refer to names(mods$Tspace).}

\item{tail}{direction of the alternative hypothesis. It can be "two.sided" (or 0, the default), "less" (or -1) or "greater" (or +1).}
}
\value{
The function returns a \code{jointest}-object.
}
\description{
Methods for combining \code{jointest} objects.

\code{combine} combines the tests derived from multiverse models.

\code{combine_contrasts} combines the tests derived from the contrasts of a factor variable to get a 
global test for the factor (i.e. categorical predictor). 
It has strong analogies with ANOVA test.
}
\examples{
#First example
library(jointest)
set.seed(123)

#Simulate data
n=20
D=data.frame(X=rnorm(n),Z1=rnorm(n),Z2=rnorm(n))
D$Y=D$Z1+D$X+rnorm(n)

# Run four glms abd combine it in a list
mod1=glm(Y~X+Z1+Z2,data=D)
mod2=glm(Y~X+poly(Z1,2)+Z2,data=D)
mod3=glm(Y~X+poly(Z1,2)+poly(Z2,2),data=D)
mod4=glm(Y~X+Z1+poly(Z2,2),data=D)
mods=list(mod1=mod1,mod2=mod2,mod3=mod3,mod4=mod4)

# Let us analyze the tests related to coefficient "X" and combine them
res=join_flipscores(mods,n_flips = 5000, seed = 1, tested_coeffs = "X")
summary(combine(res))
# Second (continued) example
# flipscores jointly on all models and all coefficients
res=join_flipscores(mods,n_flips = 2000)
summary(combine(res))
summary(combine(res, by="Model"))
summary(combine(res, by="Coeff"))
res2=combine_contrasts(res)
summary(res2)
#custom combinations:
coeffs=c("(Intercept)","X","Z1","Z2")
coeffs_ids=lapply(coeffs,grep,res2$summary_table$Coeff)
names(coeffs_ids)=coeffs
summary(combine(res2,by_list =   coeffs_ids))
}
