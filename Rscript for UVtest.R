require(ggplot2)
require(tidyr)
require(lmerTest)
require(doBy)
require(lattice)
###absorbance curb data
uv <- read.csv(file="Majorsplatemastersheet.csv", sep=",",head=TRUE, na.string="na")
###remove arbuscula
uv <- uv[-c(367), ]
uv <- uv[-c(32:36), ]

##reshape for plotting
comp_uv <- tidyr::gather(uv,"wavelength","absorbance",6:19)

comp_uv <- na.omit(comp_uv)
uv_sum1 <- summaryBy(absorbance~pop+ssp+wavelength, data=comp_uv, FUN=c(mean,min,max))

#COMPARE LMER CIs to ORIGINAL CIs
require(plyr)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) 

  # New version of length which can handle NA's: if na.rm==T, don't count them
  {length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # Collapse the data
  formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
  datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)
  
  # Rename columns
  names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
  names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
  names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N"
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
uv_sum2 <- summarySE(comp_uv, measurevar="absorbance", groupvars=c("wavelength","ssp"))
uv_sum3 <- summarySE(comp_uv, measurevar="absorbance", groupvars=c("wavelength","type"))



###plotting
w <- ggplot(uv_sum2, aes(wavelength, absorbance,color=ssp))
w + geom_point() + geom_line(aes(group=ssp)) + geom_errorbar(aes(ymin = (absorbance +ci), ymax = (absorbance - ci), width = 0.2)) + theme_bw()

w <- ggplot(uv_sum3, aes(wavelength, absorbance,color=type))
w + geom_point() + geom_line(aes(group=type)) + geom_errorbar(aes(ymin = (absorbance +ci), ymax = (absorbance - ci), width = 0.2)) + theme_bw()



###lmer
lmer_uv <- lmer(X330nm ~  lat + elev + (1|ssp:pop),  data=uvclimate)
summary(lmer_uv)
rand(lmer_uv)
re_pop <- ranef(lmer_uv, condVar=TRUE, whichel = "ssp:elev")
dotplot(re_pop)


###UV data @330nm and climate data
#uvclimate <- read.csv(file="UV-climate.csv", sep=",",head=TRUE, na.string="na")
#uvclimatesum <- summaryBy(X330nm ~ pop+ssp+elev+lat, FUN=c(mean), data=uvclimate)