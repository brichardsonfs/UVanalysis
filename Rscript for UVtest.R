require(ggplot2)
require(tidyr)
require(lmerTest)
require(doBy)
require(lattice)

uv <- read.csv(file="ephraimplatemastersheet.csv", sep=",",head=TRUE, na.string="na")
uv <- uv[-c(367), ]
##reshape for plotting
comp_uv <- tidyr::gather(uv,"wavelength","absorbance",4:17)


comp_uv <- na.omit(comp_uv)
uv_sum1 <- summaryBy(absorbance~pop+ssp+wavelength, data=comp_uv, FUN=c(mean,min,max))

###plotting
w <- ggplot(uv_sum1, aes(wavelength, absorbance.mean,color=ssp, label=pop))
w + geom_point() + geom_line(aes(group=pop)) + geom_text() + theme_bw()
w + geom_point() + geom_line(aes(group=pop)) + geom_linerange(aes(ymin = absorbance.min, ymax = absorbance.max))

z <- ggplot(uv, aes(pop,X330,fill=ssp,label=pop))
z + geom_boxplot()


###lmer
lmer_uv <- lmer(X330 ~  ssp + (1|ssp:pop),  data=uv)
summary(lmer_uv)
rand(lmer_uv)
re_pop <- ranef(lmer_uv, condVar=TRUE, whichel = "ssp:pop")
dotplot(re_pop)
