#install.packages("googlesheets4")
library(googlesheets4)
library(tidyr)
library(ggplot2)

weightsdf=read_sheet("https://docs.google.com/spreadsheets/d/1shSRN1rfqWZu7ld4G8d2uME7OvtLLwhmoZZBFVjds84/edit?gid=0#gid=0")
weightsdf$diff <- weightsdf$after-weightsdf$before
weightsdf$diff2 <- weightsdf$before-weightsdf$after
weightsdf

beforemean=mean(weightsdf$before)
beforesamplevar=var(weightsdf$before)
aftermean=mean(weightsdf$after)
aftersamplevar=var(weightsdf$after)
nafter=nrow(weightsdf)
nbefore=nrow(weightsdf)

print(beforesamplevar)
print(aftersamplevar)

manttestunpaired = (beforemean-aftermean)/
  (sqrt((beforesamplevar/nbefore)+beforesamplevar/nafter))

manttestunpaired

t.test(weightsdf$before, weightsdf$after, paired=FALSE)


manttestpaired = (mean(weightsdf$diff2))/
  (sd(weightsdf$diff)/sqrt(nbefore))
manttestpaired

t.test(weightsdf$before, weightsdf$after, paired=TRUE)

weightsdf_long <- weightsdf %>%
  pivot_longer(cols = c(before, after), 
               names_to = "time", 
               values_to = "value")

ggplot(weightsdf_long, aes(x=time, y=value))+geom_point()