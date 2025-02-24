#install.packages("googlesheets4")
library(googlesheets4)
library(tidyr)
library(ggplot2)

#If you want to read it from google sheets use this line.
#But using this line requires you to sign into google.
#weightsdf=read_sheet("https://docs.google.com/spreadsheets/d/1shSRN1rfqWZu7ld4G8d2uME7OvtLLwhmoZZBFVjds84/edit?gid=0#gid=0")

#Alternativly, if you don't want to link to google,
#you can uncomment the lines below to build weightdf from scratch. 

person = c("Bob1","Bob2","Bob3","Bob4","Bob5","Bob6")
before = c(200,205,195,250,300,150)
after = c(196,202,193,248,290,149)
weightsdf <- data.frame(person, before, after)

weightsdf$diff <- weightsdf$after-weightsdf$before
weightsdf$diff2 <- weightsdf$before-weightsdf$after

weightsdf

weightsdf_long <- weightsdf %>%
  pivot_longer(cols = c(before, after), 
               names_to = "Drug", 
               values_to = "Weight")


  
#draw a plot of the weights before and after drug  
ggplot(weightsdf_long, aes(x=Drug, y=Weight, color=person))+geom_point()

#put before and after in the right order
weightsdf_long$Drug = factor(weightsdf_long$Drug, levels=c("before", "after"))

#redraw the same plot now that R knows what order we want before and after in
ggplot(weightsdf_long, aes(x=Drug, y=Weight, color=person))+geom_point()

#I want a line for each person
ggplot(weightsdf_long, aes(x = Drug, y = Weight, color = person, group = person)) +
  geom_point() +
  geom_line()

#Maybe this is not the best way to graph this. 
#What if I try a bar graph. 
ggplot(weightsdf_long, aes(y = as.numeric(Weight), x=interaction(Drug, person), fill = Drug, group=person)) +
    geom_bar(stat = "identity", position = "dodge")

#Ugh, the x axis labels are on top of each other 
ggplot(weightsdf_long, aes(y = as.numeric(Weight), x=interaction(Drug, person), fill = Drug, group=person)) +
  geom_bar(stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for better readability

#I am still not sure if the drug made people loose weight!


#calucate the means and variance for before and after
beforemean=mean(weightsdf$before)
beforesamplevar=var(weightsdf$before)
aftermean=mean(weightsdf$after)
aftersamplevar=var(weightsdf$after)
nafter=nrow(weightsdf)
nbefore=nrow(weightsdf)

print(beforesamplevar)
print(aftersamplevar)


# This is paired data because it shows weights of the same person before and after a drug.
# But what happens if we use the unpaired t-test even if we shouldn't.

#First I do it manually so you can see the equation
manttestunpaired = (beforemean-aftermean)/
  (sqrt((beforesamplevar/nbefore)+beforesamplevar/nafter))

manttestunpaired

t.test(weightsdf$before, weightsdf$after, paired=FALSE)

#Using the wrong test says they are not the same. 

# This is the right way to do the t-test since this is paired data. 

#First I do it manually so you can see the equation. 
#Notice we work with the mean of the difference of weights 
#rather than the mean of the weights.
manttestpaired = (mean(weightsdf$diff2))/
  (sd(weightsdf$diff)/sqrt(nbefore))
manttestpaired

#How to test it in R
t.test(weightsdf$before, weightsdf$after, paired=TRUE)


