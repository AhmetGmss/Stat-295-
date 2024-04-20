#Quetion 3
#i
nmmaps <- read.csv("chicago-nmmaps-custom.csv")

nmmaps$date<-as.Date(nmmaps$date)

library(ggplot2)

ggplot(nmmaps,aes(x=date,y=temp))+
  geom_point() + facet_wrap(~ year,scales="free_x")+
  labs(title="Relationsihp Between Date and Temperatrure by Year")

#interpretation: Between 1997 and 2000, air temperatures start to increase
# from January onwards and reach their peak in July and start to cool down from July to January.

#ii
ggplot(nmmaps,aes(x=date,y=temp,z=season,color=season))+
  geom_point()+facet_wrap(~season,scales = "free_x")+
  labs(title="Relationship Between Date,Temp And Season")

#iii
ggplot(nmmaps, aes(x = dewpoint, y = temp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Adding linear regression line
  annotate("text", x = 40, y = 100, label = paste("Correlation:", round(cor(nmmaps$dewpoint, nmmaps$temp), 2))) + # Adding correlation text
  labs(title = "Relationship Between Temp and Dewpoint")

#interpretation: There is a positive correlation between dewpoint and temp.
# Both of them increase in direct proportion to each other and our correlation line shows us this.