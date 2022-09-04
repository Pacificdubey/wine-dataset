library(ggplot2)
red_wine_qual <-  read.csv("C:/Users/Prashant/Desktop/winequality-red.csv",sep=";")

# sample size
dim(red_wine_qual)
colnames(red_wine_qual)


# outliers detection
oldpar = par(mfrow = c(2,6))
for ( i in 1:11 ) {
  boxplot(red_wine_qual[[i]])
  mtext(names(red_wine_qual)[i], cex = 0.8, side = 1, line = 2)
}
par(oldpar)

# Calculate Actual Outliers
boxplot.stats(red_wine_qual[[1]])$out

for ( i in 1:11 ) {
  print("*******************************")
  print(colnames(red_wine_qual)[i])
  a = boxplot.stats(red_wine_qual[[i]])$out
  print(a)
  print("*****************************")
  print(" " )
}

# summary of dataset
summary(red_wine_qual)


#-------------Distribution of each variable-----------------------------------#
#1 QUality Vairable
#histogram
ggplot(aes(x = quality),data = red_wine_qual) +
  geom_histogram(binwidth=1,color="darkblue", fill="lightblue") + 
  xlab('Quality') +
  ylab('Count') +
  ggtitle('Quality Data Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))
  

# Density Plot
ggplot(aes(x = quality),data = red_wine_qual) + 
  geom_density(fill = "seagreen",
               color = "midnightblue") +
  xlab('Quality') +
  ylab('Density') +
  ggtitle('Quality Data Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))

table(red_wine_qual$quality)

# --------------------------------Fixed Acidity-----------------------------#
ggplot(data = red_wine_qual, aes(x = fixed.acidity)) +
  geom_histogram(binwidth = 1, color = 'black',fill = I('orange')) +
  xlab('Fixed Acidity g/dm^3') +
  ylab('Count') +
  ggtitle('Acidity Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))


# Density Plot
ggplot(aes(x = fixed.acidity),data = red_wine_qual) + 
  geom_density(fill = "seagreen",
               color = "midnightblue") +
  xlab('Fixed Acidity g/dm^3') +
  ylab('Count') +
  ggtitle('Acidity Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))

# table
table(red_wine_qual$fixed.acidity)

#-------------------------------VOlatile Acidity---------------------------#

ggplot(data = red_wine_qual, aes(x = volatile.acidity)) +
  geom_histogram(binwidth = 0.05, color = 'black',fill = I('Blue')) +
  xlab('Volatile Acidity g/dm^3') +
  ylab('Count') +
  ggtitle('Volatile Acidity Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))

# Density Plot
ggplot(data = red_wine_qual, aes(x = volatile.acidity))+
  geom_density(fill = "slateblue1",
               color = "midnightblue")+
  xlab('Volatile Acidity g/dm^3') +
  ylab('Count') +
  ggtitle('Volatile Acidity Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))
  
# table
table(red_wine_qual$volatile.acidity)

#------------------------------------citric Acid--------------------------------#


ggplot(data = red_wine_qual, aes(x = citric.acid)) +
  geom_histogram(binwidth = 0.08, color="darkblue", fill="antiquewhite") +
  scale_x_continuous(breaks = seq(0,1,0.1), lim = c(0,1)) +
  xlab('Citric Acid g/dm^3') +
  ylab('Count') +
  ggtitle('CItric Acid Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))



ggplot(data = red_wine_qual, aes(x = citric.acid)) +
  geom_density(fill = "cadetblue2",
               color = "midnightblue") +
  xlab('Citric Acid g/dm^3') +
  ylab('Count') +
  ggtitle('CItric Acid Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))

# table
table(red_wine_qual$citric.acid)

#----------------------------------------Residual Sugar--------------------------------#

ggplot(data = red_wine_qual, aes(x = residual.sugar)) +
  geom_histogram(binwidth = 0.1, color="darkblue", fill="aquamarine") +
  xlab('Residual Sugar g/dm^3') +
  ylab('Count') +
  ggtitle('Residual Sugar Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))

# density plot
ggplot(data = red_wine_qual, aes(x = residual.sugar)) +
  geom_density(fill = "chartreuse",
               color = "midnightblue") +
  xlab('Residual Sugar g/dm^3') +
  ylab('Count') +
  ggtitle('Residual Sugar Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))

# table
table(red_wine_qual$residual.sugar)

#----------------------------------Chlorides-------------------------------#
ggplot(data = red_wine_qual, aes(x = chlorides)) +
  geom_histogram(binwidth = 0.01,  color="darkblue", fill="chocolate1")+
  xlab('Chlorides g/dm^3') +
  ylab('Count') +
  ggtitle('Chlorides Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))

# density plot
ggplot(data = red_wine_qual, aes(x = chlorides)) +
  geom_density(fill = "chocolate3",
               color = "midnightblue") +
  xlab('Chlorides g/dm^3') +
  ylab('Count') +
  ggtitle('Chlorides Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))

# table
table(red_wine_qual$chlorides)



#----------------------------free Sulfur Dioxide-------------------------#

ggplot(data = red_wine_qual, aes(x = free.sulfur.dioxide)) +
  geom_histogram(binwidth = 1, color="darkblue", fill="coral") +
  scale_x_continuous(breaks = seq(0,80,5)) +
  xlab('Free Sulfur Dioxide mg/dm^3 ') +
  ylab('Count') +
  ggtitle('Free Sulfur Dioxide Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))

# density plot 
ggplot(data = red_wine_qual, aes(x = free.sulfur.dioxide))+
  geom_density(fill = "cornflowerblue",
               color = "midnightblue") +
  xlab('Free Sulphar Dioxide mg/dm^3 ') +
  ylab('Count') +
  ggtitle('Free Sulfur Dioxide Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))

table(red_wine_qual$free.sulfur.dioxide)

#------------------------------Total Suplfur Dioxide--------------------#

ggplot(data = red_wine_qual, aes(x = total.sulfur.dioxide)) +
  geom_histogram(binwidth = 5,color="darkblue", fill="blue2") +
  xlab('Total Sulfur Dioxide mg/dm^3 ') +
  ylab('Count') +
  ggtitle('Total Sulfur Dioxide Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))

# density plot
ggplot(data = red_wine_qual, aes(x = total.sulfur.dioxide)) +
  geom_density(fill = "cyan",
               color = "midnightblue") +
  xlab('Total Sulfur Dioxide mg/dm^3 ') +
  ylab('Count') +
  ggtitle('Total Sulfur Dioxide Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))

# table
table(red_wine_qual$total.sulfur.dioxide)
  
#-----------------------------Density--------------------------------------#
#Density(Has a very normal distribution)

ggplot(data = red_wine_qual, aes(x = density)) +
  geom_histogram(binwidth = 0.001,color="darkblue", fill="brown1")+
  xlab('Density g/cm^3 ') +
  ylab('Count') +
  ggtitle('Density Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))

# density plot
ggplot(data = red_wine_qual, aes(x = density)) +
  geom_density(fill = "brown2",
               color = "midnightblue") +
  xlab('Density g/cm^3 ') +
  ylab('Count') +
  ggtitle('Density Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))

# table
table(red_wine_qual$density)


#--------------------------------PH-----------------------------------------------#
#pH(Has a very normal distribution)
ggplot(data = red_wine_qual, aes(x = pH)) +
  geom_histogram(binwidth = 0.1,color="darkblue", fill="deepskyblue2") +
  xlab('pH ') +
  ylab('Count') +
  ggtitle('pH Scale Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))


# density plot
ggplot(data = red_wine_qual, aes(x = pH)) +
  geom_density(fill = "deepskyblue3",
               color = "midnightblue") +
  xlab('pH ') +
  ylab('Count') +
  ggtitle('pH Scale Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))

# table
table(red_wine_qual$pH)

#--------------------------------Sulphate-----------------------------------#

#Sulphates(Positively skewed. Similar to Chlorides and residual sugar)

ggplot(data = red_wine_qual, aes(x = sulphates)) +
  geom_histogram(binwidth = 0.1,color="darkblue", fill="darkorchid1") +
  xlab('Sulphate g/dm^3  ') +
  ylab('Count') +
  ggtitle('Sulphate Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))

# density plot
ggplot(data = red_wine_qual, aes(x = sulphates)) +
  geom_density(fill = "darkorchid2",
               color = "midnightblue") +
  xlab('Sulphate g/dm^3  ') +
  ylab('Count') +
  ggtitle('Sulphate Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))

# table
table(red_wine_qual$sulphates)

#-----------------------------------Alcohol-------------------------------#

ggplot(data = red_wine_qual, aes(x = alcohol)) +
  geom_histogram(binwidth = 0.1,color="darkblue", fill="darkolivegreen2") +
  xlab('Alcohol %') +
  ylab('Count') +
  ggtitle('Alcohol Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))

# density plot
ggplot(data = red_wine_qual, aes(x = alcohol)) +
  geom_density(fill = "darkolivegreen3",
               color = "midnightblue") +
  xlab('Alcohol %') +
  ylab('Count') +
  ggtitle('Alcohol Distribution') +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  theme(axis.title = element_text(face="bold")) +
  theme(axis.text.x = element_text(face="bold",size=12),
        axis.text.y = element_text(face="bold",size=12))

# table
table(red_wine_qual$alcohol)



































