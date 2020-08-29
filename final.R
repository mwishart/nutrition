library(ggplot2)
library(rpart)
library(rpart.plot)

nutrition <- read.delim("~/Data Science/final/nutrition.dat")
colnames(nutrition)

nutrition <- subset(nutrition, select = c(FOOD, CALORIES, PROTEIN, FAT,IRON,CAL_GRAM,IRN_GRAM, PRO_GRAM,FAT_GRAM))
nutrition <- na.omit(nutrition)

ggplot(nutrition, aes(CALORIES)) + geom_histogram(color = "orange", binwidth= 5)

max(nutrition$CALORIES)

nutrition$cal_bin <- cut(nutrition$CALORIES, breaks = c(0, 300,6175))

levels(nutrition$cal_bin) <- c("Low", "High")
summary(nutrition$cal_bin)
ggplot(nutrition, aes(FAT_GRAM, PRO_GRAM, color = cal_bin, shape = cal_bin)) + geom_point() +geom_smooth(se = FALSE) + xlab("Fat per Gram") + ylab("Protein per Game") +
  ggtitle("Protein per gram compared to Fat per gram overlayed with Calories")
  
ggplot(nutrition, aes(FAT_GRAM, IRN_GRAM, color= cal_bin, shape = cal_bin)) + geom_point() + geom_smooth(se=FALSE) + xlab("Fat per Gram") +
  ylab("Iron per Gram") + ggtitle("Fat per gram compared to Iron per gram overlayed with Calories")


nutrition$IRN_GRAM.Z <- scale(nutrition$IRN_GRAM)
nutrition$PRO_GRAM.Z <- scale(nutrition$PRO_GRAM)
nutrition$FAT_GRAM.Z <- scale(nutrition$FAT_GRAM)

set.seed(50)

choose <- runif(n = dim(nutrition)[1], min=0, max=1)
nutri.training <- nutrition[choose <= 0.75,]
nutri.testing <- nutrition[choose > 0.25,]

t.test(x = nutri.testing$CAL_GRAM, y = nutri.training$CAL_GRAM)$p.value
t.test(x = nutri.testing$IRN_GRAM, y = nutri.training$IRN_GRAM)$p.value
t.test(x = nutri.testing$PRO_GRAM, y = nutri.training$PRO_GRAM)$p.value
t.test(x = nutri.testing$FAT_GRAM, y = nutri.training$FAT_GRAM)$p.value
train.flag <- ifelse(choose <= 0.75, 1, 0)
cal.stat.table <- table(nutrition$CAL_GRAM, train.flag)
irn.stat.table <- table(nutrition$IRN_GRAM, train.flag)
pro.stat.table <- table(nutrition$PRO_GRAM, train.flag)
fat.stat.table <- table(nutrition$FAT_GRAM, train.flag)
prop.test(cal.stat.table, correct = FALSE)$p.value
prop.test(irn.stat.table, correct = FALSE)$p.value
prop.test(pro.stat.table, correct = FALSE)$p.value
prop.test(fat.stat.table, correct = FALSE)$p.value

round(prop.table(table(nutrition$cal_bin))*100, 2)




cart02 <- rpart( cal_bin ~ IRN_GRAM.Z + PRO_GRAM.Z + FAT_GRAM.Z,
                 data = nutrition,
                 method = "class")
rpart.plot(cart02, type = 4, extra = 108)



cart <- rpart( cal_bin ~ IRN_GRAM + PRO_GRAM + FAT_GRAM,
                 data = nutri.training,
                 method = "class")
rpart.plot(cart, type = 4, extra = 108)





