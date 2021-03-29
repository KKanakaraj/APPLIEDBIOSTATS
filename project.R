
rm(list=ls(all=TRUE)); graphics.off()   
library(GGally)
#data <- read.delim(dataname, header = FALSE, sep="")

data <- read.fwf("http://www.stat.ufl.edu/~winner/data/airline_costs.dat", 
         widths=c(20,8,8,8,8,8,8,8,8,8,8,8),header=FALSE)

colnames(data) <- c("airline","length", "speed", "dailytime", "population", "cost", "revenue", "tmlf", "capacity", "totassets", "investments", "adjassests")

data[, -which(names(data) %in% c("airline","tmlf", "totassets", "investments","revenue"))] <- apply(data[, -which(names(data) %in% c("airline","tmlf", "totassets", "investments","revenue"))], 2, log)

data <- na.omit(data)

summary(data)


#check if there are missing values
print(sapply(data,function(x) any(is.na(x))))

#check type of each columns 
print(sapply(data, typeof))

#for visualizing data we can plot the pairs in which cost is the outcome of interest 
pairs(data[ , c('cost', 'length', 'speed', 'dailytime', 'population', 'tmlf', 'capacity', 'adjassests')], pch = 16)

#visualize the relationship of couple of variables, their sample correlation 
# and their approximated density function: Relation between predictors and response

ggpairs(data[ , c('cost', 'length', 'speed', 'dailytime', 'population', 'tmlf', 'capacity', 'adjassests')], lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1)))


#fit complete linear model and have summary of coefficients
g = lm( cost ~ length + speed + dailytime + population + tmlf + capacity + adjassests, data = data )

summary(g)

coef(g)

#1. from median min max etc it seems quite symmetric 

#2. The null hypothesis Ho: the predictors do not really influence the outcome cost 
# so if p-value is low then reject HO 

# -->  we can see that tmlf and capacity has highest influence, then length and dailytime. 
# instead of population, speed and adjassests which are the ones with highest p value
# so less influent

#3.Through the F-statistic, we can investigate whether there is at least one 
#covariate’s parameter among B1,B2, B3, B4, B5, B6,B7 which is different from 0. 

# --> Since the p-value of F-statistic is so small, the null hypothesis 
#is rejected and there is at least one covariate’s parameter that is different from 0


# 4. In order to measure the goodness of fit of the model, we have to look at R2 and R2
#adj. They assume values between 0 and 1 and represent the percentage of 
#explained variability by regressors, thus the more they are #near to 1 the
#more the model explain well the dependent variable. 
#e.g if R2 is 0.02 means that model explain
#only 2% of data variability. But look at adjusted R2 since multiple R2 does not 
#decrease as more independent variables you add 

# --> here R2adj is 0.9536 so ok.

# 5. even if R2 seems pretty good let's look at residuals, the difference 
# between real value and predicted one, sum of residual should be lower as possible ->

#--> residual is 0.1399 -> quite low so ok 

plot( g$res, ylab = "Residuals", main = "Plot of residuals" ) #they have to seem random 


#check: with cooks plot you can see if there are points that can influence because follow a different pattern. 
#detecting influential points, the ones that could actually cause large change in model fit
# - outliers: point that does not fit the model 
# - leverage: point that affects a lot the estimates of the model 

#detect leverage: 
lev = hatvalues( g )

plot( g$fitted.values, lev, ylab = "Leverages", main = "Plot of Leverages",
      pch = 16, col = 'black' )
abline( h = 2 * p/n, lty = 2, col = 'red' )


cooks.distance(g)
plot(cooks.distance(g))       

influence.measures( g ) #the point with asterisk are important

# --> here we can see some points quite far from normal patter

#1.homoschedastcity 
g.resid <- residuals(g)
g.fitted <- fitted(g) 
plot(g, which = 1)

#same as doing 
plot(g.fitted, g.resid)


#2.normality
qqnorm( g$res, ylab = "Raw Residuals", pch = 16 )
qqline( g$res )
shapiro.test( g$res ) #since pvalue is 0.3 I have no enough significance to reject HO=gaussianity of data

qqnorm( rstandard( g ), ylab = "Studentized residuals", pch = 16 )
abline( 0, 1 )

#Histogram of residuals and boxplot are useful tools to look at 
#the shape of the residual distribution and to
#see if the tails oh the distributions are souspiciously heavy (i.e., there are many outliers) respectively.

hist( g$res, 10, probability = TRUE, col = 'lavender', main = 'residuals' )
boxplot( g$res, main = "Boxplot of savings residuals", pch = 16, col = 'lavender')


#to have all attributes of linear model object 
names(g)

g$call #formula
g$coefficients #beta_hat

g$fitted #estimated cost for each observation
#or we can compute directly the fitted values ->
X= model.matrix(g)
y_hat_mat= X %*% g$coefficients

g$residuals

g$rank # the numeric rank of the fitted linear model ->8 

#Calculate Variance-Covariance Matrix for a Fitted Model Object
vcov(g)


#model selection -> check for significance, looking at pvalues

#remove population
g_2 = lm( cost ~ length + speed + dailytime + tmlf + capacity + adjassests, data = data )
summary(g_2)

#remove speed
g_3 <- lm( cost ~ length + dailytime + tmlf + capacity + adjassests, data = data )
summary(g_3)

#remove length
g_3 <- lm( cost ~ dailytime + tmlf + capacity + adjassests, data = data )
summary(g_3)

#let's see the correlated variable


X= data[ , c('length', 'speed', 'dailytime', 'population', 'tmlf', 'capacity', 'adjassests')]
cor( X )

heatmap( cor( X ), Rowv = NA, Colv = NA, symm = TRUE, keep.dendro = F)

#image( as.matrix( cor( X ) ), main = 'Correlation of X' )

pairs( X )

#BIC method for model selection

BIC(g)

step( g, direction = "backward" , trace =T)
g_BIC_back = step( g, direction = "backward", k = log(31), trace = T)
# seems to have at the end only: dailytime, capacity, tmlf and adjassets. 
