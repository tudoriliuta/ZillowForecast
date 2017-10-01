# setup
setwd("Zillow House Prices/")
library(data.table)
library(FSelector)
library(rworldmap)
library(rworldxtra)

# load and prepare data
prop <- fread("properties_2016.csv")
trn <- read.csv("train_2016_v2.csv")

prop$censustractandblock <- factor(prop$censustractandblock)
prop$longitude <- prop$longitude/1000000
prop$latitude <- prop$latitude/1000000
fulltrn <- merge(x = trn, y = prop, by = "parcelid", all.x = TRUE)

# top features using gain ration
top.weights <- gain.ratio(logerror~., fulltrn)
print(top.weights)
gain.subset <- cutoff.k(top.weights, 5)
print(gain.subset)

# top features using information gain
top.weights <- information.gain(logerror~., fulltrn)
print(top.weights)
info.subset <- cutoff.k(top.weights, 5)
print(info.subset)

# map
newmap <- getMap(resolution = "high")
plot(newmap, 
     xlim = c(min(prop$longitude, na.rm = TRUE), max(prop$longitude, na.rm = TRUE)), 
     ylim = c(min(prop$latitude, na.rm = TRUE), max(prop$latitude, na.rm = TRUE)), 
     asp = 1)
points(prop$longitude, prop$latitude, col = "red", cex = .1)

# time features
date.info <- unclass(as.POSIXlt(fulltrn$transactiondate))
ls(date.info)
date.feature.names <- c("mday", "mon", "year", "wday", "yday")
fulltrn[feature.names] <- date.info[feature.names]

# all features
full.subset <- subset(fulltrn, select = unique(c(gain.subset, info.subset, date.feature.names, "logerror")))
prop.less <- subset(prop, select = c(gain.subset, info.subset))
res <- subset(prop, select= "parcelid")

# Predict
prop.less$mday <- 1
prop.less$mon <- 9
prop.less$year <- 116
linear.model <- lm(logerror ~ ., data = full.subset)
pred.w.plim <- predict(linear.model, prop.less, interval = "prediction")
res$first <- pred.w.plim[,"fit"]

prop.less$mon <- 10
pred.w.plim <- predict(linear.model, prop.less, interval = "prediction")
res$second <- pred.w.plim[,"fit"]

prop.less$mon <- 11
pred.w.plim <- predict(linear.model, prop.less, interval = "prediction")
res$third <- pred.w.plim[,"fit"]

prop.less$year <- 117
pred.w.plim <- predict(linear.model, prop.less, interval = "prediction")
res$sixth <- pred.w.plim[,"fit"]

prop.less$mon <- 10
pred.w.plim <- predict(linear.model, prop.less, interval = "prediction")
res$fifth <- pred.w.plim[,"fit"]

prop.less$mon <- 9
pred.w.plim <- predict(linear.model, prop.less, interval = "prediction")
res$fourth <- pred.w.plim[,"fit"]

# Save res
write.csv(res, "zillow.predictions.csv", na="0", row.names = FALSE)