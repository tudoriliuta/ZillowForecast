# setup
setwd("zillow-forecast/")
library(data.table)
library(FSelector)
library(rworldmap)
library(rworldxtra)

# load data
properties <- fread("data/properties_2016.csv")
training.set <- read.csv("data/train_2016_v2.csv")

# map
properties$longitude <- properties$longitude/1000000
properties$latitude <- properties$latitude/1000000

newmap <- getMap(resolution = "high")
plot(newmap, 
     xlim = c(min(properties$longitude, na.rm = TRUE), max(properties$longitude, na.rm = TRUE)), 
     ylim = c(min(properties$latitude, na.rm = TRUE), max(properties$latitude, na.rm = TRUE)), 
     asp = 1)
points(properties$longitude, properties$latitude, col = "red", cex = .1)

# Prepare Data
training.set[is.na(training.set)] <- 0
properties[is.na(properties)] <- 0

properties$censustractandblock <- factor(properties$censustractandblock)
training.set.merged <- merge(x = training.set, y = properties, by = "parcelid", all.x = TRUE)

# top features using gain ration
gain.ratio.feature.weights <- gain.ratio(logerror~., training.set.merged)
print(gain.ratio.feature.weights)
gain.ratio.top.features <- cutoff.k(gain.ratio.feature.weights, 10)
print(gain.ratio.top.features)

# top features using information gain
information.gain.feature.weights <- information.gain(logerror~., training.set.merged)
print(information.gain.feature.weights)
information.gain.top.features <- cutoff.k(information.gain.feature.weights, 10)
print(information.gain.top.features)

# time features
date.info <- unclass(as.POSIXlt(training.set.merged$transactiondate))
ls(date.info)
date.feature.names <- c("mday", "mon", "year", "yday")
training.set.merged[date.feature.names] <- date.info[date.feature.names]

# all features
training.set <- subset(training.set.merged, select = unique(c(gain.ratio.top.features, information.gain.top.features, date.feature.names, "logerror")))
test.set <- subset(properties, select = c(gain.ratio.top.features, information.gain.top.features))
results <- subset(properties, select= "parcelid")

# Predict
test.set$mday <- 1
test.set$mon <- 9
test.set$year <- 116
test.set$yday <- 274
linear.model <- lm(logerror ~ ., data = training.set)
pred.w.plim <- predict(linear.model, test.set)
results$first <- pred.w.plim

test.set$mon <- 10
test.set$yday <- 305
pred.w.plim <- predict(linear.model, test.set)
results$second <- pred.w.plim

test.set$mon <- 11
test.set$yday <- 335
pred.w.plim <- predict(linear.model, test.set)
results$third <- pred.w.plim

test.set$year <- 117
pred.w.plim <- predict(linear.model, test.set)
results$sixth <- pred.w.plim

test.set$mon <- 10
test.set$yday <- 305
pred.w.plim <- predict(linear.model, test.set)
results$fifth <- pred.w.plim

test.set$mon <- 9
test.set$yday <- 274
pred.w.plim <- predict(linear.model, test.set)
results$fourth <- pred.w.plim

# Save results
results <- results[, c(1,2,3,4,7,6,5)]
write.csv(results, "data/predictions/zillow.predictions.csv", na="0", row.names = FALSE)
