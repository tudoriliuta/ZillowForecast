# setup
setwd("zillow-forecast/")
library(data.table)

# load and prepare data
properties <- fread("data/properties_2016.csv")
training.set <- read.csv("data/train_2016_v2.csv")

# Prepare Data
training.set[is.na(training.set)] <- 0
properties[is.na(properties)] <- 0
training.set.merged <- merge(x = training.set, y = properties, by = "parcelid", all.x = TRUE)
date.info <- unclass(as.POSIXlt(training.set.merged$transactiondate))
date.feature.names <- c("mday", "mon", "year", "yday")
training.set.merged[date.feature.names] <- date.info[date.feature.names]
training.set.merged$transactiondate <- NULL
training.set <- training.set.merged
test.set <- properties
results <- subset(properties, select= "parcelid")
rm(list = c("training.set.merged", "properties", "date.info", "date.feature.names"))

# unused.levels <- which(!(test.set$propertycountylandusecode %in% levels(training.set$propertycountylandusecode)))
# test.set$propertycountylandusecode[unused.levels] <- NA
test.set$propertycountylandusecode <- NULL
test.set$propertyzoningdesc <- NULL
training.set$propertycountylandusecode <- NULL
training.set$propertyzoningdesc <- NULL

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
write.csv(results, "data/predictions/zillow.predictions.2.csv", na="0", row.names = FALSE)