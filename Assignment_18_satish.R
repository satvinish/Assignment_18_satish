#decision tree
input<- Example_WearableComputing_weight_lifting_exercises_biceps_curl_variations1
View(input)
install.packages("caTools")
library(caTools)
install.packages('tree')
library(tree)
set.seed(1)
split<- sample.split(input$user_name, SplitRatio = 0.70)
inputTrain<- subset(input, split == TRUE)
inputTest<- subset(input, split == FALSE)
table(input$user_name)
table(inputTrain$user_name)
table(inputTest$user_name)
prop.table(table(inputTrain$user_name)+table(inputTest$user_name))
modelClassTree<- tree(user_name~raw_timestamp_part_1+raw_timestamp_part_2+cvtd_timestamp+new_window+num_window+roll_belt+pitch_belt+yaw_belt+
total_accel_belt+gyros_belt_x+gyros_belt_y+gyros_belt_z+accel_belt_x+accel_belt_y+accel_belt_z+magnet_belt_x+magnet_belt_y+magnet_belt_z+roll_arm+
pitch_arm+yaw_arm+total_accel_arm+gyros_arm_x+gyros_arm_y+gyros_arm_z+accel_arm_x+accel_arm_y+accel_arm_z+magnet_arm_x+magnet_arm_y+magnet_arm_z+
roll_dumbbell+pitch_dumbbell+yaw_dumbbell+total_accel_dumbbell+gyros_dumbbell_x+gyros_dumbbell_y+gyros_dumbbell_z+accel_dumbbell_x+accel_dumbbell_y+
accel_dumbbell_z+magnet_dumbbell_x+magnet_dumbbell_y+magnet_dumbbell_z+roll_forearm+pitch_forearm+yaw_forearm+total_accel_forearm+gyros_forearm_x+
gyros_forearm_y+gyros_forearm_z+accel_forearm_x+accel_forearm_y+accel_forearm_z+magnet_forearm_x+magnet_forearm_y+magnet_forearm_z+classe, data = inputTrain)
plot(modelClassTree)
text(modelClassTree, pretty = 0, cex = 0.75)
pred<- predict(modelClassTree, newdata = inputTest, type = "class")
conf<- table(inputTest$user_name, pred)
conf
oaa<- (conf[1,1]+conf[2,2]+conf[3,3]+conf[4,4]+conf[5,5])/sum(conf)
oaa

#pruning
train<- sample(1:nrow(input), 1000)
input.test<- input[-train,]
High.test<- High[-train]
library(tree)
tree.input1<- tree(High~.-user_name, input, subset = train)
tree.pred<- predict(tree.input1, input.test, type = 'class')
table(tree.pred, High.test)
(86+57/200)
summary(tree.input1)
set.seed(3)
cv.input<- cv.tree(tree.input1, FUN = prune.misclass)
names(cv.input)
cv.input
par(mfrow = c(1,2))
plot(cv.input$size, cv.input$dev, type = 'b', col = "red", lwd = 2)
plot(cv.input$k, cv.input$dev, type = 'b', col = "blue", lwd = 2)
prune.input<- prune.misclass(tree.input1, best = 9)
plot(prune.input)
text(prune.input, pretty = 0)
tree.pred1<- predict(prune.input, input.test, type = 'class')
table(tree.pred1, High.test)                     
#(94+60)/2
#random forest, bagging
library(randomForest)
set.seed(1)
bag.input<- randomForest(High~.-user_name, input, subset = train, mtry = 10, importance = TRUE)
dim(input)
importance(bag.input)
varImpPlot(bag.input, col = 'red', pch = 10, cex = 1.25)
bag.input
test.pred.bag<- predict(bag.input, newdata = input[-train, ], type = 'class')
table(test.pred.bag, High.test)
#(96+66)/2
set.seed(1)
rf.input<- randomForest(High~.-user_name, input, subset = train, mtry = 3, importance = TRUE)
dim(input)
importance(rf.input)
varImpPlot(rf.input, col = 'blue', pch = 10, cex = 1.25)
rf.input
test.pred.rf<- predict(rf.input, newdata = input[-train, ], type = 'class')
table(test.pred.rf, High.test)
#(98+63)/2
