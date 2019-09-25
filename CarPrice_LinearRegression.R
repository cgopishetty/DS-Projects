
library(stringr)
# Load the file into R
carprice <- read.csv("CarPrice_Assignment.csv")


# removing model names from carname
carprice$CarName<-str_replace_all(carprice$CarName,"-"," ")

carprice$CarName<- sub(" .*", "", carprice$CarName)

#Correcting spelling mistakes

carprice$CarName<-str_replace_all(carprice$CarName,"maxda","mazda")

carprice$CarName<-str_replace_all(carprice$CarName,"Nissan","nissan")

carprice$CarName<-str_replace_all(carprice$CarName,"porshce","porsche")

carprice$CarName<-str_replace_all(carprice$CarName,"vokswagen","volkswagen")

carprice$CarName<-str_replace_all(carprice$CarName,"vw","volkswagen")

carprice$CarName<-str_replace_all(carprice$CarName,"toyouta","toyota")

summary(factor(carprice$CarName))


# Removing Car_ID as it is only ID and has no significance in achieving our target

carprice<-carprice[,-1]

##DUMMY VARIABLE CREATION. 

#Converting Symboling into Dummies

str(carprice$symboling)

    #Convert symboling into string as it is Categorical variable

carprice$symboling<- as.character(carprice$symboling)

summary(factor(carprice$symboling))

dummy_symboling <- data.frame(model.matrix( ~symboling, data = carprice))

dummy_symboling<-dummy_symboling[,-1]

carprice_1<-cbind(carprice[,-1],dummy_symboling)



#Converting CarName into Dummies

summary(factor(carprice_1$CarName))

dummy_carname <- data.frame(model.matrix( ~CarName, data = carprice_1))

dummy_carname<-dummy_carname[,-1]

carprice_2<-cbind(carprice_1[,-1],dummy_carname)


#Converting fueltype into continuos variable

summary(factor(carprice_2$fueltype))

levels(carprice_2$fueltype)<-c(1,0)
    # Diesel is 1, Gas is 0

carprice_2$fueltype <- as.numeric(levels(carprice_2$fueltype))[carprice_2$fueltype]

#Converting aspiration into continuos variable

summary(factor(carprice_2$aspiration))

levels(carprice_2$aspiration)<-c(1,0)
  # std is 1, turbo is 0

carprice_2$aspiration <- as.numeric(levels(carprice_2$aspiration))[carprice_2$aspiration]

#Converting doornumber into continuos variable

summary(factor(carprice_2$doornumber))

levels(carprice_2$doornumber)<-c(1,0)
  # four is 1, two is 0

carprice_2$doornumber <- as.numeric(levels(carprice_2$doornumber))[carprice_2$doornumber]


#Converting Carbody into Dummies

summary(factor(carprice_2$carbody))

dummy_carbody <- data.frame(model.matrix( ~carbody, data = carprice_2))

dummy_carbody<-dummy_carbody[,-1]

carprice_3<-cbind(carprice_2[,-4],dummy_carbody)


#Converting drivewheel into Dummies

summary(factor(carprice_3$drivewheel))


dummy_drivewheel <- data.frame(model.matrix( ~drivewheel, data = carprice_3))

dummy_drivewheel<-dummy_drivewheel[,-1]

carprice_4<-cbind(carprice_3[,-4],dummy_drivewheel)

#Converting enginelocation into continuos variable

summary(factor(carprice_4$enginelocation))

levels(carprice_4$enginelocation)<-c(1,0)
  # front is 1, rear is 0

carprice_4$enginelocation <- as.numeric(levels(carprice_4$enginelocation))[carprice_4$enginelocation]


#Converting enginetype into Dummies

summary(factor(carprice_4$enginetype))


dummy_enginetype <- data.frame(model.matrix( ~enginetype, data = carprice_4))

dummy_enginetype<-dummy_enginetype[,-1]

carprice_5<-cbind(carprice_4[,-10],dummy_enginetype)

#Converting cylindernumber into Dummies

summary(factor(carprice_5$cylindernumber))

dummy_cylnum <- data.frame(model.matrix( ~cylindernumber, data = carprice_5))

dummy_cylnum<-dummy_cylnum[,-1]

carprice_6<-cbind(carprice_5[,-10],dummy_cylnum)



#Converting fuelsystem into Dummies

summary(factor(carprice_6$fuelsystem))

dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = carprice_6))

dummy_fuelsystem<-dummy_fuelsystem[,-1]

carprice_final<-cbind(carprice_6[,-11],dummy_fuelsystem)


# Deriving Car volume and Average Mileage

carprice_final$carvolume<-carprice_final$carlength*carprice_final$carwidth*carprice_final$carheight

carprice_final$avgmpg <- (carprice_final$citympg+carprice_final$highwaympg)/2




######### carprice_final Dataset is the dataset used to build Linear regression model   #####

# Divide into training and test data set
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(carprice_final), 0.7*nrow(carprice_final))
# generate the train data set
train = carprice_final[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = carprice_final[-trainindices,]


#### Linear Regression Model #####


Model_1<-lm(price~.,data=train)

summary(Model_1)

library(MASS)

library(carData)
library(car)

# Perform stepAIC to remove insignificant variables

Model_StepAIC<-stepAIC(Model_1,direction = "both")

Model_StepAIC

# Variables after stepaic

Model_2<-lm(formula = price ~ aspiration + enginelocation + carlength + 
              carheight + curbweight + enginesize + stroke + peakrpm + 
              highwaympg + symboling1 + CarNamebmw + CarNamebuick + CarNamedodge + 
              CarNamehonda + CarNamejaguar + CarNamemazda + CarNamemercury + 
              CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
              CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
              CarNametoyota + CarNamevolkswagen + carbodyhardtop + carbodyhatchback + 
              carbodysedan + carbodywagon + drivewheelrwd + enginetypeohc + 
              enginetyperotor + cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi + 
              carvolume, data = train)

summary(Model_2)

vif(Model_2)

# Removing car length as p value is high(though carvolume has high VIF,it's p value is very less)


Model_3<-lm(formula = price ~ aspiration + enginelocation + carheight + curbweight + enginesize + stroke + peakrpm + 
              highwaympg + symboling1 + CarNamebmw + CarNamebuick + CarNamedodge + 
              CarNamehonda + CarNamejaguar + CarNamemazda + CarNamemercury + 
              CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
              CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
              CarNametoyota + CarNamevolkswagen + carbodyhardtop + carbodyhatchback + 
              carbodysedan + carbodywagon + drivewheelrwd + enginetypeohc + 
              enginetyperotor + cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi + 
              carvolume, data = train)

summary(Model_3)
vif(Model_3)

# No the calrvolume has high VIF is not significant.So removing

Model_4<-lm(formula = price ~ aspiration + enginelocation + carheight + curbweight + enginesize + stroke + peakrpm + 
              highwaympg + symboling1 + CarNamebmw + CarNamebuick + CarNamedodge + 
              CarNamehonda + CarNamejaguar + CarNamemazda + CarNamemercury + 
              CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
              CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
              CarNametoyota + CarNamevolkswagen + carbodyhardtop + carbodyhatchback + 
              carbodysedan + carbodywagon + drivewheelrwd + enginetypeohc + 
              enginetyperotor + cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi, data = train)



summary(Model_4)
vif(Model_4)


# Removing carbodysedan as it has high VIF and high pvalue(enginesize and curbweight has high vif but are significant)

Model_5<-lm(formula = price ~ aspiration + enginelocation + carheight + curbweight + enginesize + stroke + peakrpm + 
              highwaympg + symboling1 + CarNamebmw + CarNamebuick + CarNamedodge + 
              CarNamehonda + CarNamejaguar + CarNamemazda + CarNamemercury + 
              CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
              CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
              CarNametoyota + CarNamevolkswagen + carbodyhardtop + carbodyhatchback + carbodywagon + 		drivewheelrwd + enginetypeohc + 
              enginetyperotor + cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi, data = train)



summary(Model_5)
vif(Model_5)

# Removing highwaympg as it has high VIF and high pvalue


Model_6<-lm(formula = price ~ aspiration + enginelocation + carheight + curbweight + enginesize + stroke + 		peakrpm +  symboling1 + CarNamebmw + CarNamebuick + CarNamedodge + 
              CarNamehonda + CarNamejaguar + CarNamemazda + CarNamemercury + 
              CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
              CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
              CarNametoyota + CarNamevolkswagen + carbodyhardtop + carbodyhatchback + carbodywagon + 				drivewheelrwd + enginetypeohc + 
              enginetyperotor + cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi, data = train)



summary(Model_6)
vif(Model_6)


# Removing carnamehonda as it has high vif and high pvalue

Model_7<-lm(formula = price ~ aspiration + enginelocation + carheight + curbweight + enginesize + stroke + 				peakrpm +  symboling1 + CarNamebmw + CarNamebuick + CarNamedodge + CarNamejaguar + CarNamemazda + 	CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
              CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
              CarNametoyota + CarNamevolkswagen + carbodyhardtop + carbodyhatchback + carbodywagon + 				drivewheelrwd + enginetypeohc + 
              enginetyperotor + cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi, data = train)



summary(Model_7)
vif(Model_7)

#Removing carheight as it has high pvalue

Model_8<-lm(formula = price ~ aspiration + enginelocation  + curbweight+ enginesize + stroke + 				peakrpm +  symboling1 + CarNamebmw + CarNamebuick + CarNamedodge + CarNamejaguar + CarNamemazda + 	CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
              CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
              CarNametoyota + CarNamevolkswagen + carbodyhardtop + carbodyhatchback + carbodywagon + 				drivewheelrwd + enginetypeohc + 
              enginetyperotor + cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi, data = train)



summary(Model_8)
vif(Model_8)

#Removing carbodyhardtop as it has high pvalue


Model_9<-lm(formula = price ~ aspiration + enginelocation  + curbweight+ enginesize + stroke + 				peakrpm +  symboling1 + CarNamebmw + CarNamebuick + CarNamedodge + CarNamejaguar + CarNamemazda + 	CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
              CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
              CarNametoyota + CarNamevolkswagen + carbodyhatchback + carbodywagon + 				drivewheelrwd + enginetypeohc + 
              enginetyperotor + cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi, data = train)



summary(Model_9)
vif(Model_9)

#Removing cylindernumberfive as it has high pvalue


Model_9<-lm(formula = price ~ aspiration + enginelocation  + curbweight+ enginesize + stroke + 				peakrpm +  symboling1 + CarNamebmw + CarNamebuick + CarNamedodge + CarNamejaguar + CarNamemazda + 	CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
              CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
              CarNametoyota + CarNamevolkswagen + carbodyhatchback + carbodywagon + 				drivewheelrwd + enginetypeohc + 
              enginetyperotor  + fuelsystem2bbl + fuelsystemmpfi, data = train)



summary(Model_9)
vif(Model_9)



#Removing carbodyhatchback as it has high pvalue


Model_10<-lm(formula = price ~ aspiration + enginelocation  + curbweight+ enginesize + stroke + 				peakrpm +  symboling1 + CarNamebmw + CarNamebuick + CarNamedodge + CarNamejaguar + CarNamemazda + 	CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
              CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
              CarNametoyota + CarNamevolkswagen + carbodywagon + 				drivewheelrwd + enginetypeohc + 
              enginetyperotor  + fuelsystem2bbl + fuelsystemmpfi, data = train)



summary(Model_10)
vif(Model_10)


##### There is no significant drop in R square from Model_1 to Model_10 #######

#Removing peakrpm as it has high pvalue


Model_11<-lm(formula = price ~ aspiration + enginelocation  + curbweight+ enginesize + stroke +  symboling1 + CarNamebmw + CarNamebuick + CarNamedodge + CarNamejaguar + CarNamemazda + 	CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
               CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
               CarNametoyota + CarNamevolkswagen + carbodywagon + 				drivewheelrwd + enginetypeohc + 
               enginetyperotor  + fuelsystem2bbl + fuelsystemmpfi, data = train)



summary(Model_11)
vif(Model_11)

#Removing carbodywagon as it has high pvalue

Model_12<-lm(formula = price ~ aspiration + enginelocation  + curbweight+ enginesize + stroke +  symboling1 + CarNamebmw + CarNamebuick + CarNamedodge + CarNamejaguar + CarNamemazda + 	CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
               CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
               CarNametoyota + CarNamevolkswagen + drivewheelrwd + enginetypeohc + 
               enginetyperotor  + fuelsystem2bbl + fuelsystemmpfi, data = train)



summary(Model_12)
vif(Model_12)


#Removing carbodywagon as it has high pvalue

Model_12<-lm(formula = price ~ aspiration + enginelocation  + curbweight+ enginesize + stroke +  symboling1 + CarNamebmw + CarNamebuick + CarNamedodge + CarNamejaguar + CarNamemazda + 	CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
               CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
               CarNametoyota + CarNamevolkswagen + drivewheelrwd + enginetypeohc + 
               enginetyperotor  + fuelsystem2bbl + fuelsystemmpfi, data = train)



summary(Model_12)
vif(Model_12)



#Removing carnamemercury as it has high pvalue

Model_13<-lm(formula = price ~ aspiration + enginelocation  + curbweight+ enginesize + stroke +  symboling1 + CarNamebmw + CarNamebuick + CarNamedodge + CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
               CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
               CarNametoyota + CarNamevolkswagen + drivewheelrwd + enginetypeohc + 
               enginetyperotor  + fuelsystem2bbl + fuelsystemmpfi, data = train)



summary(Model_13)
vif(Model_13)


#Removing fuelsystem2bbl as it has high pvalue

Model_14<-lm(formula = price ~ aspiration + enginelocation  + curbweight+ enginesize + stroke +  symboling1 + CarNamebmw + CarNamebuick + CarNamedodge + CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
               CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
               CarNametoyota + CarNamevolkswagen + drivewheelrwd + enginetypeohc + 
               enginetyperotor   + fuelsystemmpfi, data = train)



summary(Model_14)
vif(Model_14)


#Removing enginetypeohc as it has high pvalue

Model_15<-lm(formula = price ~ aspiration + enginelocation  + curbweight+ enginesize + stroke +  symboling1 + CarNamebmw + CarNamebuick + CarNamedodge + CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
               CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
               CarNametoyota + CarNamevolkswagen + drivewheelrwd + 
               enginetyperotor   + fuelsystemmpfi, data = train)



summary(Model_15)
vif(Model_15)


#Removing carnamesaab as it has high pvalue

Model_16<-lm(formula = price ~ aspiration + enginelocation  + curbweight+ enginesize + stroke +  symboling1 + CarNamebmw + CarNamebuick + CarNamedodge + CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
               CarNameporsche + CarNamerenault + CarNamesubaru + 
               CarNametoyota + CarNamevolkswagen + drivewheelrwd + 
               enginetyperotor   + fuelsystemmpfi, data = train)



summary(Model_16)
vif(Model_16)


#Removing carnamevolkswagen as it has high pvalue

Model_17<-lm(formula = price ~ aspiration + enginelocation  + curbweight+ enginesize + stroke +  symboling1 + CarNamebmw + CarNamebuick + CarNamedodge + CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
               CarNameporsche + CarNamerenault + CarNamesubaru + 
               CarNametoyota + drivewheelrwd + 
               enginetyperotor   + fuelsystemmpfi, data = train)



summary(Model_17)
vif(Model_17)

#Removing fuelsystemmpfi as it has high pvalue

Model_18<-lm(formula = price ~ aspiration + enginelocation  + curbweight+ enginesize + stroke +  symboling1 + CarNamebmw + CarNamebuick + CarNamedodge + CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
               CarNameporsche + CarNamerenault + CarNamesubaru + 
               CarNametoyota + drivewheelrwd + 
               enginetyperotor   , data = train)



summary(Model_18)
vif(Model_18)


#Removing carnamerenault as it has high pvalue

Model_19<-lm(formula = price ~ aspiration + enginelocation  + curbweight+ enginesize + stroke +  symboling1 + CarNamebmw + CarNamebuick + CarNamedodge + CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
               CarNameporsche + CarNamesubaru + 
               CarNametoyota + drivewheelrwd + 
               enginetyperotor   , data = train)



summary(Model_19)
vif(Model_19)

# Removing curbweight as it has high vif(though it is significant,to check if rsquare drops or not)


Model_20<-lm(formula = price ~ aspiration + enginelocation   + enginesize + stroke +  symboling1 + CarNamebmw + CarNamebuick + CarNamedodge + CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
               CarNameporsche + CarNamesubaru + 
               CarNametoyota + drivewheelrwd + 
               enginetyperotor   , data = train)



summary(Model_20)
vif(Model_20)


#Removing carnamepeugeut as it has high pvalue

Model_21<-lm(formula = price ~ aspiration + enginelocation   + enginesize + stroke +  symboling1 + CarNamebmw + CarNamebuick + CarNamedodge + CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamenissan +  CarNameplymouth + 
               CarNameporsche + CarNamesubaru + 
               CarNametoyota + drivewheelrwd + 
               enginetyperotor   , data = train)



summary(Model_21)
vif(Model_21)

#Removing symboling1 as it has high pvalue

Model_22<-lm(formula = price ~ aspiration + enginelocation   + enginesize + stroke + CarNamebmw + CarNamebuick + CarNamedodge + CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamenissan +  CarNameplymouth + 
               CarNameporsche + CarNamesubaru + 
               CarNametoyota + drivewheelrwd + 
               enginetyperotor   , data = train)



summary(Model_22)
vif(Model_22)

# Removing drivewheelrwd as it has high p value


Model_23<-lm(formula = price ~ aspiration + enginelocation  + enginesize + stroke + CarNamebmw + CarNamebuick + CarNamedodge + CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamenissan +  CarNameplymouth + 
               CarNameporsche + CarNamesubaru + 
               CarNametoyota + enginetyperotor   , data = train)



summary(Model_23)
vif(Model_23)

# Removing carnamemazda as it is insignificant compared to others


Model_24<-lm(formula = price ~ aspiration + enginelocation  + enginesize + stroke + CarNamebmw + CarNamebuick + CarNamedodge + CarNamejaguar + CarNamemitsubishi + CarNamenissan +  CarNameplymouth + 
               CarNameporsche + CarNamesubaru + 
               CarNametoyota + enginetyperotor   , data = train)



summary(Model_24)
vif(Model_24)

# Removing carnamenissan as it is insignificant compared to others


Model_25<-lm(formula = price ~ aspiration + enginelocation  + enginesize + stroke + CarNamebmw + CarNamebuick + CarNamedodge + CarNamejaguar + CarNamemitsubishi +   CarNameplymouth + 
               CarNameporsche + CarNamesubaru + 
               CarNametoyota + enginetyperotor   , data = train)



summary(Model_25)
vif(Model_25)


# Removing carnamedodge as it is insignificant compared to others


Model_26<-lm(formula = price ~ aspiration + enginelocation  + enginesize + stroke + CarNamebmw + CarNamebuick + CarNamejaguar + CarNamemitsubishi +   CarNameplymouth + 
               CarNameporsche + CarNamesubaru + 
               CarNametoyota + enginetyperotor   , data = train)



summary(Model_26)
vif(Model_26)


# Removing carnameplymouth as it is insignificant compared to others


Model_27<-lm(formula = price ~ aspiration + enginelocation  + enginesize + stroke + CarNamebmw + CarNamebuick + CarNamejaguar + CarNamemitsubishi + 
               CarNameporsche + CarNamesubaru + 
               CarNametoyota + enginetyperotor   , data = train)



summary(Model_27)
vif(Model_27)

# Removing carnamemitsubishi as it is insignificant compared to others


Model_28<-lm(formula = price ~ aspiration + enginelocation  + enginesize + stroke + CarNamebmw + CarNamebuick + CarNamejaguar  + 
               CarNameporsche + CarNamesubaru + 
               CarNametoyota + enginetyperotor   , data = train)



summary(Model_28)
vif(Model_28)

# Removing carnametoyota as it is insignificant compared to others


Model_29<-lm(formula = price ~ aspiration + enginelocation  + enginesize + stroke + CarNamebmw + CarNamebuick + CarNamejaguar  + 
               CarNameporsche + CarNamesubaru +  enginetyperotor   , data = train)



summary(Model_29)
vif(Model_29)

# Removing carnameporsche as it is insignificant compared to others
## cannot remove enginesize at this point as the r square drops to 0.7

Model_30<-lm(formula = price ~ aspiration + enginelocation  + enginesize + stroke + CarNamebmw + CarNamebuick + CarNamejaguar  + 
                CarNamesubaru +  enginetyperotor   , data = train)



summary(Model_30)
vif(Model_30)


# Removing carnamejaguar as it is insignificant compared to others
## cannot remove enginesize at this point as the r square drops to 0.7

Model_31<-lm(formula = price ~ aspiration + enginelocation  + enginesize + stroke + CarNamebmw + CarNamebuick + 
               CarNamesubaru +  enginetyperotor   , data = train)



summary(Model_31)
vif(Model_31)

# Removing carnamesubaru as it is insignificant compared to others
## cannot remove enginesize at this point as the r square drops to 0.7

Model_32<-lm(formula = price ~ aspiration + enginelocation  + enginesize + stroke + CarNamebmw + CarNamebuick + 
                +  enginetyperotor   , data = train)



summary(Model_32)
vif(Model_32)




# predicting the results in test dataset
Predict_carprice <- predict(Model_32,test[,-1])
test$test_price <- Predict_carprice

# Checking the R Square of the prediction
r <- cor(test$price,test$test_price)

rsquare <- cor(test$price,test$test_price)^2
rsquare


##########    Final Results    #############

#The Variables which are significant in prediction the price of car are

summary(Model_32)

#Price of a car =  15707.423 - 2774.312(aspiration) - 11082.09(enginelocation) - 2983.361(stroke) + 155.911(enginesize) + 7449.305(carenamebmw) + 5369.359(carnamebuick) + 9091.085(enginetypeotor)

# the predicted values from the model are able to explain 
rsquare # % of variation in the actual outcomes.


