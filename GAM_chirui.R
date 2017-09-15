# using GAM to construct formula

# clear working space
rm(list = ls())

# packages uploaded
library("nlme")
library("mgcv")

setwd("/Users/Huxtable/Desktop/chirui")

##############################################
### Please input your parameters here !!!    #
# please input you lagTime here              #
#                                            #
lagTime = 4                                  #
movingTime = 7                               #
#                                            #
#                                            #
##############################################


# read the data
GAM_CHI <- read.table("total.csv", header = TRUE, sep = ',', fileEncoding = "GBK")

# delete the New_No. column
GAM_CHI$New_No. <- NULL

# sort GAM_CHI by timesSeries
GAM_CHI <- GAM_CHI[order(GAM_CHI$timeSeries),]
row.names(GAM_CHI) <- c(1 : 1096)

# clean the data at first
for(i in 1:1096){
    # there are 
    
    if(is.na(GAM_CHI$egvs_res[i]))
        GAM_CHI$egvs_res[i] = 0

    if(is.na(GAM_CHI$egvs_t2dm[i]))
        GAM_CHI$egvs_t2dm[i] = 0

    if(is.na(GAM_CHI$egvs_xmz[i]))
        GAM_CHI$egvs_xmz[i] = 0

    if(is.na(GAM_CHI$egvs_btty[i]))
        GAM_CHI$egvs_btty[i] = 0

    if(is.na(GAM_CHI$egvs_skin[i]))
        GAM_CHI$egvs_skin[i] = 0

    if(is.na(GAM_CHI$egvs_pharyngitis[i]))
        GAM_CHI$egvs_pharyngitis[i] = 0

    if(is.na(GAM_CHI$egvs_otit[i]))
        GAM_CHI$egvs_otit[i] = 0

    if(is.na(GAM_CHI$egvs_nose[i]))
        GAM_CHI$egvs_nose[i] = 0

    if(is.na(GAM_CHI$egvs_mental[i]))
        GAM_CHI$egvs_mental[i] = 0

    if(is.na(GAM_CHI$egvs_hyp[i]))
        GAM_CHI$egvs_hyp[i] = 0

    if(is.na(GAM_CHI$egvs_hf[i]))
        GAM_CHI$egvs_hf[i] = 0

    if(is.na(GAM_CHI$egvs_ecz[i]))
        GAM_CHI$egvs_ecz[i] = 0

    if(is.na(GAM_CHI$egvs_derm[i]))
        GAM_CHI$egvs_derm[i] = 0

    if(is.na(GAM_CHI$egvs_copd[i]))
        GAM_CHI$egvs_copd[i] = 0

    if(is.na(GAM_CHI$egvs_chd[i]))
        GAM_CHI$egvs_chd[i] = 0

    if(is.na(GAM_CHI$egvs_cbv[i]))
        GAM_CHI$egvs_cbv[i] = 0

    if(is.na(GAM_CHI$egvs_xh[i]))
        GAM_CHI$egvs_xh[i] = 0

    if(is.na(GAM_CHI$egvs_xxg[i]))
        GAM_CHI$egvs_xxg[i] = 0

    if(is.na(GAM_CHI$egvs_bronchi[i]))
        GAM_CHI$egvs_bronchi[i] = 0

    if(is.na(GAM_CHI$egvs_asthma[i]))
        GAM_CHI$egvs_asthma[i] = 0

    if(is.na(GAM_CHI$egvs_arrh[i]))
        GAM_CHI$egvs_arrh[i] = 0

    if(is.na(GAM_CHI$egvs_allergicrinitis[i]))
        GAM_CHI$egvs_allergicrinitis[i] = 0

    if(is.na(GAM_CHI$egvs_sg[i]))
        GAM_CHI$egvs_sg[i] = 0

    if(is.na(GAM_CHI$egvs_qstroke[i]))
        GAM_CHI$egvs_qstroke[i] = 0

    if(is.na(GAM_CHI$egvs_cstroke[i]))
        GAM_CHI$egvs_cstroke[i] = 0

    if(is.na(GAM_CHI$egvs_piyan[i]))
        GAM_CHI$egvs_piyan[i] = 0
}


# divide the dataframe into 3 dataframes : [2]pollutionData  ;  [3]emerRateData ； [1]dateData

#  [1]dateData
# combine different slices
dateData <- cbind(GAM_CHI[1 : 7], GAM_CHI[23 : 25])

#  [2]pollutionData
pollutionData <- cbind(GAM_CHI[8: 22], GAM_CHI[26 : 35], GAM_CHI[62 : (62 + 25)])

#  [3]emerRateData
emerRateData <- GAM_CHI[36 : (36 + 25)]

# bind date , and construct poData and emerData ...
poData <- cbind(dateData, pollutionData)
emerData <- cbind(dateData, emerRateData)

# use poData and emerData to build dataframe with different lag days

# the first line from emerData is emerData[, (lagTime + 1)]
# emerData.new is emerData[,((lagTime + 1):1096], and the row number is  (1096 - lagTime)
# slice !

# the lengthOfData is 1096 - lagTime - movingTime
lengthOfData = 1096 - lagTime - movingTime

# emerData.new should be from  (movingTime + lagTime) to 1096
emerData.new <- emerData[((lagTime + movingTime) : 1096), ]

# poData.new <- poData[(1:(1096 - lagTime)), ]
poData.new <- NULL
for(i in (movingTime : (1096 - lagTime))){
    tmpRow <- poData[i,]
    tmpRow$atomean <- mean(poData$atomean[(i - movingTime + 1) : i])
    tmpRow$atomax <- mean(poData$atomax[(i - movingTime + 1) : i])
    tmpRow$atomin <- mean(poData$atomin[(i - movingTime + 1) : i])
    tmpRow$rhmean <- mean(poData$rhmean[(i - movingTime + 1) : i])
    tmpRow$rhmin <- mean(poData$rhmin[(i - movingTime + 1) : i])
    tmpRow$premean <- mean(poData$premean[(i - movingTime + 1) : i])
    tmpRow$winspeed <- mean(poData$winspeed[(i - movingTime + 1) : i])
    tmpRow$windsmax <- mean(poData$windsmax[(i - movingTime + 1) : i])
    tmpRow$winddirection <- mean(poData$winddirection[(i - movingTime + 1) : i])
    tmpRow$windextr <- mean(poData$windextr[(i - movingTime + 1) : i])
    tmpRow$windextrdirec <- mean(poData$windextrdirec[(i - movingTime + 1) : i])
    tmpRow$sun <- mean(poData$sun[(i - movingTime + 1) : i])
    tmpRow$pm2.5 <- mean(poData$pm2.5[(i - movingTime + 1) : i])
    tmpRow$so2 <- mean(poData$so2[(i - movingTime + 1) : i])
    tmpRow$co <- mean(poData$co[(i - movingTime + 1) : i])
    tmpRow$no2 <- mean(poData$no2[(i - movingTime + 1) : i])
    tmpRow$o3_dmean <- mean(poData$o3_dmean[(i - movingTime + 1) : i])
    tmpRow$o3_8h <- mean(poData$o3_8h[(i - movingTime + 1) : i])
    tmpRow$rank <- mean(poData$rank[(i - movingTime + 1) : i]) 
    tmpRow$AQI <- mean(poData$AQI[(i - movingTime + 1) : i]) 
    tmpRow$level <- mean(poData$level[(i - movingTime + 1) : i]) 
    tmpRow$PM <- mean(poData$PM[(i - movingTime + 1) : i]) 
    tmpRow$OC <- mean(poData$OC[(i - movingTime + 1) : i]) 
    tmpRow$EC <- mean(poData$EC[(i - movingTime + 1) : i]) 
    tmpRow$NO3. <- mean(poData$NO3.[(i - movingTime + 1) : i]) 
    tmpRow$SO42. <- mean(poData$SO42.[(i - movingTime + 1) : i]) 
    tmpRow$NH4. <- mean(poData$NH4.[(i - movingTime + 1) : i])
    tmpRow$Cl <- mean(poData$Cl[(i - movingTime + 1) : i]) 
    tmpRow$Al <- mean(poData$Al[(i - movingTime + 1) : i]) 
    tmpRow$Ca <- mean(poData$Ca[(i - movingTime + 1) : i]) 
    tmpRow$Cr <- mean(poData$Cr[(i - movingTime + 1) : i]) 
    tmpRow$Cu <- mean(poData$Cu[(i - movingTime + 1) : i]) 
    tmpRow$Fe <- mean(poData$Fe[(i - movingTime + 1) : i]) 
    tmpRow$K <- mean(poData$K[(i - movingTime + 1) : i]) 
    tmpRow$Mg <- mean(poData$Mg[(i - movingTime + 1) : i]) 
    tmpRow$Mn <- mean(poData$Mn[(i - movingTime + 1) : i])
    tmpRow$Na <- mean(poData$Na[(i - movingTime + 1) : i]) 
    tmpRow$Ni <- mean(poData$Ni[(i - movingTime + 1) : i]) 
    tmpRow$Pb <- mean(poData$Pb[(i - movingTime + 1) : i]) 
    tmpRow$Si <- mean(poData$Si[(i - movingTime + 1) : i]) 
    tmpRow$Sr <- mean(poData$Sr[(i - movingTime + 1) : i])
    tmpRow$Ti <- mean(poData$Ti[(i - movingTime + 1) : i]) 
    tmpRow$V <- mean(poData$V[(i - movingTime + 1) : i]) 
    tmpRow$Zn <- mean(poData$Zn[(i - movingTime + 1) : i]) 
    tmpRow$Br <- mean(poData$Br[(i - movingTime + 1) : i]) 
    tmpRow$crustal <- mean(poData$crustal[(i - movingTime + 1) : i])
    tmpRow$non.crustal <- mean(poData$non.crustal[(i - movingTime + 1) : i])

    poData.new <- rbind(poData.new, tmpRow)
}

# cbind poData and emerData
poData.new$timeSeries <- NULL
emerData.new$timeSeries <- NULL

# rename data for date in emerData.new and poData.new
names(emerData.new)[1 : 9] <- c("emerData.date","emerData.year","emerData.month","emerData.day","emerData.number","emerData.site","emerData.dayofweek","emerData.holiday1","emerData.daycount") 
names(poData.new)[1 : 9] <- c("poData.date","poData.year","poData.month","poData.day","poData.number","poData.site","poData.dayofweek","poData.holiday1","poData.daycount") 

# rbind emerData.new and poData.new as wholeData
wholeData <- cbind(poData.new , emerData.new)

rm(list = c("pollutionData", "dateData", "emerData", "emerData.new", "emerRateData", "GAM_CHI", "poData", "poData.new", "tmpRow"))

# add timeSeries - 1:(1096-lagTime)
timeSeries <- as.data.frame(c(1 : (1096 - lagTime - movingTime + 1)))
wholeData <- cbind(timeSeries, wholeData)
names(wholeData)[1] = "timeSeries"

# build gam model for lag0 
# pollution : PM2.5

# lagModel <- gam(egvs_res ~ PM + Na + s(timeSeries, k = 14) + as.factor(emerData.dayofweek) + s(tmean, k = 3) + s(rhmean, k = 3), family = poisson, data = wholeData) 

# test the function of GAM model ...
# lagModel <- gam(egvs_res ~ PM + Na + s(timeSeries, k = 14) + as.factor(emerData.dayofweek) + s(tmean, k = 3) + s(rhmean, k = 3), family = poisson, data = wholeData) 
# summary(lagModel)

# begin to test different freedom for timeSeries

chooseDf <- c(NA, NA)

for(i in (3 : 30)){
    tmpModel <- gam(egvs_res ~ PM + Na + s(timeSeries, k = i) + as.factor(emerData.dayofweek) + s(tmean, k = 3) + s(rhmean, k = 3), family = poisson, data = wholeData)
    
    tmpAcf <- pacf(tmpModel$residuals, lag.max = 30)
    
    tmpM <- sum(abs(tmpAcf$acf))
    
    chooseDf <- append(chooseDf, tmpM)
}

# draw a chooseDf plot
# should choose the smallest one
plot(chooseDf)

chooseDf <- cbind(as.data.frame(c(1 : 30)) , as.data.frame(chooseDf))
names(chooseDf) <- c("df", "acfSum")

dfNum = 3
tmpMin = chooseDf$acfSum[3]
for(i in c(3:30)){
    if(chooseDf$acfSum[i] < tmpMin)
        dfNum = chooseDf$df[i]
    if(chooseDf$acfSum[i] < tmpMin)
        tmpMin = chooseDf$acfSum[i]
}

# the final lag model !!!
lagModel <- gam(egvs_res ~ PM + Na + s(timeSeries, k = dfNum) + as.factor(emerData.dayofweek) + s(tmean, k = 3) + s(rhmean, k = 3), family = poisson, data = wholeData)

# print the results on the screen
summary(lagModel)

dfNum