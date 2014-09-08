###############################################################

#### DATA QUALITY CHECKS
####
#### To be used for looking into the quality of vitals tables
####
#### Written by: A. Werner-Allen
#### Arcadia Solutions 8/26/2014

################################################################

#### Background:
#### This script is intended to dive more into the vitals base table
#### allowing the user to look at the quality of the data.
#### What is of interest are anomolies and outliers; data that does
#### not seem to make sense. We do that here by looking at the 
#### distribution of values to find outliers, determining appropriate
#### "good" cutoffs, examining the spread of the values to see if any
#### trends are present, and looking at averages to ensure that the 
#### data falls in the intuitive range.

#### This current script looks at data from:
#### CHPW_WAREHOUSE_DEV (database) on qdwsqldev05 (server)
#### Note that the SQL Scripts here only examine one site, NewHP
#### This is the most recent data acquisition

################################################################

# What should a data quality check look at?
# Severity of inconsistency
# Incompleteness
# Outliers
# Missing / Unknown

#### Packages needed:

# Connect remotely to correct ODBC (create a new one if necessary)
# install.packages("RODBC")
library(RODBC)
myconn <- odbcConnect("CHPW_Warehouse_Dev")

# Use ggplot2 for creating nice-looking plots
# install.packages("ggplot2")
library(ggplot2)

# Use outlier package for running Grubbs test
library(outliers)

grubbs.flag = function(x) {
  outliers = NULL
  test = x
  grubbs.result = grubbs.test(test)
  pv <- grubbs.result$p.value
  while(pv < 0.05) {
    outliers = c(outliers,as.numeric(strsplit(grubbs.result$alternative," ")[[1]][3]))
    test = x[!x %in% outliers]
    grubbs.result = grubbs.test(test)
    pv = grubbs.result$p.value
  }
  return(data.frame(X=x,Outlier=(x %in% outliers)))
}


###################################################################
#### Height distribution 

height = sqlQuery(myconn, "select distinct vitals_height, vitals_height_unit, datediff(year, pat_date_of_birth, '01-01-2014') age from t_vitals vit inner join t_patient pat on vit.vitals_patient_id = pat.pat_id where vitals_patient_id in (select enc_patient_id from t_encounter where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) ) and vitals_orig_site_id = '6'")
dim(height)[1] # Total number, no NAs
height = na.omit(height)
dim(height)[1] # Total number, with NAs

# Here we convert cm to in:
height$conversion = 0
for(i in 1:dim(height)[1]) {
  
  unit = height$vitals_height_unit[i]
  
  if(unit == "cm") { 
    height$conversion[i] = height$vitals_height[i] / 2.5 
  }
  
  else {   
    height$conversion[i] = height$vitals_height[i] 
  }
}

# Check to see that conversion worked:
head(height[height$vitals_height_unit == "cm",])
dim(height[height$vitals_height_unit == "cm",]) 

# Some looks into the data:
max(height$conversion[!is.na(height$conversion)]) 
min(height$conversion[!is.na(height$conversion)])

dim(height[height$conversion > 100,])[1] # how many are over 100 inches? 
dim(height[height$conversion < 10,])[1] # how many are under 10 inches? 

good_heights = dim(height[height$conversion < 100 & height$conversion > 10,])[1]/dim(height)[1] # percent that are good

# Basic Statistics
mean(height$conversion, na.rm=TRUE) #46.58245
median(height$conversion, na.rm=TRUE) #44
sd(height$conversion, na.rm=TRUE)

# Look at distribution of good heights:
height_temp = height[which(height$conversion < 100 & height$conversion > 10),]
average_height = mean(height_temp$conversion)

m = ggplot(height_temp, aes(x=conversion))
m + geom_histogram(aes(fill = ..count..)) + xlab("Height (in)")
qqnorm(height_temp$conversion)
ggplot(height_temp, aes(sample = height_temp$conversion)) + stat_qq(color="firebrick2", alpha=1)  + geom_abline(intercept = mean(height_temp$conversion), slope = sd(height_temp$conversion))

# Grubbs Test for outliers
grubbs.test(height$conversion)
height_flag = grubbs.flag(height$conversion)
dim(height_flag[height_flag$Outlier==TRUE,])[1] #83 outliers identified
ggplot(grubbs.flag(height$conversion),aes(x=height$conversion,fill=Outlier))+ geom_histogram(binwidth=diff(range(height$conversion))/30)+ theme_bw() + xlab("Height in Inches")

# Look at child/adult distinction
height_temp$age_factor = c("Child")

for(i in 1:dim(height_temp)[1]) {
  
  if(height_temp$age[i] >= 16) {
    
    height_temp$age_factor[i] = "Adult"
    
  }
  
}

m = ggplot(height_temp, aes(x=conversion))
m + geom_histogram(aes(fill=factor(age_factor)), binwidth = 1)+ xlab("Height (in)")


####################################################################
#### Weight distribution 

weight = sqlQuery(myconn, "select distinct vitals_weight, vitals_weight_unit, datediff(year, pat_date_of_birth, '01-01-2014') age from t_vitals vit inner join t_patient pat on vit.vitals_patient_id = pat.pat_id where vitals_patient_id in (select enc_patient_id from t_encounter where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) ) and vitals_orig_site_id = '6'")
dim(weight)[1] # Total number of entries
weight = na.omit(weight)
dim(weight)[1] # Total without NAs

# Here we convert kg to lb:
for(i in 1:dim(weight)[1]) {
  
  unit = weight$vitals_weight_unit[i]
  
  if(unit == "kg") {
    weight$conversion[i] = weight$vitals_weight[i] * 2.2
  }
  
  else {
    weight$conversion[i] = weight$vitals_weight[i]
  }
  
}

# Check kg conversion:
head(weight[weight$vitals_weight_unit == "kg",])
dim(weight[weight$vitals_weight_unit == "kg",]) 

# Some looks into the data:
weight[weight$conversion == max(weight$conversion),] # what is max? 
weight[weight$conversion == min(weight$conversion),] # what is min? 

dim(weight[weight$conversion > 400,])[1] # how many are over 400 lbs? 
dim(weight[weight$conversion < 2,])[1] # how many are under 2 lbs? 
dim(weight)[1] # total number: 5435
good_weights = dim(weight[weight$conversion < 400 & weight$conversion > 2,])[1]/dim(weight)[1] # percent that are good

# Basic Statistics
mean(weight$conversion, na.rm=TRUE) #
median(weight$conversion, na.rm=TRUE) #
sd(weight$conversion, na.rm=TRUE)

# Look at distribution of good weights:
weight_temp = weight[which(weight$conversion < 400 & weight$conversion > 2),] 
average_weight = mean(weight_temp$conversion)

m = ggplot(weight_temp, aes(x=conversion))
m + geom_histogram(aes(fill = ..count..)) + xlab("Weight (lbs)")
ggplot(weight_temp, aes(sample = weight_temp$conversion)) + stat_qq(color="firebrick2", alpha=1)  + geom_abline(intercept = mean(weight_temp$conversion), slope = sd(weight_temp$conversion))

# Look at child/adult distinction
weight_temp$age_factor = c("Child")

for(i in 1:dim(weight_temp)[1]) {
  
  if(weight_temp$age[i] >= 18) {
    
    weight_temp$age_factor[i] = "Adult"
    
  }
  
}

m = ggplot(weight_temp, aes(x=conversion))
m + geom_histogram(aes(fill=factor(age_factor)), binwidth = 1)+ xlab("Weight (lbs)")


####################################################################
#### BMI 

BMI = sqlQuery(myconn, "select distinct vitals_BMI from t_vitals where vitals_orig_site_id = '6' order by vitals_BMI")
dim(BMI)[1] # Total number of entries
BMI = na.omit(BMI)
dim(BMI)[1] # Total entries without NA

# Some looks into the data:
BMI[BMI$vitals_BMI == max(BMI$vitals_BMI),] # what is max? 
BMI[BMI$vitals_BMI == min(BMI$vitals_BMI),] # what is max? 

length(BMI[BMI$vitals_BMI > 60,]) # how many are over 100? 
length(BMI[BMI$vitals_BMI < 10,]) # how many are under 10? 
good_BMI = length(BMI[BMI$vitals_BMI < 60 & BMI$vitals_BMI > 10,])/dim(BMI)[1] 

# Look at distribution of good BMIs:
BMI_temp = BMI[which(BMI$vitals_BMI < 60 & BMI$vitals_BMI >10),]
BMI_temp = as.data.frame(BMI_temp)
average_BMI = mean(BMI_temp$BMI_temp)

m = ggplot(BMI_temp, aes(x=BMI_temp))
m + geom_histogram(aes(fill = ..count..), binwidth = 0.3) + xlab("BMI")

# Grubbs Test for outliers
grubbs.test(BMI$vitals_BMI)
BMI_flag = grubbs.flag(BMI$vitals_BMI)
dim(BMI_flag[BMI_flag$Outlier==TRUE,])[1] # outliers identified
ggplot(grubbs.flag(BMI$vitals_BMI),aes(x=BMI$vitals_BMI,fill=Outlier))+ geom_histogram(binwidth=diff(range(BMI$vitals_BMI))/30)+ theme_bw() + xlab("BMI")


####################################################################
#### Systolic/Diastolic distribution

systolic = sqlQuery(myconn, "select distinct vitals_systolic from t_vitals where vitals_orig_site_id = '6' order by vitals_systolic")
systolic = na.omit(systolic)
diastolic = sqlQuery(myconn, "select distinct vitals_diastolic from t_vitals where vitals_orig_site_id = '6' order by vitals_diastolic")
diastolic = na.omit(diastolic)

# Some looks into the data:
systolic[systolic$vitals_systolic == max(systolic$vitals_systolic),] # what is max? 14068
length(systolic[systolic$vitals_systolic > 200,]) # how many are over 200? 
length(systolic[systolic$vitals_systolic < 60,]) # how many are under 60? 
dim(systolic)[1] # total number: 177

good_systolic = length(systolic[systolic$vitals_systolic < 140 & systolic$vitals_systolic > 60,])/dim(systolic)[1] 
average_systolic = mean(systolic$vitals_systolic[systolic$vitals_systolic < 200 & systolic$vitals_systolic > 60])

diastolic[diastolic$vitals_diastolic == max(diastolic$vitals_diastolic),] # what is max? 
length(diastolic[diastolic$vitals_diastolic > 150,]) # how many are over 150? 
length(diastolic[diastolic$vitals_diastolic < 50,]) # how many are under 60? 
dim(diastolic)[1] # total number: 118

good_diastolic = length(diastolic[diastolic$vitals_diastolic < 90 & diastolic$vitals_diastolic > 50,])/dim(diastolic)[1] 
average_diastolic = mean(diastolic$vitals_diastolic[diastolic$vitals_diastolic < 150 & diastolic$vitals_diastolic > 50])

####################################################################
#### Temperature distribution

temperature = sqlQuery(myconn, "select distinct vitals_temperature, vitals_temperature_units from t_vitals where vitals_orig_site_id = '6' order by vitals_temperature")
dim(temperature)[1] # Total number of entries
temperature = na.omit(temperature)
dim(temperature)[1] # Total number of entries without NAs

# Convert from Farenheit to Celcius:
for(i in 1:dim(temperature)[1]) {
  
  unit = temperature$vitals_temperature_unit[i]
  
  if(unit == "C") {
    temperature$conversion[i] = (temperature$vitals_temperature[i] * 1.8) + 32
    # F = 9/5*C +32
  }
  
  else {
    temperature$conversion[i] = temperature$vitals_temperature[i]
  }
}

# Check C/F conversion:
head(temperature[temperature$vitals_temperature_unit == "C",])
dim(temperature[temperature$vitals_temperature_unit == "C",]) 

# Some looks into the data:
temperature[temperature$vitals_temperature == max(temperature$vitals_temperature),] # what is max?
temperature[temperature$vitals_temperature == min(temperature$vitals_temperature),] # what is max?

dim(temperature[temperature$conversion > 150,])[1] # how many are over 150F? 
dim(temperature[temperature$conversion < 80,])[1] # how many are under 80F? 
dim(temperature)[1] # total number: 260
good_temps = dim(temperature[temperature$conversion < 150 & temperature$conversion > 80,])[1]/dim(temperature)[1] # percent that are good

# Look at distribution of good temps:
temperature_temp = temperature[which(temperature$conversion < 150 & temperature$conversion > 80),]
average_temp = mean(temperature_temp$conversion)

m = ggplot(temperature_temp, aes(x=conversion))
m + geom_histogram(aes(fill = ..count..), binwidth = 1) + xlab("Temp (F)")


########################## VTIALS TABLE OUTPUT #########################

# good_heights:       
# good_weights:       
# good_BMI:           
# good_systolic:      
# good_diastolic:
# good_temps:         

# average_height:     
# average_weight:     
# average_BMI:        
# average_systolic:   
# average_diastolic:  
# average_temp:       








