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
#### SLVTN_WARESHOUSE_DEV (database) on qdwsqldev05 (server)

################################################################

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
    outliers <- c(outliers,as.numeric(strsplit(grubbs.result$alternative," ")[[1]][3]))
    test <- x[!x %in% outliers]
    grubbs.result <- grubbs.test(test)
    pv <- grubbs.result$p.value
  }
  return(data.frame(X=x,Outlier=(x %in% outliers)))
}

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

########################## LAB RESULTS TABLE #########################

# The following are the types of labs that are run:
# select distinct lab_type from t_result
# a1c
# hdl
# ldl
# microalbumin
# Needs Update
# pap
# Total Cholesterol
# Triglycerides

###################################################################
#### A1c Data Check 

# Clean data
a1c = sqlQuery(myconn, "select * from t_result where pat_id in (select enc_patient_id from t_encounter where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) ) and lab_type ='a1c' and orig_site_id = '6'")
a1c = sapply(a1c, as.character)
a1c[is.na(a1c)] = " "
a1c = as.data.frame(a1c)

length(grep(pattern = "%", x = a1c$result_value)) # How many have %? 168
a1c$result_value = sub("%", "", a1c$result_value) # Remove % sign

length(grep(pattern = ">", x = a1c$result_value)) # How many have >? 6
a1c$result_value = sub(">", "", a1c$result_value) # Remove > sign

length(grep(pattern = "<", x = a1c$result_value)) # How many have >? 0
a1c$result_value = sub("<", "", a1c$result_value)

a1c$result_value = as.numeric.factor(a1c$result_value) # Text converted to NA
a1c$result_value_numeric = as.numeric.factor(a1c$result_value_numeric)
a1c[is.na(a1c)] = " "
a1c = as.data.frame(a1c)

# Units recorded correctly:
length(grep(pattern = "%", x = a1c$result_units)) # 4
a1c$result_units = c("%")

# Average value:
mean(a1c$result_value_numeric, na.rm=TRUE) #
median(a1c$result_value_numeric, na.rm=TRUE) #
sd(a1c$result_value_numeric, na.rm=TRUE) #

#### Look at results
# Extreme values
max(a1c$result_value_numeric[!is.na(a1c$result_value_numeric)]) # 
min(a1c$result_value_numeric[!is.na(a1c$result_value_numeric)]) # 

# Outliers
a1c_flag = grubbs.flag(a1c$result_value_numeric[!is.na(a1c$result_value_numeric)])
dim(a1c_flag[a1c_flag$Outlier==TRUE,])[1] #
min(a1c_flag[a1c_flag$Outlier==TRUE,1]) # 
a1c_good = a1c_flag[a1c_flag$Outlier==FALSE,1]
a1c_good = as.data.frame(a1c_good)
ggplot(a1c_flag ,aes(x=X,fill=Outlier))+ geom_histogram(binwidth = 0.1)+ theme_bw() + xlab("A1c Values")

# Normal range: 3-14%
a1c_range = a1c[a1c$result_value_numeric < 14 & a1c$result_value_numeric > 3,]
dim(a1c_range) #

# Plots without outliers:
a1c_plot =  ggplot(a1c_range, aes(x = result_value_numeric))
a1c_plot + geom_histogram(aes(fill = ..count..)) + xlab("A1C Values")

# Missing Values:
dim(a1c[a1c$result_value_numeric == " ",])[1] #246 missing values


###################################################################
#### hdl Data Check

# Clean data
hdl = sqlQuery(myconn, "select * from t_result where pat_id in (select enc_patient_id from t_encounter where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) ) and lab_type ='hdl' and orig_site_id = '6'")
hdl = sapply(hdl, as.character)
hdl[is.na(hdl)] = " "
hdl = as.data.frame(hdl)

length(grep(pattern = ">", x = hdl$result_value)) # 
length(grep(pattern = "<", x = hdl$result_value)) # 

hdl$result_value = as.numeric(hdl$result_value) # Text converted to NA
hdl$result_value_numeric = as.numeric.factor(hdl$result_value_numeric)
hdl[is.na(hdl)] = " "
hdl = as.data.frame(hdl)

# Units recorded correctly:
length(grep(pattern = "mg/dL", x = hdl$result_units)) # 
hdl$result_units = c("mg/dL")

#### Look at results
# Max/Min values
max(hdl$result_value_numeric[!is.na(hdl$result_value_numeric)]) #
min(hdl$result_value_numeric[!is.na(hdl$result_value_numeric)]) #

# Plots:
hdl_plot =  ggplot(hdl, aes(x = result_value_numeric))
hdl_plot + geom_histogram(aes(fill = ..count..)) + xlab("HDL Values")

# Outliers
hdl_flag = grubbs.flag(hdl$result_value_numeric)
dim(hdl_flag[hdl_flag$Outlier==TRUE,])[1] #
min(hdl_flag[hdl_flag$Outlier==TRUE,1]) # 
hdl_good = hdl_flag[hdl_flag$Outlier==FALSE,1]
hdl_good = as.data.frame(hdl_good)

# Averages
mean(hdl$result_value_numeric, na.rm=TRUE) #
median(hdl$result_value_numeric, na.rm=TRUE) #
sd(hdl$result_value_numeric, na.rm=TRUE) #

# Plots without outliers:
hdl_plot =  ggplot(hdl_range, aes(x = result_value_numeric))
hdl_plot + geom_histogram(aes(fill = ..count..), binwidth = 2) + xlab("HDL Values")

# Normal range: 3-14%
hdl_range = hdl[hdl$result_value_numeric < 100 & hdl$result_value_numeric > 20,]
dim(hdl_range) #19050

dim(hdl[hdl$result_value_numeric == " ",])[1] # missing values

###################################################################
#### ldl Data Check

# Clean data
ldl = sqlQuery(myconn, "select * from t_result where pat_id in (select enc_patient_id from t_encounter where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) ) and lab_type ='ldl' and orig_site_id = '6'")
ldl = sapply(ldl, as.character)
ldl[is.na(ldl)] = " "
ldl = as.data.frame(ldl)

length(grep(pattern = ">", x = ldl$result_value)) # 
length(grep(pattern = "<", x = ldl$result_value)) # 

ldl$result_value = as.numeric(ldl$result_value) # Text converted to NA
ldl$result_value_numeric = as.numeric.factor(ldl$result_value_numeric)

# Units recorded correctly:
length(grep(pattern = "mg/dL", x = ldl$result_units)) # 
ldl$result_units = c("mg/dL")

#### Look at results
# Max/Min values
max(ldl$result_value_numeric[!is.na(ldl$result_value_numeric)]) #
min(ldl$result_value_numeric[!is.na(ldl$result_value_numeric)]) #
length(ldl$result_value_numeric[ldl$result_value_numeric <= 0]) #

# Outliers
ldl_flag = grubbs.flag(ldl$result_value_numeric)
dim(ldl_flag[ldl_flag$Outlier==TRUE,])[1] #
ldl_good = ldl_flag[ldl_flag$Outlier==FALSE,1]
ldl_good = as.data.frame(ldl_good)

# Averages
mean(ldl$result_value_numeric, na.rm=TRUE) #
median(ldl$result_value_numeric, na.rm=TRUE) #
sd(ldl$result_value_numeric, na.rm=TRUE #
 
# Normal range: 5-300
ldl_range = ldl[ldl$result_value_numeric < 300 & ldl$result_value_numeric > 5,]
dim(ldl_range) #

# Plots:
ldl_plot =  ggplot(ldl_range, aes(x = result_value_numeric))
ldl_plot + geom_histogram(aes(fill = ..count..)) + xlab("ldl Values")

dim(ldl[ldl$result_value_numeric == " ",])[1] # missing values

###################################################################
#### Microalbumin Data Check

# Clean data
micro = sqlQuery(myconn, "select * from t_result where pat_id in (select enc_patient_id from t_encounter where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) ) and lab_type ='microalbumin' and orig_site_id = '6'")
micro = sapply(micro, as.character)
micro[is.na(micro)] = " "
micro = as.data.frame(micro)

length(grep(pattern = "NEGATIVE", x = micro$result_value)) # 
length(grep(pattern = "Neg", x = micro$result_value)) # 
length(grep(pattern = "<", x = micro$result_value)) # 

micro$result_value = as.numeric(micro$result_value) # Text converted to NA
micro$result_value_numeric = as.numeric.factor(micro$result_value_numeric)

# Units recorded correctly:
dim(micro[micro$result_units == " ",]) # 

#### Look at results
# Max/Min values
max(micro$result_value_numeric[!is.na(micro$result_value_numeric)]) #
min(micro$result_value_numeric[!is.na(micro$result_value_numeric)]) #
which.max(micro$result_value_numeric) # 6585
micro[6585,]
micro = micro[-6585,]

# Plots:
micro_plot =  ggplot(micro, aes(x = result_value_numeric))
micro_plot + geom_histogram(aes(fill = ..count..), binwidth = 100) + xlab("micro Values")

# Outliers
micro_flag = grubbs.flag(micro$result_value_numeric)
dim(micro_flag[micro_flag$Outlier==TRUE,])[1] #
micro_good = micro_flag[micro_flag$Outlier==FALSE,1]
micro_good = as.data.frame(micro_good)

# Averages
micro_average = mean(micro_good$micro_good, na.rm=TRUE) #
micro_median = median(micro_good$micro_good, na.rm=TRUE) #
micro_dev = sd(micro_good$micro_good, na.rm=TRUE) # 

# Plots (no outliers):
micro_plot =  ggplot(micro_good, aes(x = micro_good))
micro_plot + geom_histogram(aes(fill = ..count..)) + xlab("micro Values")
micro_plot + geom_density(colour="darkgreen", fill="green") + xlab("micro Values")

# Normal range: 3-14%
micro_range = micro[micro$result_value_numeric < 400 & micro$result_value_numeric > 1,]
dim(micro_range) #
dim(micro)[1] # total

dim(micro[micro$result_value_numeric == " ",])[1] #490 missing values

###################################################################
#### Total Cholesterol Data Check

# Clean data
tc = sqlQuery(myconn, "select * from t_result where pat_id in (select enc_patient_id from t_encounter where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) ) and lab_type ='Total Cholesterol' and orig_site_id = '6'")
tc = sapply(tc, as.character)
tc[is.na(tc)] = " "
tc = as.data.frame(tc)

length(grep(pattern = "<", x = tc$result_value)) # 
length(grep(pattern = ">", x = tc$result_value)) 

tc$result_value = as.numeric(tc$result_value) # Text converted to NA
tc$result_value_numeric = as.numeric.factor(tc$result_value_numeric)

# Units recorded correctly:
dim(tc[tc$result_units == "mg/dL",]) # 

#### Look at results
# Max/Min values
max(tc$result_value_numeric[!is.na(tc$result_value_numeric)]) #
min(tc$result_value_numeric[!is.na(tc$result_value_numeric)]) #

# Plots:
tc_plot =  ggplot(tc_range, aes(x = result_value_numeric))
tc_plot + geom_histogram(aes(fill = ..count..)) + xlab("Total Cholestoral Values")

# Outliers
tc_flag = grubbs.flag(tc$result_value_numeric)
dim(tc_flag[tc_flag$Outlier==TRUE,])[1] #19 outliers
tc_good = tc_flag[tc_flag$Outlier==FALSE,1]
tc_good = as.data.frame(tc_good)

# Averages
mean(tc$result_value_numeric, na.rm=TRUE) #
median(tc$result_value_numeric, na.rm=TRUE) #
sd(tc$result_value_numeric, na.rm=TRUE) #

# Plots (no outliers):
tc_plot =  ggplot(tc_good, aes(x = tc_good))
tc_plot + geom_histogram(aes(fill = ..count..)) + xlab("tc Values")
tc_plot + geom_density(colour="darkgreen", fill="green") + xlab("tc Values")

# Normal range: 3-14%
tc_range = tc[tc$result_value_numeric < 400 & tc$result_value_numeric > 100,]
dim(tc_range) #
dim(tc)[1] # total

dim(tc[tc$result_value_numeric == " ",])[1] # missing values

###################################################################
#### Triglycerides Data Check

# Clean data
trig = sqlQuery(myconn, "select * from t_result where pat_id in (select enc_patient_id from t_encounter where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) ) and lab_type ='Triglycerides' and orig_site_id = '6'")
trig = sapply(trig, as.character)
trig[is.na(trig)] = " "
trig = as.data.frame(trig)

length(grep(pattern = ">", x = trig$result_value)) # 
length(grep(pattern = "<", x = trig$result_value)) # 

trig$result_value = as.numeric(trig$result_value) # Text converted to NA
trig$result_value_numeric = as.numeric.factor(trig$result_value_numeric)

# Units recorded correctly:
length(grep(pattern = "mg/dL", x = trig$result_units)) # 
trig$result_units = c("mg/dL")

#### Look at results
# Max/Min values
max(trig$result_value_numeric[!is.na(trig$result_value_numeric)]) #
min(trig$result_value_numeric[!is.na(trig$result_value_numeric)]) #

# Plots:
trig_plot =  ggplot(trig_range, aes(x = result_value_numeric))
trig_plot + geom_histogram(aes(fill = ..count..)) + xlab("Triglyceride Values")

# Outliers
trig_flag = grubbs.flag(trig$result_value_numeric)
dim(trig_flag[trig_flag$Outlier==TRUE,])[1] #
trig_good = trig_flag[trig_flag$Outlier==FALSE,1]
trig_good = as.data.frame(trig_good)

# Averages
mean(trig$result_value_numeric, na.rm=TRUE) #
median(trig$result_value_numeric, na.rm=TRUE) #
sd(trig$result_value_numeric, na.rm=TRUE) #

# Plots (no outliers):
trig_plot =  ggplot(trig_good, aes(x = trig_good))
trig_plot + geom_histogram(aes(fill = ..count..)) + xlab("trig Values")
trig_plot + geom_density(colour="darkgreen", fill="green") + xlab("trig Values")

# Normal range: 20-700
trig_range = trig[trig$result_value_numeric < 700 & trig$result_value_numeric > 20,]
dim(trig_range) #
dim(trig)[1] # total

dim(trig[trig$result_value_numeric == " ",])[1] # missing values

