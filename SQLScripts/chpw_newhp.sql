
--CHPW_WAREHOUSE_DEV

-- How many patients have had an encounter in the past 18 months?
declare @site_id int
set @site_id = '6'
declare @period date
set @period = '01-01-2014'
select count(*) from t_encounter 
where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , @period)) 
and enc_orig_site_id = @site_id
--800548 patients with recent encounter

-- How many patients have had a medical encounter in the past 18 months?
declare @period date
set @period = '01-01-2014'
select count(*) from t_encounter 
where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , @period)) 
and enc_medical_ind = 'Y' 
--135319 with recent medical encounter

-- *** FROM NOW ON: Patients means a patient who has had an encounter in the past 18 months

select distinct lab_type from t_result where lab_type like '%albumin%'
--A1c
--HDL
--LDL
--Microalbumin
--Needs Update
--pap
--Total Cholesterol
--Triglycerides

--A1c: How many have had a1c test?
select distinct lab_type
from t_result
where pat_id in (
select enc_patient_id from t_encounter 
where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) 
)
and orig_site_id = '6'
--110731

select count(*)
from t_result 
where pat_id in (
select enc_patient_id from t_encounter 
where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) 
)
and lab_type ='a1c'
and result_value_numeric is not NULL
and orig_site_id = '6'
group by lab_type
--7.10345088294387 (average)
--6.46371882110449 (std deviation)

select count(*) 
from t_result 
where pat_id in (
select enc_patient_id from t_encounter 
where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) 
)
and lab_type ='a1c'
and result_value_numeric is NULL
--217

-------------------------------------------------
--HDL
select count(*) 
from t_result 
where pat_id in (
select enc_patient_id from t_encounter 
where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) 
)
and lab_type ='hdl'
and orig_site_id = '192'
--19131

select avg(result_value_numeric)
from t_result 
where pat_id in (
select enc_patient_id from t_encounter 
where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) 
)
and lab_type ='microalbumin'
and result_value_numeric is not NULL
and orig_site_id = '1'
group by lab_type
--46.9549057719936(average)
--14.4431970681877 (std deviation)

select count(*) 
from t_result 
where pat_id in (
select enc_patient_id from t_encounter 
where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) 
)
and lab_type ='hdl'
and result_value_numeric is NULL
--420

-------------------------------------------------
--LDL
select count(*) 
from t_result 
where pat_id in (
select enc_patient_id from t_encounter 
where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) 
)
and lab_type ='ldl'
and orig_site_id = '1'
--19131

select avg(result_value_numeric)
from t_result 
where pat_id in (
select enc_patient_id from t_encounter 
where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) 
)
and lab_type ='ldl'
and result_value_numeric is not NULL
group by lab_type
--110.291219893469 (average)
--14.4431970681877 (std deviation)

select count(*) 
from t_result 
where pat_id in (
select enc_patient_id from t_encounter 
where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) 
)
and lab_type ='hdl'
and result_value_numeric is NULL
--420


-------------------------------------------------
--Microalbumin
select count(*)
from t_result 
where pat_id in (
select enc_patient_id from t_encounter 
where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) 
)
and lab_type ='Microalbumin'
and orig_site_id = '6'

select distinct result_units 
from t_result 
where pat_id in (
select enc_patient_id from t_encounter 
where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) 
)
and lab_type ='Microalbumin'
and orig_site_id = '6'
--mcg/min
--mg/24 hr
--mg/day
--mg/dL
--mg/L
--ug/mL

select avg(result_value_numeric)
from t_result 
where pat_id in (
select enc_patient_id from t_encounter 
where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) 
)
and lab_type ='Microalbumin'
and result_value_numeric is not NULL
--and result_units = 'ug/min'
and (result_units = 'mg/24h' or result_units = 'mg/24hr') 
and orig_site_id = '6'

select count(*) 
from t_result 
where pat_id in (
select enc_patient_id from t_encounter 
where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) 
)
and lab_type ='Microalbumin'
and (result_value is NOT NULL)
--5742

select count(*) 
from t_result 
where pat_id in (
select enc_patient_id from t_encounter 
where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) 
)
and lab_type ='Microalbumin'
and (result_value = 'NEGATIVE' and result_units is NULL)
--3422

select *
from t_result 
where pat_id in (
select enc_patient_id from t_encounter 
where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) 
)
and lab_type ='Microalbumin'
and result_value_numeric > '300'

-------------------------------------------------
--Triglycerides
select count(*)
from t_result 
where pat_id in (
select enc_patient_id from t_encounter 
where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) 
)
and lab_type ='Triglycerides'
and orig_site_id = '8'



select stdev(result_value_numeric)
from t_result 
where pat_id in (
select enc_patient_id from t_encounter 
where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) 
)
and lab_type ='Triglycerides'
and result_value_numeric is not NULL
and orig_site_id = '8'

--201.500072202166 (average)
--7014.45502894305 (std deviation)

select min(result_value_numeric)
from t_result 
where pat_id in (
select enc_patient_id from t_encounter 
where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) 
)
and lab_type ='Triglycerides'
and result_value_numeric is not NULL

--201.500072202166 (average)
--7014.45502894305 (std deviation)

select count(*) 
from t_result 
where pat_id in (
select enc_patient_id from t_encounter 
where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) 
)
and lab_type ='Total Cholesterol'
and (result_value is NOT NULL)
--5742

select *
from t_result 
where pat_id in (
select enc_patient_id from t_encounter 
where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) 
)
and lab_type ='Total Cholesterol'
and result_value_numeric > '300'

select distinct vitals_weight, vitals_weight_unit, datediff(year, pat_date_of_birth, '01-01-2014') age from t_vitals vit inner join t_patient pat on vit.vitals_patient_id = pat.pat_id where vitals_patient_id in (select enc_patient_id from t_encounter where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) ) and vitals_orig_site_id = '1'


select count(*) 
from t_vitals vit with (nolock) inner join t_patient pat with (nolock)
on vit.vitals_patient_id = pat.pat_id 
where vitals_patient_id 
in (select enc_patient_id from t_encounter with (nolock) where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) ) 
and vitals_orig_site_id = '1'















