
--------------------------------------------------------------------------
---------- DIABETES: SENSITITY ANALYSIS in BENCHMARK DB ------------------
--------------------------------------------------------------------------
/*------------------------------------------------------------------------
<SDDX
  NAME = ""
  DESC = "Sensitivity analysis of diabetic transitions"
  NOTE = "--"
  AUTH = "AKumbara, AWerner-Allen"
  LOG  = 2015-6-17 v0.03 corresponds to 	
  >
</SDDX>
*/

---------------------------------------------------------------------
--- Create necessary tables
IF OBJECT_ID('tempdb..#transitions') IS NOT NULL DROP TABLE #transitions
Create  table #transitions
(	state varchar(50), 
	Healthy int, 
	PreDiabetes int, 
	DiabetesHCC19 int, 
	DiabetesHCC18 int,
	DiabetesHCC17 int,)

IF OBJECT_ID('tempdb..#healthy1') IS NOT NULL DROP TABLE #healthy1
Create  table #healthy1
(patid varchar(50))

IF OBJECT_ID('tempdb..#healthy2') IS NOT NULL DROP TABLE #healthy2
Create  table #healthy2
(patid varchar(50), enctime int)

---------------------------------------------------------------------
--- Define states
DECLARE @healthy TABLE(IDs VARCHAR(100));
INSERT INTO @healthy
VALUES('10');
DECLARE @PreDiabetes TABLE(IDs VARCHAR(100));
INSERT INTO @PreDiabetes
--VALUES('11'),('40'),('45');
VALUES('11');
DECLARE @Diabetes19 TABLE(IDs VARCHAR(100));
INSERT INTO @Diabetes19
--VALUES('12'),('50'),('55');
VALUES('12');
DECLARE @Diabetes18 TABLE(IDs VARCHAR(100));
INSERT INTO @Diabetes18
--VALUES('13'),('60'),('65');
VALUES('13');
DECLARE @Diabetes17 TABLE(IDs VARCHAR(100));
INSERT INTO @Diabetes17
--VALUES('14'),('70'),('75');
VALUES('14');
DECLARE @PotentialDiab TABLE(IDs VARCHAR(100));
INSERT INTO @PotentialDiab
VALUES('30'),('35');

---------------------------------------------------------------------
--- Declare parameters
declare @start_year int = 2011
declare @context_id varchar(225) = '2073FFB5-8CB8-475A-948E-47AAE4B27E8C'

--88F343FA-B35D-4C6C-8A9A-DDC3AE08EED7	Ellis Hospital
--5D5EF67E-9067-4146-9AAD-3CD1436A4799	LifePoint Hospitals
--A21A18F9-4CFF-40E5-99F8-4298D5034E0B	CHPW
--5C9935C8-869F-4EFE-A3E7-09C0FE1DE614	LHCQF
--2073FFB5-8CB8-475A-948E-47AAE4B27E8C	BIDCO

----------------------------------------------------------------------
--- Define healthy population

--- People healthy in First Year
--- Note that we consider people to be healthy if they had an encounter
--- and no diabetes classification 

-- restrict this to 2 years (i.e. NOT 3 years)
;with healthy_encounters as (
select distinct enc_patient_id as pats
	from t_encounter
	where enc_timestamp > @start_year-2
	and enc_timestamp <= @start_year
	and enc_delete_ind = 'N'
	and context_id = @context_id
	and enc_medical_ind = 'Y')
,diabetics as (
select distinct patientid
from wrk.DiabetesTransitions
where score1 in (select * from @diabetes17 
				union select * from @Diabetes18
				union select * from @Diabetes19
				union select * from @Prediabetes
				union select * from @PotentialDiab) 
and year1 in (@start_year-1, @start_year))
insert into #healthy1 select distinct pats from healthy_encounters
where pats not in (select * from diabetics)

--- People healthy in Second Year
;with healthy_encounters as (
select distinct enc_patient_id as pats, enc_timestamp as enctime
	from t_encounter
	where enc_timestamp > @start_year-1
	and enc_timestamp <= @start_year+1
	and enc_delete_ind = 'N'
	and context_id = @context_id
	and enc_medical_ind = 'Y')
,diabetics as (
select distinct patientid
from wrk.DiabetesTransitions
where score2 in (select * from @diabetes17 
				union select * from @Diabetes18
				union select * from @Diabetes19
				union select * from @Prediabetes
				union select * from @PotentialDiab) 
and year1 in (@start_year, @start_year+1))
insert into #healthy2 select distinct pats, enctime from healthy_encounters
where pats not in (select * from diabetics)

----------------------------------------------------------------------
--------------------- TRANSITION MATRIX ------------------------------

insert into #transitions (state) values ('Healthy')

update #transitions 
set healthy = (select count(distinct patid) 
	from #healthy1 where patid in (select distinct patid from #healthy2))
where state = 'Healthy'

;with healthy_prediab as (
select distinct patientid as count
	from wrk.DiabetesTransitions 
	where score2 in (select * from @PreDiabetes) 
	and year1 = @start_year
	and contextid = @context_id)
update #transitions 
set PreDiabetes = (select count(distinct patid) 
	from #healthy1 where patid in (select * from healthy_prediab))
where state = 'Healthy'

;with healthy_diabetic19 as (
select distinct patientid as count
	from wrk.DiabetesTransitions 
	where score2 in (select * from @Diabetes19)  
	and year1 = @start_year
	and contextid = @context_id)
update #transitions 
set DiabetesHCC19 = (select count(distinct patid) from #healthy1 where patid in (select * from healthy_diabetic19))
where state = 'Healthy'

;with healthy_diabetic18 as (
select distinct patientid as count
	from wrk.DiabetesTransitions 
	where score2 in (select * from @Diabetes18)  
	and contextid = @context_id)
update #transitions 
set DiabetesHCC18 = (select count(distinct patid) from #healthy1 where patid in (select * from healthy_diabetic18))
where state = 'Healthy'

;with healthy_diabetic17 as (
select distinct patientid as count
	from wrk.DiabetesTransitions 
	where score2 in (select * from @Diabetes17)  
	and contextid = @context_id)
update #transitions 
set DiabetesHCC17 = (select count(distinct patid) from #healthy1 where patid in (select * from healthy_diabetic17))
where state = 'Healthy'

-----------------------------------------------------------------

insert into #transitions (state) values ('PreDiabetes')

;with prediabetes_healthy as (
select count(distinct patientid) as count 
	from wrk.DiabetesTransitions
	where score1 in (select * from @Prediabetes) 
	and score2 in (select * from @healthy)  
	and year1 = @start_year
	and contextid = @context_id)
update #transitions 
set Healthy = (select * from prediabetes_healthy)
where state = 'PreDiabetes'

;with prediabetes_prediabetes as (
select count(distinct patientid) as count 
	from wrk.DiabetesTransitions
	where score1 in (select * from @Prediabetes) 
	and score2 in (select * from @Prediabetes)  
	and year1 = @start_year
	and contextid = @context_id)
update #transitions 
set Prediabetes = (select * from prediabetes_prediabetes)
where state = 'PreDiabetes'

;with prediabetes_diabetes19 as (
select count(distinct patientid) as count 
	from wrk.DiabetesTransitions
	where score1 in (select * from @Prediabetes) 
	and score2 in (select * from @Diabetes19)  
	and year1 = @start_year
	and contextid = @context_id)
update #transitions 
set DiabetesHCC19 = (select * from prediabetes_diabetes19)
where state = 'PreDiabetes'

;with prediabetic_diabetes18 as (
select count(distinct patientid) as count 
	from wrk.DiabetesTransitions
	where score1 in (select * from @PreDiabetes) 
	and score2 in (select * from @Diabetes18)  
	and year1 = @start_year
	and contextid = @context_id)
update #transitions 
set DiabetesHCC18 = (select * from prediabetic_diabetes18)
where state = 'PreDiabetes'

;with prediabetic_diabetes17 as (
select count(distinct patientid) as count 
	from wrk.DiabetesTransitions
	where score1 in (select * from @PreDiabetes) 
	and score2 in (select * from @Diabetes17)  
	and year1 = @start_year
	and contextid = @context_id)
update #transitions 
set DiabetesHCC17 = (select * from prediabetic_diabetes17)
where state = 'PreDiabetes'

-----------------------------------------------------------------

insert into #transitions (state) values ('DiabetesHCC19')

update #transitions 
set healthy = 0
where state = 'DiabetesHCC19'

update #transitions 
set PreDiabetes = 0
where state = 'DiabetesHCC19'

;with diabetic19_diabetic19 as (
select distinct patientid as pats 
	from wrk.DiabetesTransitions
	where score1 in (select * from @Diabetes19)  
	and score2 not in (select * from @Diabetes18 union select * from @Diabetes17)
	and year1 = @start_year
	and contextid = @context_id)
,diabetic19 as (
select distinct patientid as pats 
	from wrk.DiabetesTransitions
	where score1 in (select * from @Diabetes19)  
	and year1 = @start_year
	and contextid = @context_id)
update #transitions 
set DiabetesHCC19 = (
select sum(cnt) as c from 
(select count(distinct patid) as cnt from #healthy2 where patid in (select * from diabetic19)
union 
select count(distinct pats) as cnt from diabetic19_diabetic19) x)
where state = 'DiabetesHCC19'

;with diabetic19_diabetic18 as (
select count(distinct patientid) as count 
	from wrk.DiabetesTransitions
	where score1 in (select * from @Diabetes19) 
	and score2 in (select * from @Diabetes18)
	and year1 = @start_year
	and contextid = @context_id)
update #transitions 
set DiabetesHCC18 = (select * from diabetic19_diabetic18)
where state = 'DiabetesHCC19'

;with diabetic19_diabetic17 as (
select count(distinct patientid) as count 
	from wrk.DiabetesTransitions
	where score1 in (select * from @Diabetes19) 
	and score2 in (select * from @Diabetes17)
	and year1 = @start_year
	and contextid = @context_id)
update #transitions 
set DiabetesHCC17 = (select * from diabetic19_diabetic17)
where state = 'DiabetesHCC19'

-----------------------------------------------------------------

insert into #transitions (state) values ('DiabetesHCC18')

update #transitions 
set healthy = 0
where state = 'DiabetesHCC18'

update #transitions 
set PreDiabetes = 0
where state = 'DiabetesHCC18'

;with diabetic18_diabetic19 as (
select count(distinct patientid) as count 
	from wrk.DiabetesTransitions
	where score1 in (select * from @Diabetes18) 
	and score2 in (select * from @Diabetes19)
	and year1 = @start_year
	and contextid = @context_id)
update #transitions 
set DiabetesHCC19 = (select * from diabetic18_diabetic19)
where state = 'DiabetesHCC18'

;with diabetic18_diabetic18 as (
select distinct patientid as pats 
	from wrk.DiabetesTransitions
	where score1 in (select * from @Diabetes18) 
	and score2 not in (select * from @Diabetes19 union select * from @Diabetes17)
	and year1 = @start_year
	and contextid = @context_id)
,diabetic18 as (
select distinct patientid as pats 
	from wrk.DiabetesTransitions
	where score1 in (select * from @Diabetes18) 
	and year1 = @start_year
	and contextid = @context_id)
update #transitions 
set DiabetesHCC18 = (
select sum(cnt) as c from 
(select count(distinct patid) as cnt from #healthy2 where patid in (select * from diabetic18) 
	--and enctime = @start_year + 1
union 
select count(distinct pats) as cnt from diabetic18_diabetic18) x)
where state = 'DiabetesHCC18'

;with diabetic18_diabetic17 as (
select count(distinct patientid) as count 
	from wrk.DiabetesTransitions
	where score1 in (select * from @Diabetes18) 
	and score2 in (select * from @Diabetes17)
	and year1 = @start_year
	and contextid = @context_id)
update #transitions 
set DiabetesHCC17 = (select * from diabetic18_diabetic17)
where state = 'DiabetesHCC18'

-----------------------------------------------------------------

insert into #transitions (state) values ('DiabetesHCC17')

update #transitions 
set healthy = 0
where state = 'DiabetesHCC17'

update #transitions 
set PreDiabetes = 0
where state = 'DiabetesHCC17'

;with diabetic17_diabetic19 as (
select count(distinct patientid) as count 
	from wrk.DiabetesTransitions
	where score1 in (select * from @Diabetes17) 
	and score2 in (select * from @Diabetes19)
	and year1 = @start_year
	and contextid = @context_id)
update #transitions 
set DiabetesHCC19 = (select * from diabetic17_diabetic19)
where state = 'DiabetesHCC17'

;with diabetic17_diabetic18 as (
select count(distinct patientid) as count 
	from wrk.DiabetesTransitions
	where score1 in (select * from @Diabetes17) 
	and score2 in (select * from @Diabetes18)
	and year1 = @start_year
	and contextid = @context_id)
update #transitions 
set DiabetesHCC18 = (select * from diabetic17_diabetic18)
where state = 'DiabetesHCC17'

;with diabetic17_diabetic17 as (
select distinct patientid as pats 
	from wrk.DiabetesTransitions
	where score1 in (select * from @Diabetes17) 
	and score2 not in (select * from @Diabetes18 union select * from @Diabetes19)
	and year1 = @start_year
	and contextid = @context_id)
, diabetic17 as (
select distinct patientid as pats 
	from wrk.DiabetesTransitions
	where score1 in (select * from @Diabetes17) 
	and year1 = @start_year
	and contextid = @context_id)
update #transitions 
set DiabetesHCC17 = (
select sum(cnt) as c from 
(select count(distinct patid) as cnt from #healthy2 where patid in (select * from diabetic17) 
	--and enctime = @start_year + 1
union 
select count(distinct pats) as cnt from diabetic17_diabetic17) x)
where state = 'DiabetesHCC17'

select * from #transitions
