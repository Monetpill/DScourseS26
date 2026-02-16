--a. Read the Florida insurance data CSV file

.mode csv
.import FL_insurance_sample.csv

--b. Print out the first 10 rows of the data set

SELECT * FROM insurance LIMIT 10;

--c. List which counties are in the sample (i.e. list unique values of the county variable) 

SELECT DISTINCT county 
FROM insurance;

--d. Compute the average property appreciation from 2011 to 2012

SELECT AVG(tiv_2012 - tiv_2011) as avg_appreciation
FROM insurance;

--e. Create a frequency table of the construction variable to see what fraction of buildings are made out of wood or some other material

SELECT 
    CASE 
        WHEN construction = 'Wood' THEN 'Wood'
        ELSE 'Other'
    END as material,
    COUNT(*) as count
FROM insurance 
GROUP BY material;
