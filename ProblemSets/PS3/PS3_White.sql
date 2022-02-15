CREATE TABLE Florida(         /* This starts data table creation 
"policyID" INTEGER,           /* Variables
"statecode" CHAR,
"county" CHAR,
"eq_site_limit" INTEGER,
"hu_site_limit" INTEGER,
"fl_site_limit" INTEGER,
"fr_site_limit" INTEGER, 
"tiv_2011" INTEGER,
"tiv_2012" DOUBLE,
"eq_site_deductible" DOUBLE,
"hu_site_deductible" DOUBLE,
"fl_site_deductible" DOUBLE,
"fr_site_deductible" DOUBLE,
"point_latitude" DOUBLE,
"point_longitude" DOUBLE,
"line" CHAR,
"construction" CHAR,
"point_granularity" INTEGER);
.mode csv                                  /* This part switches to csv
.import FL_insurance_sample.csv            /* This pulls it in from the csv
SELECT * FROM Florida LIMIT 10;            /* This selects the first 10 observations of Florida
SELECT DISTINCT county FROM Florida;       /* Distinct returns the unique values from the variable of choice
SELECT AVG(tiv_2012-tiv_2011) FROM Florida;/* Select a function from the dataset, in this case, average of tiv_2012-tiv_2011
SELECT construction, COUNT(*) FROM Florida GROUP BY construction; /* This creates a one way frequency table, that counts unique values and groups by them.

