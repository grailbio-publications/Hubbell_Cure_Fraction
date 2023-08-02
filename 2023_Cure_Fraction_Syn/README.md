Cure fraction code base: demonstrated using synthetic data.
This will execute a full fit for all cancer types and stages similar to the manuscript but using the full synthetic life tables
scripts/00_coordination_script.R.  The code is largely similar to the original cure fraction, but the files used for inputs are from the synthetic data rather than from the SEER Draw.

As the above is time consuming to iterate across all cancers, a demo set of code using the gallbladder data is included that runs quickly.
scripts/DEMO_00_coordination_script.R runs using the demo data for gallbladder only and makes the main table and figures one and three using only the demo gallbladder data.  Note: the demo gallbladder data is from the demo of synthetic data generation and is therefore not identical to the synthetic data used in the main demonstration run.  




