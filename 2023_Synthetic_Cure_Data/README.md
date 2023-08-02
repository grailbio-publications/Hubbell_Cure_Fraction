Generate synthetic life tables to demonstrate cure fraction without sharing SEER data directly.

The idea here is to generate synthetic life table data to demonstrate cure fraction analysis.  This is to ensure privacy by using synthetic data to demonstrate code, where the synthetic data captures the real data well. One reference for this sort of procedure is https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-022-01654-1.

for this purpose
1) read in original stratified life tables
2) for each combination including stage
3) expand life tables to within-month timing to generate artificial individual-level events
4) fit flexsurvspline with enough knots to capture high fidelity data
5) generate synthetic individual death details from the spline using inverse uniform trick (already in survival code)
6) fit flexsurvspline to capture censorship rates by time
7) generate synthetic individual censorship times 
8) collapse synthetic individuals into life table
9) Output synthetic life tables to be used to fit data

Some tricks and complexities needed to be handled:
1) The data is structured in age-bands - some age bands have very little data.  The code recursively combines adjacent age bands until there is enough data for a reasonable fit, then divides synthetic individuals randomly to reflect original age band relative population levels.
2) Even with combined data, sometimes the flexsurvspline fit has too many knots leading to unstable convergence.  When this is detected, the number of knots is reduced to simplify the model.
3) The number of individuals within an age band may itself be privacy revealing for some low incidence cancers.  Expected number of individuals are boosted above 5 to blur any such information leakage concerning the number of individuals, and random sampling is done as to the actual number of synthetic individuals assigned to an age band.

As this data is only being used to demonstrate code, these routines are only capturing overall trends.

As original SEER data has been suppressed this code will not run as provided as the input data is missing.  If the input data is provided, you would run scripts/00_coordination_script.R to call all the routines in the proper order.

To briefly demonstrate this procedure, a demonstration routine has been added to generate a single cancer life table from synthetic data.
run scripts/DEMO_00_coordination_script.R to generate gallbadder synthetic demo data from the synthetic data originally generated.
