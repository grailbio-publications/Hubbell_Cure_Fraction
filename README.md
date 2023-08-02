This repository contains subprojects related to the cure fraction paper:
1) Original Cure Fraction Code: SEER data is suppressed for privacy concerns
1a) Original SEER draw specifications are retained so SEER draw can be duplicated
2) Generation of synthetic input data: SEER-like data to demonstrate code
2a) Transforms original SEER draw data into synthetic data with similar properties
3) Synthetic Cure Fraction: execute full workflow using synthetic data 
3a) Similar to original cure fraction code, but uses synthetic data to demonstrate execution

See individual README.md for descriptions of each subproject.

Guide to data:
(will not run without original SEER draw)
SEER_Draw->2023_Cure_Fraction_MS->results in paper

Guide to synthetic data:
(Will not run without original SEER draw)
SEER_Draw->2023_Synthetic_Cure_Data
(will run without original SEER Draw - time consuming)
2023_Cure_Fraction_Syn->figures/tables from synthetic data

Guide to demo data:
(will run quickly)
2023_Synthetic_Cure_Data->DEMO:2023_Synthetic_Cure_Data
DEMO:2023_Cure_Fraction_Syn-> figures tables for demo gallbladder