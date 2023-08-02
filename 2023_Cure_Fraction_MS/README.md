Updated cure fraction data/code for manuscript in 2023.

This code takes a SEER data draw containing life tables for cancers in an excel spreadsheet as input.

It estimates cure fraction across all stages of a cancer type simultaneously embedding prognosis constraints per stage
so that cure fraction decreases per stage, and outcomes are strictly worse for those not cured by stage.

This uses MCMC inference in the stan programming language to fit parameters to a likelihood model for survival.  The survival is given the
cure fraction, a weibull survival for those not cured, and a small probability of recurrence even in those "cured" to soak up real world
effects over time such as long-term recurrence or potential shifts in survival over time.  This four-parameter model fits the data well
and summarizes in naturally interpretable ways the distribution of survival among cancer types and stages.

If the SEER data draw were populated (suppressed here for privacy concerns):
Rscript scripts/00_coordination_script.R would execute the analysis producing tables/figures for the paper.
