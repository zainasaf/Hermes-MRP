A two-stage multilevel regression with post-stratification (MRP) model built in R using Stan for Bayesian estimation.
Stage 1 estimates the probability that a respondent has ever voted Conservative.
Stage 2, conditional on Stage 1, estimates the probability that former Conservative voters currently support Labour, the Conservatives, or Reform.
The model generates constituency-level predictions, allowing geographically granular insight into vote-switching patterns among former Conservative voters across Great Britain.
Files

hermes_model_script_v4.R — main model specification and estimation
normalization.Rmd — data preparation and normalisation
preds_hermes.R — generating and exporting constituency-level predictions
