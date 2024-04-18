# Aquatics_challenge_synthesis

Github repo for the synthesis analysis of the aquatics challenge submissions as part of the EFI-RCN group. This repository includes code to recreate the figures in the paper entitled *"What can we learn from 50,00 forecasts?: an analysis of the aquatics theme of the NEON Forecasting Challenge"*. The scripts should be run as follows:

1.  `install_packages.R` - script that includes the required packages
2.  `00_get_scores.R` - grab the scores from the Zenodo archived database. Needs to be run before 01, 02a, or 02b.
3.  `01a_inventory.R` - get the summary statistics included in the manuscript from the scores database and the model description Google Sheet
4.  `02a_manuscript_plots.Rmd` - code to generate the plots in the main manuscript
5.  `02b_supplementary_plots.Rmd` - code to generate the plots in the supplementary information

------------------------------------------------------------------------

Note: In addition, analysis completed for the oral presentation 'Power of multiple models in lake forecasting' given as an invited talk at AGU 2023 is included in the AGU_analysis sub-directory.
