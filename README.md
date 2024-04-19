# Aquatics_challenge_synthesis

Github repo for the synthesis analysis of the aquatics challenge submissions as part of the EFI-RCN group. This repository includes code to recreate the figures in the paper entitled *"What can we learn from 50,00 forecasts?: an analysis of the aquatics theme of the NEON Forecasting Challenge"*. The scripts should be run as follows:

1.  `install_packages.R` - script that includes the required packages
2.  `00_get_scores.R` - grab the scores from the Zenodo archived database. Needs to be run before 01, 02a, or 02b.
3.  `01a_inventory.R` - get the summary statistics included in the manuscript from the scores database and the model description Google Sheet
4.  `02a_manuscript_plots.Rmd` - code to generate the plots in the main manuscript
5.  `02b_supplementary_plots.Rmd` - code to generate the plots in the supplementary information

------------------------------------------------------------------------

## Running this code

To reproduce the figures from the manuscript you can access and run the code yourself by either downloading as a zip or cloning the repository (see below)

2.  **Clone** the code repository to a local RStudio project.

-   Use the \<\> Code button to copy the HTTPS link.
-   In RStudio go to New Project --\> Version Control --\> Git.
-   Paste the HTTPS link in the Repository URL space, and choose a suitable location for your local repository --\> Create Project.

3.  **Download** the zip file of the repository code. You can save changes (without version control) locally.

-   Find the \<\> Code button --\> Download ZIP.
-   Unzip this to a location on your PC and open the `Aquatics_challenge_synthesis.Rproj` file in RStudio.


### Using a Docker container 

Alternatively, using a Docker container provides a stable environment to execute the code.  Rocker is a Docker container with R (potentially also with Rstudio and other R packages). The one with Rstudio makes it easy to use a Docker container in a familiar interface.

1.  Download and install Docker Desktop on your computer. The instructions are at <https://www.docker.com/>.

2.  Launch Docker by starting the Docker application.

3.  Find the command line for your computer (terminal in Mac or command prompt in Windows)

4.  In the command line, try to run `docker run --rm -ti -e PASSWORD=yourpassword -p 8787:8787 rocker/geospatial` that is documented here: <https://rocker-project.org/> The geospatial image already includes many common R packages. 

5.  Point your browser to `localhost:8787`. Log in with user = rstudio, passwoard = yourpassword.

6. Follow the instructions above for cloning the code repository. 

------------------------------------------------------------------------
Note: Some of the data files are large and require approximately 3-4 GB of RAM

Note: In addition, analysis completed for the oral presentation 'Power of multiple models in lake forecasting' given as an invited talk at AGU 2023 is included in the AGU_analysis sub-directory.
