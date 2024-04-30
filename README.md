# Aquatics_challenge_synthesis

Archived scripts fpr synthesis analysis of the aquatics challenge submissions as part of the EFI-RCN group. This archive includes code to recreate the figures in the paper entitled:
 *"What can we learn from 100,000 freshwater forecasts? A synthesis from the NEON Ecological Forecasting Challenge"*. The scripts should be run as follows:

1.  `install_packages.R` - script that includes the required packages and installation instructions
2.  `manuscript_plots.Rmd` - code to generate the plots in the main manuscript
3.  `supplementary_plots.Rmd` - code to generate the plots in the supplementary information

------------------------------------------------------------------------

## Running this code

To reproduce the figures from the manuscript you can access and run the code yourself by downloading the scripts.zip folder

**Download** the zip file from Zenodo (Olsson, Thomas, & Carey, 2024). 

-   Unzip this to a location on your PC and open the `manuscript_plots.Rmd` or the `supplementary_plots.Rmd` file in RStudio.
- 	You can either run each chunk seperately or knit the whole document.
-	The first section of the `manuscript_plots.Rmd` will download and extract the archived scores and targets from the Zenodo data publication (Olsson, et al., 2024). 
-	Both scripts will save plots to a new Plots directory.

Note: If you do not have the required packages run the install_packages.R script first

------------------------------------------------------------------------

Olsson, F., Thomas, R. Q., & Carey, C. C. (2024). What can we learn from 100,000 freshwater forecasts? A synthesis from the NEON Ecological Forecasting Challenge: scripts. Zenodo. https://doi.org/10.5281/zenodo.11093206 

Olsson, F., Carey, C. C., Carl, B., Gregory, H., Ladwig, R., Lapeyrolerie, M., Lewis, A. S. L., Lofton, M., Montetealegre-Mora, F., Rabaey, J. S., Robbins, C. J., Yang, X., & Thomas, R. Q. (2024). 
What can we learn from 100,000 freshwater forecasts? A synthesis from the NEON Ecological Forecasting Challenge: scores and targets [Data set]. Zenodo. https://doi.org/10.5281/zenodo.11087208

------------------------------------------------------------------------

## Running this code from this repository

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
