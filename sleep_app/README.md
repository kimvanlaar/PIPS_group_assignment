# Sleep lab installation, usage and statistical analysis
This README covers the essential information required to install, run, and interpret our project. 

## Project overview
This project contains an R Shiny application about sleep, daylight and the biological clock. Besides that, it includes a separate R script for statistical analyses of social jet lag en tiredness and mood. Data for these analyses have been taken from the NHANES survey (Bryan et al., 2021). 

## Repository structure

```text
sleep_app/
├── data/
│   └── nhanes_ref.rds       # the NHANES dataset used for the app and analysis
├── data-raw/
│   └── build_reference.R    # Script used to rebuild the NHANES dataset
├── R/
│   └── helpers.R            # Contains functions used in app.R and statistical_analysis.R !!!OR ONLY IN APP.R?!!!
├── app.R                    # Shiny application
├── statistical_analysis.R   # Statistical analysis and graphs of social jet lag, tiredness and mood
├── sleep_app.Rproj          # RStudio project file
├── README.md                # This installation and usage guide
```

## Requirements for running the app
You should have R and RStudio installed on your computer. You should also have an internet connection for installing packages. 

Clone the repository using R
CHECK THIS!!!! I DON'T THINK IT WORKS :(

```r
system("git clone https://github.com/kimvanlaar/PIPS_group_assignment.git")
```

## Running the app
- Open sleep_app.Rproj

   Working via the project makes sure that the project directory is used as the working directory.

- Open app.R
- Click **Run App**

## Using the app
The app contains two main parts, the human lab part and the rat lab part. 

### The Human Lab
On the first page of the app, **Your week**, enter your sleep and wake-up times for the last seven days. Indicates on which days you had to work or go to school and which days are days off. Also enter your daily mood and fatigue rating as well as your location, which compares your sleep schedule to the sunrise and sunset at that place. The app uses the geographical coordinates to calculate local daylight times. (There is an option to also manually enter the sunrise and sunset times for customisation).

When you've entered everything mentioned above, you can view your results. Click on **See my results** on the bottom of the page or select Results on top. You can now view your results, your average sleep duration, body clock type, social jet lag, mood and tiredness scores and when you sleep compared to daylight hours. The results can also compare your results to the average US population (the NHANES dataset). You can even filter the reference group by age or sex. 

If you want more information about the data, you can click **About the data** on the top of the screen. There you can read more about the data. 

**NHANES reference data**

The app uses the NHANES data when it can find it. If the file is missing or not in the correct folder, the app uses a simulated reference dataset instead. The message that pops up when you open the app will tell you which data is being used.

### The Rat Lab
The Rat Lab is an actogram simulator, which lets you simulate the activity of rats based on different light and darkness schedules. You can access the rat lab by clicking on **Rat clock lab** on the top of the screen. We wanted to simulate this phenomenon to show that not only humans but all living creatures have an internal clock that is linked with the circadian rhythms and will go haywire when the Light and Dark schedule is disturbed. 

In order to do so, select a predefined lightning schedule or create one yourself. The actogram will now automatically start running. You can play around with the lightning and rat settings to explore free-running rhythms, entrainment and phase shifts.  

## Running the statistical analysis
### Research questions
The statistical_analysis.R script investigates two research questions:
1. Is greater social jet lag associated with more tiredness?
2. Is greater social jet lag associated with lower mood?
The regression models used to answer these research questions also adjust for age, sex and average weekly sleep duration.

### Data
We use the NHANES 2017 to March 2020 data that is already included in the repository (`sleep app/data/nhanes_ref.rds`; Bryan et al., 2021). It contains sleep and wake times on workdays and free days, which we use to compute social jet lag (Wittmann et al., 2006), and two items from the PHQ-9 questionnaire (Kroenke et al., 2001): "Feeling tired or having little energy" and "Feeling down, depressed or hopeless", both scored from 0 (not at all) to 3 (nearly every day). sd_week is the average weekly sleep duration.

### Data preparation
- Only adults (18+) are kept, because the PHQ-9 items were only asked from age 18.
- Participants with more than 6 hours of social jet lag are excluded, since such large values most likely reflect shift work rather than normal work/free day differences.
- Participants with missing values on any analysis variable are excluded.

This leaves about 8000 participants per analysis.

### Analysis
For each outcome, a multiple linear regression predicts the score from social jet lag, controlling for age, sex and average sleep duration. Residual plots are included to check the model assumptions. Since the outcomes only take the values 0 to 3, the linear model is a simplification, which we discuss in the main file.

### Output
- A results table per model in the console (estimate, 95% confidence interval, p-value).
- Residual plots for both models.
- Plots of both outcomes side by side: a static version (Plots tab) and an interactive plotly version (Viewer tab), where you can hover over the points to see the exact numbers, when social jet lag is considered categorical.
- To examine further nuance, we plotted the same graphs with social jet lag as continuous. Here we first plotted a straight line and on being not super informative, plotted a curve.
- Bar plots of the percentage of responses for tiredness/low mood for different social jet lag groups.

### How to run the statistical analysis:
- Open sleep_app.Rproj
- Open statistical_analysis.R
- Run the script using **Source**
- Look at the output

### Interpretation 
A regression coefficient for sjl (social jet lag) represents the estimated change in the outcome score for each additional hour of social jet lag, keeping the other covariates constant. A positive coefficient indicates a higher outcome score (tiredness or low mood) associated with greater social jet lag. 

### Some considerations
- The data are observational and largely cross-sectional and associations do not establish causality.
- The outcomes are individual PHQ items scored from 0 to 3. They are not a full diagnostic assessment or a complete PHQ-9 score.
- The data contains US citizens only, generalizability to the full world population might therefore not be possible.

## Troubleshooting
### helpers.R cannot be found
- Open the sleep_app.Rproj file and run the application from the sleep_app directory.
- Check that R/helpers.R exists.

### nhanes_ref.rds is missing
•	Run source("data-raw/build_reference.R") from the project directory.
•	If you only want to explore the application, it may use simulated data. Do not treat simulated reference results as actual research findings.

### Packages cannot be installed
- Check your internet connection, CRAN mirror settings, and permissions to install R packages.
- If necessary, install a package manually using install.packages("package_name").

### The app cannot find files
- Check the current working directory with getwd().
- Open the project through sleep_app.Rproj rather than opening an individual R file separately.

### Geographical information or daylight information does not work
- Check the entered location and your internet connection.

## References
- Bryan, S., Afful, J., Carroll, M., Te-Ching, C., Orlando, D., Fink, S., & Fryar, C. (2021). NHSR 158. National Health and Nutrition Examination Survey 2017–March 2020 Pre-pandemic Data Files (Nummer 158). https://doi.org/10.15620/cdc:106273
- Kroenke, K., Spitzer, R. L., & Williams, J. B. W. (2001). The PHQ-9: Validity of a brief depression severity measure. *Journal of General Internal Medicine, 16*(9), 606–613.
- Wittmann, M., Dinich, J., Merrow, M., & Roenneberg, T. (2006). Social jetlag: Misalignment of biological and social time. *Chronobiology International, 23*(1–2), 497–509.
