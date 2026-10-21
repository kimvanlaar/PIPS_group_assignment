# Sleep lab installation, usage and statistical analysis
## Project overview
This project contains an R Shiny application about sleep, daylight and the biological clock. Besides that, it includes a separate R script for statistical analyses of social jet lag en tiredness and mood. Data for these analyses have been taken from the NHANES survey !!!ADD CITATION!!!

The main project files are as following:
- app.R: the Shiny app.
- statistical_analysis.R: the statistical analyses and graphs of social jet lag, tiredness and mood.
- R/helpers.R: contains functions used in app.R and statistical_analysis.R !!!OR ONLY IN APP.R?!!!.
- data.nhanes_ref.rds: the NHANES dataset used for the app and analysis.
- data-raw/build_reference.R: used to rebuild the nhanes dataset.
- README.md: this installation and usage guide
- README_general.md: theoretical background on social sleep debt.

## Requirements for running the app
You should have R and RStudio installed on your computer. You should also have an internet connection for installing packages. 

## Running the app
1. Open sleep_app.Rproj

   Working via the project makes sure that the project directory is used as the working directory.

2. Open app.R
3. Click **Run App**

## Using the app
The app contains two main parts, the human lab part and the rat lab part. 

### The human lab
On the first page of the app, **Your week**, enter your sleep and wake-up times for the last seven days. Indicates on which days you had to work or go to school and which days are days off. Also enter your daily mood and fatigue rating as well as your location, which compares your sleep schedule to the sunrise and sunset at that place. The app uses the geographical coordinates to calculate local daylight times. 

When you've entered everything mentioned above, you can view your results. Click on **See my results** on the bottom of the page or select Results on top. You can now view your results, your average sleep duration, body clock type, social jet lag, mood and tiredness scores and when you sleep compared to daylight hours. The results can also compare your results to the average US population (the nhanes dataset). You can even filter the reference group by age or sex. 

If you want more information about the data, you can click **About the data** on the top of the screen. There you can read more about the data. 

### The rat lab
The rat lab is an actogram simulator, which lets you simulate the activity of rats based on different light and darkness schedules. You can access the rat lab by clicking on **Rat clock lab** on the top of the screen.

In order to do so, select a predefined lightning schedule or create one yourself. 

