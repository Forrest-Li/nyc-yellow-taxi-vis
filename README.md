# nyc-yellow-taxi-vis
NYC Yellow Taxi analysis &amp; visualization by R with RShiny app

How to run:
1. Run with pure RShinny on RStudio
    Clone the project to your dir. Open RStudio and set working dir to the project. Open the app.R file and click the `Run App` button top-right of the code editor panel or run `runApp('<repository_dir>')` in console. 

2. Run with GitHub connection with RShinny on RStudio. 
    In RStudio, run the following codes and wait a few seconds while downloading. 
``` R
# install.packages("RShiny")
library(RShiny)
runGitHub( "nyc-yellow-taxi-vis", "Forrest-Li")
```

3. Run online in shinnyapps.io.
    I already upload the app on the shinnyapps platform. If you want to save resource of your computer. You can take a look at the app here: [](https://forrestliyx.shinyapps.io/nyctaxivis/) 