# NYC Yellow Taxi Visualization
### *w/ RShinny app on RStudio*

Take a view at screenshot (*I know it looks ugly... I'll beautify the UI later...*): 
![ScreenShot](https://i.imgur.com/lRpA1f8.png)

#### Environment: 
Language: R (v3.6.1)

Platform: RStudio (v1.2.5)

#### How to run:
1. Run with pure RShinny on RStudio
    Clone the project to your dir. Open RStudio and set working dir to the project. Open the app.R file and click the `Run App` button top-right of the code editor panel or run `runApp('<repository_dir>')` in console. 

2. Run with GitHub connection with RShinny on RStudio. 
    In RStudio, run the following codes and wait a few seconds while downloading. 

``` R
# install.packages("RShiny")
library(RShiny)
runGitHub("nyc-yellow-taxi-vis", "Forrest-Li")
```

3. Run online in shinnyapps.io.
    I already upload the app on the shinnyapps platform. If you want to save resource of your computer. You can take a look at the app here: [https://forrestliyx.shinyapps.io/nyctaxivis/](https://forrestliyx.shinyapps.io/nyctaxivis/) 

#### Adjustment of attributes:
![Attr1](https://i.imgur.com/do84rpe.png)
![Attr2](https://i.imgur.com/nEcUlB1.png)

The `Time slicing mode` will switch between mode 1 & 2, which means if you're in one mode, the other mode will be disabled. 

Time in a day are differently specified between weekends and workdays.

In Time slicing mode 2, 
before checking `View by hours` box:
![before](https://i.imgur.com/GNdvfI3.png)
after checking:
![after](https://i.imgur.com/VNYFhBb.png)

#### Current Deficiencies
* Ugly UI will be improved later.
* Coding structure is inefficient, will be reconstructed.
* Only about 10% randomly data are selected, if efficiency will be improved, more data will be included. 