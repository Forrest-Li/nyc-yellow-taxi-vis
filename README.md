# NYC Yellow Taxi Visualization
### *w/ RShinny app on RStudio*

Take a view at screenshot (*I know it looks ugly... I'll beautify the UI later...*): 
![ScreenShot](https://i.imgur.com/lRpA1f8.png)

#### Environment
Language: R (v3.6.1)

Platform: RStudio (v1.2.5)

#### Packages used
`library(shiny)`

`library(shinyjs)`

`library(rgdal)`

`library(broom)`

`library(gpclib)`
> There are some problems installling this package, solution [reference here](http://tlocoh.r-forge.r-project.org/manual_install.html). If there are still problems installing, please report issues.

`library(ggplot2)`

`library(dplyr)`

`library(ggmap)` 
> Require Google **Maps Static API** application: [reference](https://console.cloud.google.com/apis/library/static-maps-backend.googleapis.com?project=digital-arbor-241501)
> 
> Acitvation tutorial: [link here](https://www.littlemissdata.com/blog/maps)

`library(corrplot)`

`library(RColorBrewer)`

#### How to run
1. Run with pure RShinny on RStudio

    Clone the project to your dir. Open RStudio and set working dir to the project. Open the app.R file and click the `Run App` button top-right of the code editor panel or run `runApp('<repository_dir>')` in console. 

2. Run with GitHub connection with RShinny on RStudio

    In RStudio, run the following codes and wait a few seconds while downloading. 

``` R
# install.packages("RShiny")
library(RShiny)
runGitHub("nyc-yellow-taxi-vis", "Forrest-Li")
```

3. Run online in shinnyapps.io

    I already upload the app on the shinnyapps platform. If you want to save resource of your computer. You can take a look at the app here: [https://forrestliyx.shinyapps.io/nyctaxivis/](https://forrestliyx.shinyapps.io/nyctaxivis/) 

#### Adjustment of attributes
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
* Coding structure is inefficient *(it's actually a mess, a disaster)*, will be reconstructed.
* Only about 10% randomly data are selected, if efficiency will be improved, more data will be included. 

#### References
* [Analyze the NYC Taxi Data | An Explorer of Things](https://chih-ling-hsu.github.io/2018/05/14/NYC)
* [Plotting maps from shapefiles with attributes using ggplot](https://rpubs.com/huanfaChen/ggplotShapefile)
* [How To Easily Customize GGPlot Legend for Great Graphics - Datanovia](https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/#change-legend-title)
* [r - Turning off some legends in a ggplot - Stack Overflow](https://stackoverflow.com/questions/14604435/turning-off-some-legends-in-a-ggplot)
* [Basic barplot with ggplot2 – the R Graph Gallery](https://www.r-graph-gallery.com/218-basic-barplots-with-ggplot2.html)
* [ggplot2 barplots: Quick start guide - R software and data visualization - Easy Guides - Wiki - STHDA](http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization)
* [Open and Plot Shapefiles in R – the R Graph Gallery](https://www.r-graph-gallery.com/168-load-a-shape-file-into-r.html)
* [Opening shapefile in R? - Geographic Information Systems Stack Exchange](https://gis.stackexchange.com/questions/19064/opening-shapefile-in-r)
* [Map Plots Created With R And Ggmap](https://www.littlemissdata.com/blog/maps)