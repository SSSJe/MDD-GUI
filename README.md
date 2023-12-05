# MDD-GUI
For a web of the cancer database
## Method
There is a method to how to run the MDD-GUI website.
1. Click `Code` button on the top of this page, then click [Download ZIP](https://github.com/SSSJe/MDD-GUI/archive/refs/heads/main.zip);
2. Unzip the file to your working directory;
3. Double click `MDD-GUI.Rproj` to open the project;
4. Make sure the `renv` package is install automatically;
  > if not install `renv` package, please <br>
  > install.package('renv')
5. Next, use `renv::restore()` to install all other packages which are needed when you first open the `MDD-GUI.Rproj` in R;
6. If you are using RStudio, just open the `app.R` in MDD-GUI directory, and click the `Run App` button to launch the application.
  <br>Or use the commend below to complete the same thing. <br>  `shiny::runApp(appDir = "MDD-GUI/")`
