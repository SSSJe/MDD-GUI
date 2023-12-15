# MDD-GUI
Website for Visualizing the snRNA-seq Profile of the Brodmann Area 9 (BA9) of the Post-Mortem Brain Tissue of 17 Control Subjects and 17 Major Depressive Disorder (MDD) Cases. Clinck the PMID to see the paper more information. [PMID: 32341540](https://doi.org/10.1038/s41593-020-0621-y).

## Data Download
Please download the data from this link at below.  

[Download DATA](https://pan.baidu.com/s/1dWleMYH6Vh62iuLYzcD5kQ?pwd=yxfn)  

The data consisted of two parts: **scMDD.obj.exp.all.rds**; **scMDD.obj.meta.all.rds**.  
Be sure the data is downloaded and stored in the same directory as app.R and MDD-GUI.Rproj.  
And then you can  perform the steps in the method. 

## Method
There is a method to how to run the MDD-GUI website.
1. Click `Code` button on the top of this page, then click [Download ZIP](https://github.com/SSSJe/MDD-GUI/archive/refs/heads/main.zip);
2. Unzip the file to your working directory;
3. Double click `MDD-GUI.Rproj` to open the project;
4. Make sure the `renv` package is install automatically;
  > if not install `renv` package, please <br>
  > install.package('renv')
5. Next, use `renv::restore()` to install all other packages which are needed when you first open the `MDD-GUI.Rproj` in R;
6. Before runApp, please make sure that you [download the data](https://pan.baidu.com/s/1dWleMYH6Vh62iuLYzcD5kQ?pwd=yxfn) which the website needed.
6. If you are using RStudio, just open the `app.R` in MDD-GUI directory, and click the `Run App` button to launch the application.
  <br>Or use the commend below to complete the same thing. <br>  `shiny::runApp(appDir = "./")`
