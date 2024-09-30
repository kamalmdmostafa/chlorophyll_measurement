# chlorophyll_measurement
Extract chlorophyll with 100% methanol (grinding in liquid nitrogen or by bead beating) in the dark. Use 1 ml Cuvette to measure the absorbance at A652 and A665 visible wavelength
# Chlorophyll Content Analysis Shiny App

This Shiny app allows users to analyze chlorophyll content data, calculate chlorophyll a, b, and total chlorophyll concentrations, and visualize the results with statistical analysis.

## Setup Instructions

### 1. Install R

R is the programming language required to run this app.

1. Go to the [R Project website](https://www.r-project.org/).
2. Click on "CRAN" under the Download section.
3. Choose a mirror close to your location.
4. Select your operating system (Windows, Mac, or Linux).
5. Download the latest version of R.
6. Run the installer and follow the installation prompts.

### 2. Install RStudio

RStudio is an integrated development environment (IDE) that makes it easier to work with R.

1. Go to the [RStudio Download page](https://www.rstudio.com/products/rstudio/download/#download).
2. Download the free RStudio Desktop version for your operating system.
3. Run the installer and follow the installation prompts.

### 3. Install Required Packages

Open RStudio and run the following commands in the console to install the necessary packages:

```r
install.packages(c("shiny", "tidyverse", "ggplot2", "viridis", "svglite"))
```

This will install all the required packages for the app.

## Running the App

1. Download the `app.R` file from this repository.
2. Open RStudio.
3. Use File > Open File to open the `app.R` file.
4. Click the "Run App" button in the top right corner of the script editor, or run the following command in the console:

```r
shiny::runApp("path/to/app.R")
```

Replace "path/to/app.R" with the actual path to where you saved the app.R file.
5. Or, just open the app with R studio and run the app
## Using the App

1. Choose your input method: Manual Entry or CSV Upload.

### For Manual Entry:

- Specify the number of samples you have.
- For each sample, provide a name and paste A652 and A665 values.
- You can paste values directly from Excel or Google Sheets.
- Make sure the number of A652 values matches the number of A665 values for each sample.

### For CSV Upload:

- Prepare a CSV file with columns: Sample, A652, A665
- Upload your CSV file using the file input button.

2. Click 'Analyze Data' to process your input.
3. View results in the Data, Box Plot, and Percent Difference Plot tabs.
4. Use the download buttons to save plots as SVG or PDF files.

## Troubleshooting

If you encounter any issues:

1. Ensure all required packages are installed correctly.
2. Check that you're using the latest versions of R and RStudio.
3. Verify that your input data is in the correct format.

For further assistance, please open an issue in this GitHub repository.

## Contributing

Contributions to improve the app are welcome. Please fork the repository and submit a pull request with your changes.

## License

This project is licensed under the MIT License - see the LICENSE file for details.
