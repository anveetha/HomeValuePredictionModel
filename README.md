# Home Value Linear Regression Model

This project implements a linear regression model to predict home prices using data from Realtor.com. The model analyzes various factors influencing home values and provides insights into the key variables driving property prices.

## Dataset

The dataset used in this project combines information from two sources:
- USA Real Estate Dataset from Realtor.com (accessed via Kaggle)
- Standard Co. Population Density for US Zip Codes

## Features

The model considers the following features:
- House size (square feet)
- Lot size (acres)
- Number of bedrooms
- Number of bathrooms
- Population density

## Data Preprocessing

The data preprocessing steps include:
- Removing entries from non-mainland US states
- Handling missing values
- Merging population density data
- Limiting the scope to focus on single-family, suburban homes

## Model Development

The project explores various aspects of model development:
- Exploratory Data Analysis (EDA)
- Feature selection and correlation analysis
- Model fitting and transformation
- Residual analysis
- Influential point analysis

## Key Findings

- Property size and number of bathrooms are significant factors in determining home prices
- A square root transformation improved the model's performance
- The final model uses four predictor variables: bathrooms, lot size, house size, and population density

## Limitations and Future Work

- The model may benefit from incorporating additional relevant factors
- Further refinement could involve creating separate models for different types of properties (e.g., urban, rural, apartments)
- Additional feature engineering and scaling techniques could potentially improve model accuracy

## Tools Used

- R programming language
- Libraries: ggplot2, dplyr, car, MASS, leaps

## Files

- `HomeValueLinearRegressionRCode.R`: R script containing the full analysis
- `HomeValueLinearRegressionReport.pdf`: Detailed report of the project methodology and findings

Citations:
[1] https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/42306819/53939856-5eee-47d2-85dd-163437c20a32/HomeValueLinearRegressionRCode.R
[2] https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/42306819/249cd65a-5007-4982-affd-4765c1b6d66d/HomeValueLinearRegressionReport.pdf
