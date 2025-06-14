# NYC-Real-Estate-TimeSeries-Regression-Project-R
"ARIMA forecasts future values using past data, trends (differencing), and random errors. My ARIMA(0,1,0) model removed trend but didn’t capture seasonality, so I used ETS for better seasonal forecasting in NYC real estate sales."


## 📊 Project Overview

This project analyzes residential real estate sales in the Elmhurst neighborhood of New York City between 2009–2023 and provides future forecasts for 2024–2025. It applies time-series forecasting, multiple linear regression, and residual analysis to identify pricing patterns, key property drivers, and potential investment opportunities.

The full analysis was performed using **R Language**, with a focus on business forecasting, real estate pricing, and predictive modeling.

---

## 🛠 Tools & Technologies

- R Language
- `forecast` package (ARIMA, ETS models)
- `lm()` function for multiple regression modeling
- `car` package for VIF multicollinearity checks
- Data visualization libraries (ggplot2, base R plotting)
- Time-series transformation (`ts()` function)

---

## 🔬 Analysis Tasks

### Task 1 — Time Series Forecasting (ARIMA)
- Modeled historical quarterly sales data (2009-2023).
- Applied ARIMA(0,1,0) model.
- Forecasted flat sales of ~$134M per quarter for 2024-2025.
- ARIMA model produced unstable confidence intervals with wide error margins.

### Task 2 — Multiple Linear Regression + ETS Forecasting
- Modeled quarterly sales as a function of time and seasonal effects.
- Used ETS(M,A,M) model to capture stronger seasonal patterns.
- ETS model provided improved forecasts with quarterly peaks in Q2.
- Regression showed sales increase marginally over time (~$2.91M/quarter).

### Task 3 — Property-Level Price Modeling
- Built multiple regression model to predict individual sale prices.
- Key predictors: 
  - Gross square feet (positive impact)
  - Property age (negative impact)
  - Total residential units (negative impact)
  - Building type (H9, D1, RR positively correlated with price)
- Model explained ~27% of sale price variation.
- VIF analysis showed multicollinearity between size and unit count.

### Task 4 — Residual Analysis for Investment Insights
- Identified underpriced ("bargain") and overpriced properties.
- Residual analysis helped flag potential undervalued investments.
- Visualized predicted vs. actual sale prices using scatterplots.

---

## 📈 Key Findings

- Gross square footage has the strongest positive effect on sale price.
- Property age and number of units negatively impact price.
- Certain building types (H9, D1, RR) command price premiums.
- ETS model is better suited than ARIMA for neighborhood-level forecasting.
- Residual analysis reveals actionable insights for investment opportunities.

---

## 💡 Recommendations

- Use multiple regression for pricing individual properties.
- Simplify pricing models to reduce multicollinearity between units and building size.
- Leverage ETS forecasts for high-level sales forecasting.
- Apply residual analysis to identify undervalued properties for investors.

---

## 📂 Project Contents

- Time-series forecasting models (ARIMA, ETS)
- Multiple linear regression model
- Multicollinearity (VIF) analysis
- Residual analysis and pricing outlier detection
- Visualizations for forecast models and price predictions

---

## 🚀 Future Work

- Improve model accuracy by incorporating renovation data and building condition.
- Explore spatial analysis by including location coordinates.
- Build interactive dashboards for brokerage pricing tools.

---

## 👩‍💻 Author

**Tuba Anwar**  
Boston University, Metropolitan College  
AD571 - Applied Business Forecasting and Analytics  
April 2025


