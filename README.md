# Insurance Benefit Valuation Tool (R Shiny)
An easy-to-use Shiny app that calculates life insurance values and premiums using a life table (ages 20–90).

## What it does
- Annuities: whole-life (due), term annuity-due
- Assurances: whole-life, term assurance (end-of-year and “immediate” approx)
- Endowments: pure endowment and endowment
- Premiums: single or level, with initial/premium/claim expenses
- Joint-life: whole-life and temporary annuity-due (basic)
- A chart showing how the premium/value changes with interest rate

## How to run (Windows)
1. Install R and RStudio.
2. Open RStudio → open `app.R`.
3. Install packages (first time only):
   ```r
   install.packages(c("shiny","ggplot2","dplyr"))

## How to use
Choose Policy: X / Y / XY
Set Age, Term, Interest Rate (%)
Pick Benefit: Term / Pure Endowment / Endowment
Pick Timing: End of year / Immediately on death
Pick Premium: Single / Level + set Expenses (%)
Enter Assured Sum (or Annuity Benefit for XY)
Read the Calculated Premium/Value and see the chart

## Notes / Assumptions
Life table ages 20–90
Approximations: immediate ≈ (1+𝑖)0.5×≈(1+i)0.5× EoY; continuous annuity ≈a¨x​−0.5
Keep age + term ≤ 90

## Screenshot of the Shiny App
<img width="1918" height="1016" alt="Screenshot 2025-09-24 190115" src="https://github.com/user-attachments/assets/63a6f0b2-228a-4bc3-bb66-b1a89805491f" />
<img width="1850" height="452" alt="Screenshot 2025-09-24 190301" src="https://github.com/user-attachments/assets/47bc107a-7c1d-4490-8cb1-62f99bd22283" />


