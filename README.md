# Global COVID-19 Spatiotemporal Analysis & Shiny Dashboard

### 📊 Project Overview
This project provides a multi-dimensional analysis of the COVID-19 pandemic's global progression. It combines high-fidelity animated visualizations with an interactive R Shiny dashboard to allow users to interrogate the spread, recovery, and mortality rates across 120+ countries.

Developed as part of a Master's research assignment, the project emphasizes data storytelling and interactive tool development using the R ecosystem.

### 🚀 Key Features
* **Dynamic Spatiotemporal Animation:** A time-series visualization built with `gganimate` that maps the global spread from January 2020 onwards, revealing transmission clusters and velocity.
* **Interactive Shiny Explorer:** A custom UI allowing users to filter by country and metric (Confirmed, Recovered, Deaths) to generate real-time trend reports.
* **Automated Data Pipeline:** Integration with the Johns Hopkins University/GitHub aggregated dataset for up-to-date analysis.
* **Analytical Reporting:** Includes a deep-dive technical document interpreting the patterns revealed by the animation.



---

### 🛠️ Technical Stack
* **Language:** R
* **Framework:** [Shiny](https://shiny.posit.co/)
* **Visualization:** `ggplot2`, `gganimate`, `maps`, `viridis`
* **Data Wrangling:** `dplyr`, `tidyr`, `lubridate`
* **Deployment:** [shinyapps.io](https://www.shinyapps.io/)
* **Live App Link:** [Click here]( https://amogelang-ramatlo.shinyapps.io/covid-19-explorer/)

---

### 📂 Repository Structure
```text
covid-19-explorer/
├── app.R      # Combined UI and Server logic for the Shiny App
├── gif.R      # Script to generate the spatiotemporal GIF
├── data/
│   └── countries-aggregated.csv  
├── docs/
│   ├── app_instructions.pdf    # How to use the Shiny App
│   └── gif_description.pdf     # Description of the generated gif
├── imgs/
│   ├── gif_animation.gif       # The generated GIF
└── README.md
```

---

### 📖 How to Run Locally
1. Ensure you have **R** and **RStudio** installed.
2. Clone this repository:
   ```bash
   git clone https://github.com/amogelang-ramatlo/covid-19-explorer.git
   ```
3. Install dependencies:
   ```r
   install.packages(c("shiny", "ggplot2", "gganimate", "dplyr", "gifski"))
   ```
4. Run the app:
   ```r
   shiny::runApp()
   ```

---

### 📝 Insights & Patterns
The analysis highlights the staggered "wave" patterns across countries. Also shows patterns for the daily changes in confirmed cases, recoveries and deaths.
