# Sinaloa Input-Output Calculator (SinCal)

**SinCal** is an interactive R Shiny dashboard designed to simulate and analyze economic impacts in the state of Sinaloa, Mexico. Using a Bi-Regional Input-Output Model (Sinaloa vs. Rest of Mexico), the application calculates how changes in final demand (shocks) affect Gross Value Added (PIB) and Employment.

[Image of Leontief input output model diagram]

## 📋 Overview

This tool allows policymakers, researchers, and economists to estimate the **multiplier effects** of investments. It answers questions such as: *"If we invest $10 million USD in the Agriculture sector, how many jobs will be created, and how much will the state GDP grow?"*

### Key Features

  * **Bi-Regional Analysis:** Models interactions between 35 sectors in Sinaloa and 35 sectors in the Rest of Mexico.
  * **Two Operation Modes:**
    1.  **Investment Mode (Origin/Destination):** user inputs an investment amount in USD. The app automatically distributes the shock based on supply chain logic.
    2.  **Manual Shock Mode:** User has granular control to input specific demand changes (in MXN) for any sector.
  * **Impact Decomposition:** Break down results into Direct, Indirect, Spillover, and Feedback effects.
  * **Employment Modules:** Estimates specific job creation numbers based on sector productivity.

-----

## 🔧 Technical Architecture

The repository consists of three main logic files:

| File | Description |
| :--- | :--- |
| **`app.R`** | The main entry point. Contains the **UI** (bs4Dash interface) and **Server** logic (reactivity, inputs, output rendering). |
| **`app_helper.R`** | Middleware. Handles data transformation, regional employment matrix calculations, and formats results for the UI. |
| **`leontieff.R`** | The mathematical core. Contains the linear algebra functions to solve the Leontief system and perform Pyatt & Round decompositions. |

-----

## 🧮 Mathematical Background

The core of the simulation is based on the **Leontief Open Model**.

### The Leontief Inverse

The total output vector $x$ is determined by the final demand vector $f$ and the technical coefficient matrix $A$:

$$x = (I - A)^{-1} f = L \cdot f$$

Where $L$ is the Leontief Inverse matrix.

### Bi-Regional Decomposition

The application implements a decomposition of multipliers (found in `leontieff.R`) to isolate regional effects:

1.  **Intra-regional ($M_1$):** Effects generated within the region (Direct + Indirect).
2.  **Spillover ($M_2$):** Demand leaking to the "Rest of the Country."
3.  **Feedback ($M_3$):** Demand that leaves the region, stimulates the rest of the country, and bounces back to demand inputs from the original region.

The total multiplier matrix is decomposed as:
$$L = M_3 \cdot M_2 \cdot M_1$$

-----

## 🚀 Installation & Usage

### Prerequisites

You need **R** installed on your system along with the following packages:

```r
install.packages(c("shiny", "tidyverse", "glue", "DT", "bs4Dash", "shinyFeedback"))
```

### Data Requirements

The application relies on a `data/` directory (not included in the source code snippets) containing the Input-Output Matrices (MIP). You must ensure the following files exist in a `data/` folder relative to `app.R`:

  * `data/mip_sinaloa.tsv`: The Input-Output matrix data.
  * `data/empleos_impuestos.tsv`: Employment totals per sector.
  * `data/input_base.tsv`: Template for input vectors.
  * `data/origen_destino.rds`:  Data used from suggesting investment structures per sector. The tables come from [INEGI](https://www.inegi.org.mx/programas/tod/2018/).

### Running the App

1.  Clone this repository.
2.  Open `app.R` in RStudio.
3.  Click the **"Run App"** button, or run the following command in the console:

<!-- end list -->

```r
shiny::runApp()
```

-----

## 📂 Input Modes

### Mode 1: Investment (Origen/Destino)

Designed for ease of use.

1.  Select a **Target Sector** (e.g., "Construction").
2.  Input **Investment Amount** in USD.
3.  **Slider Logic:** The app presents sliders for intermediate inputs. You can adjust what percentage of inputs are bought locally in Sinaloa vs. imported from the rest of Mexico.

### Mode 2: Manual Shocks

Designed for advanced economists.

  * Provides a spreadsheet-like interface to manually input demand changes (in Millions of MXN) for all 70 sectors (35 Sinaloa + 35 Rest of Country).
  * **File Upload:** You can upload a `.tsv` file containing a column `shocks_millones_mxn` to bulk-load a simulation.

-----

## 📄 License

This project is open-source. Please check the repository for specific license details.
