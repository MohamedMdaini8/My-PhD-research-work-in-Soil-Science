# 🌱 PhD Project: Effects of Soil Salinity and Climate Change on Soil Properties

This repository contains data, analyses, and visualizations related to my PhD research on the combined effects of **soil salinity** and **temperature increase** (climate change) on soil physicochemical properties, carbon and nitrogen fractions, and microbial dynamics in semiarid irrigated soils.

---

## 📘 Project Overview

This research investigates how **salinity** and **rising temperature** interact to affect:

* Soil structure and aggregation
* Soil organic carbon and nitrogen dynamics
* Microbial communities and soil health indicators

All analyses are conducted using **R** and **RStudio** to ensure reproducibility and transparent scientific workflows.

---

## 🧰 About R and RStudio

**R** is an open-source language for statistical computing, data analysis, and visualization, widely used in environmental and soil sciences.

**RStudio** (now Posit) is an integrated development environment (IDE) for R, providing a user-friendly interface for writing scripts, organizing projects, visualizing results, and managing data efficiently.

---

## ⚙️ Installation Guide

1. **Install R**

   * Download from [[CRAN](https://cran.r-project.org/)](https://cran.r-project.org/).
   * Follow instructions for your operating system (Windows, macOS, Linux).

2. **Install RStudio**

   * Download the free RStudio Desktop version from [https://posit.co/download/rstudio/](https://posit.co/download/rstudio/).
   * Install and open RStudio, which will detect your R installation automatically.

---

## 📂 Project Setup

To create a reproducible workflow:

1. Open RStudio → **File → New Project → New Directory → New Project**
2. Name your project (e.g., `PhD_Soil_Salinity_Project`)
3. (Optional) Enable Git for version control
4. Organize your folders as follows:

```
PhD_Soil_Salinity_Project/
│
├── data/                 # Raw and processed datasets
├── scripts/              # R scripts for data analysis and visualization
├── outputs/              # Figures, tables, and statistical results
├── reports/              # R Markdown or Quarto documents
└── README.md             # Project description
```

---

## 🧩 Purpose of R in this Project

R is used to:

* **Import and clean data**: soil properties under different salinity and temperature treatments
* **Perform statistical analyses**: ANOVA, mixed-effects models, correlation analysis
* **Visualize results**: create publication-ready plots of SOC, TN, C/N ratios, soil aggregation, and more
* **Model and predict**: random forests or regression to identify key soil response factors
* **Produce reproducible reports**: integrate text, code, and figures using R Markdown or Quarto

---

## 📈 Outputs and Visualizations

Analyses generate:

* Boxplots and interaction plots showing treatment effects on soil properties
* Heatmaps and correlation matrices of soil parameters
* Variable importance plots for predictive models
* Tables summarizing statistical analyses (ANOVA, post-hoc tests, mixed models)

---

## 🧾 Best Practices

* Keep raw data intact in the `data/` folder
* Use clear and consistent file naming
* Save all figures and tables in `outputs/`
* Document each analysis step in scripts or R Markdown
* Use relative paths for reproducibility
* Enable version control with Git/GitHub for backup and collaboration

---

## 👨‍🔬 Author & Research Context

**Author:** Mohamed Mdaini
**Affiliation:** PhD student at **UPCT (Spain)** and **UTM (Tunisia)**
**Research Focus:** Combined effects of salinity and warming rise on soil aggregation, carbon stabilization, and microbial community structure in semiarid irrigated soils.

