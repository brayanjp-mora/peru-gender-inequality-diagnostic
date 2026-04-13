# A Survey-Based Analysis of Gender Inequality in Peru: A Purchasing Power Perspective

## Overview

This project analyzes gender-based differences in purchasing power in Peru using nationally representative household survey data from 2017 to 2024.

Rather than focusing only on average gender gaps, the analysis examines how purchasing power varies across population subgroups defined by socioeconomic, demographic, and geographic characteristics.

The study is guided by the United Nations’ *Leave No One Behind (LNOB)* principle, which emphasizes identifying groups that are systematically disadvantaged. In this project, LNOB dimensions—such as geography, socioeconomic status, and vulnerability—are analyzed through a purchasing power lens to identify groups that consistently experience lower economic outcomes and larger gender disparities.

The objective is to provide a descriptive, policy-relevant diagnostic of inequality by identifying population subgroups that are consistently worse off in terms of purchasing power. This work is intended to inform further analysis, policy discussion, and potential intervention design.

---

## Research Objectives

This project aims to:

- Measure gender-based disparities in purchasing power over time  
- Identify population subgroups with relatively lower purchasing power and larger gender gaps  
- Examine how economic inequality varies across regions and social characteristics  
- Contribute to inclusive, rights-based policy analysis  
- Provide a transparent and adaptable analytical framework  

---

## Methodology (Summary)

The analysis applies a quantitative framework combining descriptive statistics and subgroup identification techniques to examine disparities in purchasing power.

Subgroups are identified based on two criteria:
(i) low median purchasing power, and  
(ii) relatively larger gender gaps compared to a reference group.

This approach is descriptive and does not imply causal relationships.

The analysis adopts a conservative “safe-fail” framework that prioritizes stability, interpretability, and responsible use of disaggregated data. Estimates are retained only when minimum thresholds for sample size and statistical reliability are met. This reduces the risk of unstable estimates, misleading comparisons, and unintended disclosure.

A detailed explanation of the methodology is available in `/docs/methodology.md`.

---

## Data

The analysis uses nationally representative household survey microdata from Peru (2017–2024), complemented by demographic and geographic indicators from official statistical sources.

Due to data access and licensing considerations, raw datasets are not included in this repository. Additional documentation on data sources will be provided as the project develops.

---

## Project Structure

```
Gender Project/
├─ data/        # Raw and processed datasets (excluded from repository)
├─ docs/        # Methodology and supporting documentation
├─ output/      # Tables and figures
├─ R/           # custom functions 
├─ scripts/     # Data cleaning, analysis, and visualization
└─ README.md
```

---

## How to Use

- Run scripts in `/scripts/` sequentially to reproduce data preparation and analysis steps  
- Consult `/docs/methodology.md` for detailed explanations of variables and methodological decisions  
- Outputs (tables and figures) will be stored in `/output/`  

**Note:** Raw data is not included due to licensing restrictions. Users must supply their own data in the expected format.

---

## Outputs

Analytical outputs—including tables, figures, and summary indicators—will be added as the project progresses.

---

## Reproducibility

This project follows a structured workflow separating diagnostics, rule definition, and implementation.

While raw data cannot be shared, all data transformations and analytical steps are scripted to support transparency and reproducibility, conditional on data access. Reproducibility will be further strengthened in future iterations as the workflow is refined.

---

## Intended Audience

This project is intended for:

- Policy analysts  
- Researchers and students  
- Development practitioners  
- Civil society organizations  
- Public institutions  

The framework is designed to be adaptable and open to further development.

---

## Use of AI Tools

AI tools were used to assist with drafting text and implementing some custom functions based on the author’s specified methodology and algorithmic design. All code was reviewed and tested by the author to ensure alignment with the intended logic and expected outputs.

The analytical design, methodological decisions, and interpretation of results remain the sole responsibility of the author.

---

## License

This project is released under the MIT License. You are free to use, modify, and distribute this work with attribution.

---

## Contact

If you have questions, feedback, or ideas for collaboration, please feel free to reach out. All questions, feedback, and ideas are highly welcomed!

— Brayan Mora