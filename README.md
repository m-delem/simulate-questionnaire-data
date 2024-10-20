# Simulating Likert-type questionnaire data with R  
<img src='images/likert.png' align="left" width="200" /> 

I have been working on multiple projects that involved analysing several questionnaires with "Likert scales", i.e. ordinal variables or pseudo-continuous variables obtained by summing all the ordinal items. Before engaging into the experiments proper, robust power analyses, code testing, sanity checks, etc. should be conducted to ensure that the data collection and analysis will be as smooth as possible. In several complex settings such as multivariate analyses or multilevel modelling, simulating data can be a powerful tool to test the code, the power of the analyses, the robustness of the models, etc. 

This repository contains an R script (`scripts/simulate_questionnaires.R`) with code to simulate data from two mental imagery questionnaires I used often: the [Vividness of Visual Imagery Questionnaire (VVIQ)](https://bpspsychub.onlinelibrary.wiley.com/doi/10.1111/j.2044-8295.1973.tb01322.x) and the [Object-Spatial Imagery and Verbal Questionnaire](https://onlinelibrary.wiley.com/doi/10.1002/acp.1473). The script is heavily commented to explain the rationale behind the code and the choices made. The script is also designed to be easily adaptable to other questionnaires. It shows methods to:

- Simulate score distributions from various types of information (e.g., quantile percentages, means, sd, skewness).

- Simulate different distributions for sub-scales or sub-groups in the sample.

- Correlate sub-scales with different types of distributions.

- Simulate individual ordinal items from the total scores of each subject.

I hope this script will be useful to you!

> This repository is a Quarto project endowed with a `renv` R environment to ensure the stability of the packages used. The repository is based on [this Quarto project template](https://github.com/m-delem/my-quarto-template): you can find a quick tutorial to use this project structure and an in-depth explanation of its elements in the README of the template.
