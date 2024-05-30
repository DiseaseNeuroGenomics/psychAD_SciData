# PsychAD Scientific Data supplement

This relates to the publication: **Population-scale cross-disorder atlas of the human prefrontal cortex at single-cell resolution**

doi: TBA

## Creating virtual environment

Using [anaconda](https://conda.io/projects/conda/en/latest/index.html), and the *env.yaml* file provided here the following scripts can be run.

```bash
conda env create -f env.yml
conda activate psychad_scidata
```

### psychad_plots.R

This script when run as:

```bash
Rscript psychad_plots.R
```

produces the following files:

#### Figures

- output_dir/plot_Fig2_dxDistr.pdf
- output_dir/plot_Fig2_dxDistr_ad_phenotypes_correlation.pdf
- output_dir/plot_Fig2_pie_age.pdf
- output_dir/plot_Fig2_pie_dxCat.pdf
- output_dir/plot_Fig2_pie_ethnicity.pdf
- output_dir/plot_Fig2_sexDistr.pdf
- output_dir/plot_Fig3_genoAncestryMerged.pdf
- output_dir/plot_Fig3_genoAncestrySnparray.pdf
- output_dir/plot_Fig3_genoSexCheck.pdf
- output_dir/plot_Fig5_dxAgeDist.pdf

#### Text

- output_dir/Table_2.csv
- output_dir/Table_S1.csv
- output_dir/Table_S3.csv

### SciDatafigure_figs4.py

This script when run as:

```bash
python SciDatafigure_figs4.py
```

produces the following outputs:

- output_dir/Boxplot-fig4b-upper.pdf
- output_dir/Boxplot-fig4b-bottom.pdf
- output_dir/SciData_Fig4a.pdf
- output_dir/SciData_Fig4c.pdf
- output_dir/SciData_Fig4d.pdf
