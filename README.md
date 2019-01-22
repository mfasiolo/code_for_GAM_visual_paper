#### Code for ``Scalable visualisation methods for modern Generalized Additive Models''

Here we provide the code and data for reproducing the results in Fasiolo et al. (2018). In particular:

- `UKData.Rdata` contains the data set on UK electricity demand;
- `Code_for_model_fitting.R` contains the code for fitting the three GAM models considered in the paper;
- `Code_for_plots.R` contains the `mgcViz` code that takes the three fitted GAM objects and produces the plots shown in the paper.
- `l_densCheck.R` and `smooth_uncert_2D.R` contain the code for reproducing the figures in the Supplementary Material.
- `mgcViz_0.1.3.tar.gz` is the source code for the `mgcViz` R package (which is also available from CRAN).

Running the code in `Code_for_model_fitting.R` can take quite a lot of time, especially for the most complex shash GAM model. Please email the first author to request a copy of the fitted GAM objects (unfortunately these files exceed Github's file size limits).

#### References

Fasiolo M., Nedellec R., Goude Y. and Wood S. (2018), ``Scalable visualisation methods for modern Generalized Additive Models'', submitted.