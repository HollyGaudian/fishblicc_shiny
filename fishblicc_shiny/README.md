# fishblicc Selectivity Function Interactive Design

A shiny app to use with fishblicc package for fisheries length-frequency data analysis.

This is a work in progress. The software is being developed to provide an interactive application to allow users to edit the prior selectivity model within the `fishblicc` data object. While an interactive application is of limited use if simple selectivity functions are used, an interactive session should help when constructing selectivity mixtures which are needed to explain some length frequency data.

To Do

1) Read csv and create a blicc data object with default settings
2) Allow edits of priors for M/K, Linf, F/K, Galpha, and NBphi
3) Allow "informative" option reducing sd for lognormal for above priors
4) Allow log slope option for selectivity prior
5) Allow "informative" option for selectivity parameters
6) Allow edit of relative catches among gears
