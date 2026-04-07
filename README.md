Repository contains R scripts and their plotting output, intended to facilitate a guided exploration
on four questions regarding change in the contemporary energy sector:

1. To what extent will supply constraints curtail global oil consumption in the next several decades?
2. What is the regional distribution of the world's oil production, how has it changed over time, and what are the geopolitical implications of this changing distribution?
3. How technologically mature are modern renewable sources of electricity like wind and solar, and to what extent will they displace fossil-sourced electricity in the coming decades?
4. What is Louisiana’s role in the global energy economy?

The presentation is targeted for an undergraduate-level course on energy/climate economics; e.g., ECN
326 (Climate Change and Clean Energy Economics: A Global Perspective) at the University of Portland or
ENVS 4261 (Energy and the Environment) at LSU. Thus, the first three questions above are framed somewhat
more provocatively in the presentation itself.

Main.R loads necessary libraries, sets a custom theme and plotting functions to promote graphical
consistency, and sources the scripts for data processing and visualization. Some data is gathered via
the EIA's API, which requires a key; data which is downloaded manually is located in the Data - raw
folder.

Data processing scripts are located in the R Scripts folder and output plots to the Plots folder.
Each script produces one plot as its output (except for Oil Production by Country.R, which produces two
plots, since both use data from the same EIA dataset). Two slides contain visuals not created in the R
scripts: one with a screenshot from Lazard's annual LCOE report, and another with a photo of an
ExxonMobil refinery, taken from the top of the Louisiana State Capitol.
