# Estimating External Validity Bias in RCTs: A Simulation Study

Many, if not most, randomized controlled trials are conducted on sites that are 
chosen in a non-random manner. This can bias the impact estimate if the RCT
sample is not representative of the population of interest for which researchers make
recommendations. If estimates vary across sites and the inclusion of sites in the sample
is correlated with site-level treatment impacts, the estimate obtained on the sample will
not be externally valid. Using a dataset from an already existing RCT, I model different
situations in which researchers oversample sites that are convenient to implement the
treatment in and assess the external validity bias that arises from it. I compare this
bias to a mathematical expression derived in the literature. I nd a very high external
validity bias in samples that are conveniently selected. Additionally, I test a reweighting
procedure that utilizes the inclusion probability of each site and nd that it generally
reduces, but does not eliminate, external validity bias.

This repo contains my [research paper](./estimating-external-validity-bias-rct.pdf)
, my [data sources](./data), and the [simulation code](./code).

## Data Sources
* [RCT data](#https://www.povertyactionlab.org/evaluation/primary-school-deworming-kenya)
* [Kenya zones shapefile](#https://github.com/mikelmaron/kenya-election-data/tree/master/output)
* [Kenya roads shapefile](#https://hub.arcgis.com/datasets/a3e4f55867944bcb890ea35dc0576699_0/data)

I aggregated the number of different types of roads in each ward in QGIS and saved it 
[here](./data/busia_roads_1.csv).