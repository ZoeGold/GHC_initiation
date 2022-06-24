Data and code for _"Chimpanzees communicate to coordinate a cultural practice"_ by Goldsborough, Z., Schel, A.M., and E.J.C. van Leeuwen.
[![DOI](https://zenodo.org/badge/340064913.svg)](https://zenodo.org/badge/latestdoi/340064913)

This repository contains the R script _("GHC Initiation.R")_ required to replicate all analyses reported in the manuscript. The files required to run this script are a csv consisting of the data from video-coding of GHC videos _("Handclasp Datacoding MD_postIRR.csv")_, a csv containing data extracted from the larger dataset used to create Figure 2 _("Sunburst 2A_postIRR.csv")_ and a csv with dyadic information like kinship _("Information for CMS Model Handclasp.csv")_. Some files are called "postIRR" as a number of changes were introduced after the inter-rater-reliability (IRR) was completed and the observers agreed on the correct coding. An interactive version of Figure 3 (the sunburst) can be viewed either by running the R script or by downloading and opening the HTML of Figure 3 _("Sunburst2A.html")_.

The only analysis not present in the R code is the determination of whether individuals showed elaboration when initiating a GHC (described in the Results section of the manuscript). This was done manually in Excel according to the following procedure:

1. We identified the bout numbers of the bouts during which the initiator showed several of the GHC-specific initiation behaviors
2. These bouts were isolated from the GHC video dataset
3. After an iniatior performed a gesture, we calculated the response waiting time as they time until they performed another gesture or shaping behavior (so StartTime of the second behavior minus Endtime of first behavior)
4. From this we calculated the minimum, maximum, and average response waiting time

Additionally, we have included an overview of the results of our inter-rater-reliability analyses _("IRR_GHC.xlsx")_. This contains the raw coding of the sub-sample of videos for each observer, as well as a comparison of their coding and the final calculation of the IRR scores. 
