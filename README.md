Data and code for "Chimpanzees communicate to coordinate a cultural practice" by Goldsborough, Z., Schel, A.M., and E.J.C. van Leeuwen.

This repository contains the R script ("GHC Initiation.R") required to replicate all analyses reported in the manuscript. The files required to run this script are a csv consisting of the data from video-coding of GHC videos ("Handclasp Datacoding MD_postIRR.csv") and a csv containing data extracted from the larger dataset used to create Figure 2 ("Sunburst 2A_postIRR.csv"). Both files are called "postIRR" as a number of changes were introduced after the inter-rater-reliability (IRR) was completed and the observers agreed on the correct coding. An interactive version of Figure 2 can be viewed either by running the R script or by downloading and opening the HTML of Figure 2 ("Sunburst2A.html").

The only analysis not present in the R code is the determination of whether individuals showed elaboration when initiating a GHC (described in the Results section of the manuscript). This was done manually in Excel according to the following procedure:

We identified the bout numbers of the bouts during which the initiator showed several of the GHC-specific initiation behaviors
These bouts were isolated from the GHC video dataset
After an iniatior performed a gesture, we calculated the response waiting time as they time until they performed another gesture or shaping behavior (so StartTime of the second behavior minus Endtime of first behavior)
From this we calculated the minimum, maximum, and average response waiting time
