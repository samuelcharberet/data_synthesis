# Data synthesis

Egestion and excretion elemental composition and stoichiometry likely depend on species and individual
traits (Sitters et al., 2014). Therefore, depending on the animal community, as well as environmental conditions,
we can expect contrasting effects of resource consumption and egestion by animals on nutrient cycling
(Bardgett and Wardle, 2003), likely impacting nutrients availability and relative proportion in the soil, possibly
influencing competition between plants and vegetation diversity and structure (Barthelemy et al., 2015;
Gillet et al., 2010; van der Waal et al., 2011). But knowledge on interspecific variation of C:N:P egestion and
excretion content and stoichiometry in terrestrial animals are still lacking for a large number of species, and no
general study undertook the task of describing this interspecific variability, although some studies investigate
these questions in a relatively small number of species (e.g. Frank et al., 2017; le Roux et al., 2020; Sitters et al.,
2014. It would be helpful to know what are the main traits driving this interspecific variation, and whether it
is possible to organize this diversity in functional groups of recycling. Likely to have an effect are traits related
to diet, physiology, body size and body composition. Using a combination of literature survey and chemical
analysis of our samples, I will continue to build a database that will allow to test the effect of different
qualitative and quantitative traits on egestions/excretions chemical composition, as well as estimating the net
nutrient fluxes in various ecosystem contexts. 

Here, I combine excretion and egestion chemical composition data with species-level trait data (e.g. bodymass, diet) and confront both to investigate correlations.

The chemical composition data can be found in "1_data/1_data_nutrient". It is further subdivided into "1_data_literature" consisting of a collection of publication data, and "2_data_samples" which is a complementary dataset of chemical analysis that I did. Both datasets have the same structure and are simply combined.

The traits data can be found in "1_data/2_data_traits" and consist of collection of publicly available trait databases published for specific taxonomic groups, mostly at the species level. These databases do not have the same structure and are thus combined using a procedure in which the simplest trait categorization is chosen, and data are chosen based on completeness.

The R functions running the analyses are found in the "R" repository and their execution order is specified through the "_targets.r" file.



