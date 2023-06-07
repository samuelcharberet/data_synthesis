# Does egestion and excretion chemical composition varies with species traits in terrestrial animals

Here, I combine excretion and egestion chemical composition data with species-level trait data (e.g. bodymass, diet) and confront both to investigate correlations.

The chemical composition data can be found in "1_data/1_data_nutrient". It is further subdivided into "1_data_literature" consisting of a collection of publication data, and "2_data_samples" which is a complementary dataset of chemical analysis that I did. Both datasets have the same structure and are simply combined.

The traits data can be found in "1_data/2_data_traits" and consist of collection of publicly available trait databases published for specific taxonomic groups, mostly at the species level. These databases do not have the same structure and are thus combined using a procedure in which the simplest trait categorization is chosen, and data are chosen based on completeness in cases of ties. 

The R functions running the analyses are found in the "R" repository and their execution order is specified through the "_targets.r" file.



