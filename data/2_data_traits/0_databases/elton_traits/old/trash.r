
	# get for each species a dataframe compiling all species gbif records matching that species (SLOW)
		liste_species <- get_gbifid_(elton$species_raw) 
		length(liste_species) # 15393

	# select the correct gbif record for each species
		table_species_gbif <- NULL
		for (i in 1:length(liste_species)){

		  if (nrow(liste_species[[i]])==1) table_species_gbif <- bind_rows(table_species_gbif, liste_species[[i]])

		  else 
		    if (liste_species[[i]]$status == "ACCEPTED"&liste_species[[i]]$matchtype=="EXACT") 
		      table_species_gbif <- bind_rows(table_species_gbif, liste_species[[i]][liste_species[[i]]$status == "ACCEPTED"&liste_species[[i]]$matchtype == "EXACT",][1,])
		    
		    else if (liste_species[[i]]$status == "SYNONYM"&liste_species[[i]]$matchtype=="EXACT") 
		      table_species_gbif <- bind_rows(table_species_gbif, liste_species[[i]][liste_species[[i]]$status == "SYNONYM"&liste_species[[i]]$matchtype == "EXACT",][1,])
		    
		    else table_species_gbif <- bind_rows(table_species_gbif,liste_species[[i]][1,])
		}



	# clean species table
		species_gbif <- table_species_gbif[c("canonicalname","usagekey","status","matchtype","species","specieskey","order","family","genus")]
		colnames(species_gbif) <- c("canonical_names","gbif_id","status","match_type","accepted_species","accepted_gbif_id","order","family","genus")
		species_gbif <- species_gbif[,c("accepted_gbif_id","accepted_species","order","family","genus")]
		names(species_gbif) <- gsub("accepted_species","species", names(species_gbif))

