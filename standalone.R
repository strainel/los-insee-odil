endpoint1 <- "http://rdf.insee.fr/sparql"
q1 <- sprintf("
              PREFIX skos:<http://www.w3.org/2004/02/skos/core#>
              
              SELECT  ?poste ?libelle ?notation WHERE {
				?poste skos:inScheme <http://id.insee.fr/codes/nafr2/naf> .
              ?poste skos:prefLabel ?libelle .
              ?poste skos:notation ?notation .
              FILTER(lang(?libelle) = 'fr') .
              FILTER(regex(?libelle, 'boulan')) .
              }
              ")
result_activity <- SPARQL(url=endpoint1, query=q1)$results
print (nrow(result_activity))

mylist_activity <- list()
for (i in 1:nrow(result_activity)) {
  mylist_activity[[result_activity$notation[i]]] <- result_activity$libelle[i]
  }
print(mylist_activity)


q2 <- sprintf("
			PREFIX igeo:<http://rdf.insee.fr/def/geo#>
              
              SELECT ?commune ?code ?nom  WHERE {
              ?commune igeo:codeCommune ?code .
              ?commune igeo:nom ?nom .
              FILTER(regex(?nom, '')) .
              } LIMIT 300
              ")
result_town <- SPARQL(url=endpoint1, query=q2)$results
print(nrow(result_town))
mylist_town <- list()
  for (i in 1:nrow(result_town)) {
    mylist_town[[paste(result_town$code[i],result_town$nom[i])]] <- result_town$code[i]
  }
print(mylist_town)
