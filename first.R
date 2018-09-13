# Prototype ODIL
# Team 6
# LOS hackathon Insee
# 11-13/09/2018

library(shiny)
library(SPARQL)
library(dplyr)
library(ggplot2)


options(encoding = "UTF-8")


### Start of server side ###
server <- function(input, output)  {

	observeEvent(input$myactivity,{
	
		endpoint1 <- "http://rdf.insee.fr/sparql"
		q1 <- sprintf("
			PREFIX skos:<http://www.w3.org/2004/02/skos/core#>

			SELECT  ?poste ?libelle ?notation WHERE {
				?poste skos:inScheme <http://id.insee.fr/codes/nafr2/naf> .
				?poste skos:prefLabel ?libelle .
				?poste skos:notation ?notation .
				FILTER(lang(?libelle) = 'fr') .
				FILTER(regex(?libelle, '%s')) .
			} LIMIT 200
        ",input$myactivity)
		result_activity <- SPARQL(url=endpoint1, query=q1)$results

		if (nrow(result_activity) > 20) {
			output$v_code_activity = renderUI({
				h5(paste("Too much resultats",nrow(result_activity)))
			})
		}
		else {
			mylist_activity <- list()
			for (i in 1:nrow(result_activity)) {
				mylist_activity[[paste(result_activity$notation[i],result_activity$libelle[i])]] <- result_activity$notation[i]
			}
			#print(mylist_activity)
			output$v_code_activity = renderUI({
				tagList(
				h5(nrow(result_activity)),
				radioButtons("radio_activity", h5("Matching activities"), choices = mylist_activity)
				)
			})
		}
	
	})
	

	observeEvent(input$mytown,{
	
		endpoint2 <- "http://rdf.insee.fr/sparql"
		q2 <- sprintf("
			PREFIX igeo:<http://rdf.insee.fr/def/geo#>

			SELECT ?commune ?code ?nom ?depcom  WHERE {
				?commune igeo:codeCommune ?code .
				?commune igeo:nom ?nom .
				FILTER(regex(lcase(str(?nom)), lcase('%s'))) .
				BIND (str(?code) as ?depcom)
			} LIMIT 200
        ",input$mytown)
		result_town <- SPARQL(url=endpoint2, query=q2)$results

		if (nrow(result_town) > 20) {
			output$v_code_town = renderUI({
				h5(paste("Too much resultats",nrow(result_town)))
			})
		}
		else {
			mylist_town <- list()
			for (i in 1:nrow(result_town)) {
				mylist_town[[paste(result_town$nom[i])]] <- result_town$depcom[i]
			}
			print(mylist_town)
			output$v_code_town = renderUI({
				tagList(
				h5(nrow(result_town)),
				radioButtons("radio_town", h5("Matching towns"), choices = mylist_town)
				)
			})
		}
	
	})

	observe({
	
		output$s_poplegale = renderUI({
				tagList(
				h5(input$radio_town),
				h5(input$radio_activity),
				h5(paste("Legal pop (2015) :", getpoplegale(input$radio_town))),
				h5(paste("Buildings permits (2017) :", getpermits('FRK21')))
				)
		})
		
		output$s_boxplotpoplegale = renderPlot({
				db <- getbarplotpop(input$radio_town)
				barplot(db$poptot, names.arg=db$age, ylab="Pop", xlab="Age")
		})
	})

	getpoplegale <- function (depcom) {
		endpoint2 <- "http://rdf.insee.fr/sparql"
		q3 <- sprintf("
			PREFIX idemo:<http://rdf.insee.fr/def/demo#>
			PREFIX igeo:<http://rdf.insee.fr/def/geo#>

			SELECT ?popTotale ?d WHERE {
				?commune igeo:codeCommune '%s'^^xsd:token .
				?commune igeo:nom ?nom .
				?commune idemo:population ?popLeg .
				?popLeg idemo:populationTotale ?popTotale ; idemo:date ?date .
				BIND(str(?date) as ?d) .
			}
        ",depcom)
		result <- SPARQL(url=endpoint2, query=q3)$results
		print(result)
		return (result$popTotale[nrow(result)])
	}
		
	getpermits <- function (depcom) {
		endpoint2 <- "http://graphdb.linked-open-statistics.org/repositories/plosh"
		q3 <- sprintf("
			PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
			PREFIX qb: <http://purl.org/linked-data/cube#>
			PREFIX sdmx-dimension: <http://purl.org/linked-data/sdmx/2009/dimension#>
			PREFIX mes: <http://id.insee.fr/meta/mesure/>
			PREFIX skos:<http://www.w3.org/2004/02/skos/core#>
			select ?obs ?year ?bp  ?nuts ?notation
			from <http://id.linked-open-statistics.org/graphes/BUILDING_PERMITS>
			from <http://id.linked-open-statistics.org/graphes/codes>
			where {?obs a qb:Observation .
				   ?obs <http://id.insee.fr/meta/dimension/GEO/NUTS> ?nuts .
				   ?obs sdmx-dimension:timePeriod ?year .
				   ?obs mes:NBPE ?bp .
				   ?nuts skos:notation ?notation .
				   ?nuts skos:notation '%s' .
			}
        ",depcom)
		result <- SPARQL(url=endpoint2, query=q3)$results
		print(result)
		return (result$bp[nrow(result)])
	}
	
	getbarplotpop <- function (depcom) {
		endpoint2 <- "http://graphdb.linked-open-statistics.org/repositories/pop5"
		q3 <- sprintf("
			PREFIX qb: <http://purl.org/linked-data/cube#>
			PREFIX mes: <http://id.insee.fr/meta/mesure/>
			  PREFIX cod-age: <http://id.insee.fr/codes/ageq65/>
			  PREFIX dim: <http://id.insee.fr/meta/dimension/>
			  PREFIX cog2017-dim: <http://id.insee.fr/meta/cog2017/dimension/>
			  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
			  select ?obs ?pop  ?age ?depcom
			  from <http://rdf.insee.fr/graphes/demo/pop5>
					  where {?obs a qb:Observation .
					  ?obs mes:pop15Plus ?pop .
					  ?obs dim:ageq65 ?ag .
					  ?ag skos:notation ?age .
					  ?obs cog2017-dim:DepartementOuCommuneOuArrondissementMunicipal ?depcom .
					  ?depcom skos:notation '%s' .
					  }
        ",depcom)
		result <- SPARQL(url=endpoint2, query=q3)$results

		database <- result %>%
		  group_by(age) %>%
		  summarise(poptot = sum(pop))
		return (database)	
	}
	
		
		
}

ui <- fluidPage(
	navbarPage(title="LOS - ODIL",
        tabPanel(title="Accueil",
                      fluidRow(
                        column(6,offset=3,tags$div(HTML("<h3><b>ODIL ou LIDO...</b></h3>")))
                      ),
                      fluidRow(
                        column(6,offset=3,tags$div(HTML("<h4>Blablabla</h4>")))
                      ),
                      fluidRow(
                        column(6,offset=3,tags$div(HTML("<h4>Blablabla</h4>")))
                      ),
                      tags$br(),
                      HTML('<center><img border="0" src="http://www.w3.org/RDF/icons/rdf_w3c_icon.128" alt="RDF Resource Description Framework Icon"/></center>')
                      ),
        tabPanel(title="(1) Criteria",
                      fluidRow(
                        column(8,offset=2,tags$div(HTML("<h4>First... select activity and town</h4>")))
                      ),
					  fluidRow(
                        column(3,offset=1,tags$div(textInput("myactivity", h3("Activity"),value = ""))),
                        column(3,offset=3,tags$div(textInput("mytown", h3("Town"),value = "")))
                      ),
                      fluidRow(
                        column(5,offset=0,htmlOutput("v_code_activity")),
                        column(5,offset=2,htmlOutput("v_code_town"))
                      )
					),
        tabPanel(title="(2) Statistics",
                      fluidRow(
                        column(8,offset=2,tags$div(HTML("<h4>Second... enjoy some statistics</h4>")))
                      ),
                      fluidRow(
                        column(3,offset=1,htmlOutput("s_poplegale")),
                        column(3,offset=3,plotOutput("s_boxplotpoplegale"))
                      )
        )
    )
)

shinyApp(ui = ui, server = server)