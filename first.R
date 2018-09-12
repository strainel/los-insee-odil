# Prototype ODIL
# Team 6
# LOS hackathon Insee
# 11-13/09/2018

library(shiny)
library(SPARQL)


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
			}
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

			SELECT ?commune ?code ?nom  WHERE {
				?commune igeo:codeCommune ?code .
				?commune igeo:nom ?nom .
				FILTER(regex(lcase(str(?nom)), lcase('%s'))) .
			} LIMIT 300
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
				mylist_town[[paste(result_town$nom[i])]] <- result_town$commune[i]
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
             tabPanel(title="1 - Choix activité",
                      fluidRow(
                        column(8,offset=2,tags$div(HTML("<h4>D'abord choisir son activité, puis sa commune</h4>")))
                      ),
					  fluidRow(
                        column(3,offset=1,tags$div(textInput("myactivity", h3("Activity"),value = ""))),
                        column(3,offset=3,tags$div(textInput("mytown", h3("Town"),value = "")))
                      ),
                      fluidRow(
                        column(5,offset=0,htmlOutput("v_code_activity")),
                        column(5,offset=2,htmlOutput("v_code_town"))
                        )
					)
        )
	)

shinyApp(ui = ui, server = server)