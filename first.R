
library(shiny)
library(SPARQL)


options(encoding = "UTF-8")


### Start of server side ###
server <- function(input, output) 
{

	observeEvent(input$myactivity,{
	
		endpoint1 <- "http://rdf.insee.fr/sparql"
		q1 <- sprintf("
			PREFIX skos:<http://www.w3.org/2004/02/skos/core#>

			SELECT  ?poste ?libelle WHERE {
				?poste skos:inScheme <http://id.insee.fr/codes/nafr2/naf> .
				?poste skos:prefLabel ?libelle .
				FILTER(lang(?libelle) = 'fr') .
				FILTER(regex(?libelle,'%s'))
			}
        ",input$myactivity)
    result_activity <- SPARQL(url=endpoint1, query=q1)$results

	if (nrow(res) > 20) {
		output$v_code_activity_1 = renderUI({
			h3("Too much resultats " nrow(result_activity))
			})
		}
	else {
		output$v_code_activity_1 = renderUI({
			tagList(
			h3(nrow(result_activity)),
			renderDataTable(result_activity)
			)
		}
	})
	
	
	
	
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
                        column(8,offset=2,tags$div(textInput("myactivity", h3("Activity"),value = ""))) 
                      ),
                      fluidRow(
                        column(3,offset=1,htmlOutput("v_code_activity_1"))
                        )
					)
        )
	)

shinyApp(ui = ui, server = server)