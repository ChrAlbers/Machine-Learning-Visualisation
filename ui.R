library(shiny)

shinyUI(fluidPage(
	useShinyjs(),
  column(3,
  	wellPanel(
  		selectInput(inputId = "select_model", label = "Select model",
  								choices = list("No model", "K-Nearest Neighbors", "Logistic Regression",
  								               "Second Order LogReg", "Third Order LogReg",
  															 "Gradient Boosted Trees", "Gradient Boosted LogReg",
  															 "Support Vector Machine")),
  		sliderInput(inputId = "rasterlength", label = "Select grid",
  								min = 30, max = 200, step = 10, value = 50),
  		checkboxInput(inputId = "check_scale", label = "Scale input", value = TRUE),
  		uiOutput("hyperparameters"),
  		actionButton(inputId = "ab_fit_model", "Compute Decision Boundary")
  )
  ),
  column(9,
  			 wellPanel(
  			 	rglwidgetOutput('thewidget1', width = "1000px", height = "1000px")
  			 )
  			 
  )
      
	)
)
