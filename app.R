
library(shiny)
library(rgl)
library(xgboost)
library(misc3d)
library(class)
library(e1071)


ui <- fluidPage(
	column(3,
		wellPanel(
			selectInput(inputId = "select_model", label = "Select model",
				 							choices = list("No model", "Logistic Regression",
				 														 "Second Order LogReg", "Third Order LogReg",
				 														 "K-Nearest Neighbors", 
				 														 "Gradient Boosted LogReg",
				 														 "Gradient Boosted Trees", 
				 														 "Support Vector Machine")),
			sliderInput(inputId = "rasterlength", label = "Select grid",
				 							min = 30, max = 200, step = 10, value = 50),
			helpText("Values above 100 can take long to compute"),
			checkboxInput(inputId = "check_scale", label = "Scale input", value = TRUE),
			uiOutput("hyperparameters"),
			actionButton(inputId = "ab_fit_model", "Compute Decision Boundary"),
			br(),
			br(),
			uiOutput("infotext")
		)
	),
	column(9,
		wellPanel(
			rglwidgetOutput('thewidget1', width = "1000px", height = "1000px")
		)
	)
)


options(rgl.useNULL = TRUE)
server <- function(input, output, session) {
	datenErzeugen = function(anzahl_punkte = 1000, rausch_ein = 0.001, radius = 50) {
		zielBerechnen = function(anzahl_punkte, x, y, dichte) {
			dichteAusgeben = function(x, hoch, tief, tau) {
				return(tau*x*exp(-x/tau)/(tau^2*exp(-1)))
			}
			punkteZiehen = function(anzahl_punkte, hoch, tief, tau) {
				anzahl = 10*anzahl_punkte
				vorschlagsvek = runif(anzahl, min = tief, max = hoch)
				pvek = dichteAusgeben(vorschlagsvek, hoch = hoch, tief = tief, tau = tau)
				idcs = pvek > runif(anzahl)
				return(vorschlagsvek[idcs][1:anzahl_punkte])
			}
			return(data.frame(x = punkteZiehen(anzahl_punkte, x$hoch, x$tief, x$tau),
												y = punkteZiehen(anzahl_punkte, y$hoch, y$tief, y$tau),
												z = punkteZiehen(anzahl_punkte, dichte$hoch, dichte$tief, dichte$hoch)))
		}
		
		seeed = 54321
		
		# Parameter für die Verteilung der Bevölkerung
		x = list(tief = 0, hoch = 10000, tau = 3300)
		y = list(tief = 0, hoch = 60, tau = 25)
		dichte = list(tief = 30, hoch = 3000, tau = 1500)
		
		w0 = -6500
		w_ein = 1/1000
		w_dist = 1/10
		w_dichte = -1
		w_ein_dist = 0.1
		
		set.seed(seeed)
		datensatz = zielBerechnen(anzahl_punkte, x, y, dichte)
		
		# Parabel mit einer Blase rechts oben von der Parabel.
		p_ziel = 1/(1 + exp(-rausch_ein*(w0 + w_ein*datensatz$x + 
																		 	w_dist*datensatz$y + w_dichte*datensatz$z + 
																		 	w_ein_dist * datensatz$y * datensatz$x))) 
		p_ziel = p_ziel - exp(-((datensatz$x - 7000)/1500)^4)*exp(-((datensatz$y - 40)/12)^4)
		
		zielvek = p_ziel > runif(anzahl_punkte)
		
		datensatz = data.frame(datensatz[1:3], zielvar = as.factor(as.numeric(zielvek)))
		return(datensatz)
	}
	
	
	rasterlaenge = 50
	
	datensatz = datenErzeugen()
	
	datensatz_scaled = datensatz
	meanvek = sapply(datensatz_scaled[1:3], mean)
	sdvek = sapply(datensatz_scaled[1:3], sd)
	
	for (zaehler in 1:3) {
		datensatz_scaled[, zaehler] = (datensatz_scaled[, zaehler] - meanvek[zaehler])/sdvek[zaehler]
	}
	
	idcs_auto = datensatz$zielvar == "1"
	
	
	
#	chainStart = reactive({
#		input$ab_fit_model
#		
#		updateActionButton(session = session, inputId = "ab_fit_model",
#											 label = "Computing, please wait")
#		
#		return(1)
#	})
	
	
	# Wait for Input from the grid length selector and change grid
	raster_gen = reactive({
		# chainStart()
		
		rasterliste = lapply(datensatz[c("x", "y", "z")], 
												 function(x) seq(min(x), max(x), length = input$rasterlength))
		raster = expand.grid(rasterliste)
		
		raster_scaled = raster
		for (zaehler in 1:3) {
			raster_scaled[, zaehler] = (raster_scaled[, zaehler] - meanvek[zaehler])/sdvek[zaehler]
		}
		
		list(rasterliste = rasterliste, raster = raster, raster_scaled = raster_scaled)
		
	})
	
	
	output$hyperparameters = renderUI({
		if (is.null(input$select_model)) return()
		
		switch(input$select_model,
					 "K-Nearest Neighbors" = sliderInput(inputId = "knn_slider_k", label = "Select k neighbors",
					 																		min = 1, max = 15, step = 1, value = 1, round = TRUE),
					 "Logistic Regression" = NULL,
					 "Gradient Boosted Trees" = tagList(sliderInput(inputId = "xgbtrees_slider_nrounds",
					 																							 label = "Number of iterations", min = 1, max = 1000,
					 																							 step = 1, value = 4, round = TRUE),
					 																	 sliderInput(inputId = "xgbtrees_slider_maxdepth",
					 																	 						label = "Max depth of trees", min = 1, max = 20,
					 																	 						step = 1, value = 4, round = TRUE)),
					 "Gradient Boosted LogReg" = tagList(sliderInput(inputId = "xgblinear_slider_nrounds",
					 																								label = "Number of iterations", min = 1, max = 1000,
					 																								step = 1, value = 4, round = TRUE)),
					 "Support Vector Machine" = sliderInput(inputId = "svm_slider_gamma", label = "Kernel width",
					 																			 min = 1, max = 20, step = 0.5, value = 10),
					 "Second Order LogReg" = NULL,
					 "Third Order LogReg" = NULL
		)
	})
	
	

	
	
	output$infotext = renderUI({
		if (is.null(input$select_model)) return()
		
		switch(input$select_model,
					 "No model" = helpText(paste0("The plot shows a generic dataset with with three ",
					 														 "independent variables. Each data point shown in the plot belongs ",
					 														 "to one of two classes. Green indicates class '1', red indicates ",
					 														 "class '0'. You can pick one of several classifiers and train it on ",
					 														 "the data. After the classifier is trained, the plot will show the ",
					 														 "resulting decision boundary. After pressing 'Compute Decision Boundary', ",
					 														 "please be patient. This can take a few minutes. The larger the value ",
					 														 "for grid is, the longer it takes (tip: use 30 for your first run, then ",
					 														 "increase). Note the different scales of the axes. Some classifiers are ",
					 														 "thrown off by this. Play around with the option to scale (meaning centering ",
					 														 "and setting variance to one for each variable) to see what classifier ",
					 														 "is sensitive to scaling.")),
					 "Logistic Regression" = helpText(paste0("Logistic Regression is one of the simplest classifiers around. ",
					 														 " In its simplest form, it can only produce flat planes for decision ",
					 														 "boundaries, which does not work well on nonlinear data sets. Despite ",
					 														 "this, Logistic Regression is widely used, because its simplicity can be ",
					 														 "a virtue. This classifier is unlikely to overfit (which really is a ",
					 														 "good thing!), and given a linear separable dataset, it provides the best ",
					 														 "predictions for out-of-sample data.")),
					 "Second Order LogReg" = helpText(paste0("Simple linear Logistic Regression can be enhanced by ",
					 														 "introducing higher order interactions between the predictor variables. ",
					 														 "Here we see the decision boundary resulting from introducing product terms ",
					 														 "like x*x, x*y, x*z and so on. This makes the decision boundary more flexible.")),
					 "Third Order LogReg" = helpText(paste0("Similar to Second Order Logistic Regression, but we also introduced ",
					 														 "all third order terms like x*x*x, x*x*y, x*y*y and so on. Even more ",
					 														 "flexibility results")),
					 "K-Nearest Neighbors" = helpText(paste0("K-Nearest Neighbors (or KNN) is based on a very simple idea. ",
					 															"When you want to classify some hitherto unseen data, why not just look ",
					 															"at the k nearest neighbors in the space of the data and apply the majority ",
					 															"label? The result we see here. KNN produces very flexible decision ",
					 															"boundaries. However, one of the drawbacks is its high sensitivity to ",
					 															"unscaled data. Go ahead and unscale. See and enjoy what happens.")),
					 "Gradient Boosted LogReg" = helpText(paste0("Gradient Boosting is a method that combines several weak ",
					 															"learners to give us a strong learner. In principle, a gradient boosted ",
					 															"model consists of a succession of simple models, where each one is trained ",
					 															"on the residual error of the model before it. In this instance, the weak ",
					 															"learner is logistic regression. However, even a succession of linear ",
					 															"models can not produce more than a linear decision boundary. To be true, ",
					 															"this one was included only out of curiosity.")),
					 "Gradient Boosted Trees" = helpText(paste0("Gradient Boosted Trees is one of the most powerful ",
					 															"machine models around. Instead of using Logistic Regression like ", 
					 															"Gradient Boosted LogReg, it uses Decision Trees, which are weak ",
					 															"learners prone to overfitting because of their high flexibility. ",
					 															"The combination of many of those in a succession of trees, each one trained ",
					 															"on the residuals of the one before it, results in a machine learning ",
					 															"algorithm that out-of-the-box results in very good results and has won ",
					 															"many competitions on kaggle.com.")),
					 "Support Vector Machine" = helpText(paste0("Support Vector Machines were conceived in the 90s and for ",
					 															"quite some time where the premiere method in machine learning, outperforming ",
					 															"most other methods. Recently, other methods have improved (Gradient Boosting, ",
					 															"Deep Learning) and SVMs have fallen back in performance. Nevertheless, they ",
					 															"usable. There are several variants using different kernels. This particular ",
					 															"instance uses a radial kernel, whose bandwidth can be changed."))
					 
		)
	})
	
	
	
	
	
	# Main reactive object to compute models
	raster_out = reactive({
		rasterliste = raster_gen()$rasterliste
		raster = raster_gen()$raster
		raster_scaled = raster_gen()$raster_scaled
		
		daten2use = datensatz
		
		if (input$check_scale) {
			raster = raster_scaled
			daten2use = datensatz_scaled
		}
		
		if (input$select_model == "K-Nearest Neighbors") {
			k = input$knn_slider_k
			
			if (is.null(k)) k = 1
			
			predict_knn = knn(train = daten2use[1:3], test = raster,
												cl = daten2use$zielvar, k = k, prob = TRUE)
			
			predprobs = attributes(predict_knn)$prob
			predlabels = as.integer(as.character(predict_knn))
			
			raster_vorhersage = predprobs*predlabels + (1 - predlabels)*(1 - predprobs)
			
		} else if (input$select_model == "Logistic Regression") {
			
			formel = as.formula("zielvar ~ x + y + z")
			modell_logreg = glm(formula = formel, daten2use, family = "binomial")
			
			raster_vorhersage = predict(modell_logreg, newdata = raster, type = "response")
			
		} else if (input$select_model == "Third Order LogReg") {
			
			formel = as.formula(paste0("zielvar ~ x * y * z + ",
																 "I(x^2*y) + I(x^2*z) + ",
																 "I(y^2*z) + I(y^2*x) + ",
																 "I(z^2*y) + I(z^2*x) + ",
																 "I(y^3) + I(z^3) + I(x^3)"))
			modell_logreg = glm(formula = formel, daten2use, family = "binomial")
			
			raster_vorhersage = predict(modell_logreg, newdata = raster, type = "response")
			
		} else if (input$select_model == "Second Order LogReg") {
			
			formel = as.formula(paste0("zielvar ~ x * y * z + ",
																 "I(x^2) + I(y^2) + I(z^2)"))
			modell_logreg = glm(formula = formel, daten2use, family = "binomial")
			
			raster_vorhersage = predict(modell_logreg, newdata = raster, type = "response")
			
		} else if (input$select_model == "Gradient Boosted Trees") {
			
			nrounds = input$xgbtrees_slider_nrounds
			if (is.null(nrounds)) nrounds = 4
			max_depth = input$xgbtrees_slider_maxdepth
			if (is.null(max_depth)) max_depth = 4
			
			daten2use$zielvar = as.integer(as.character(daten2use$zielvar))
			
			params_xgb = list(booster = "gbtree",
												max_depth = max_depth,
												subsample = 1)
			
			modell_xgboost = xgboost(data = as.matrix(daten2use[1:3]), label = daten2use$zielvar,
															 nrounds = nrounds, verbose = 0, params = params_xgb)
			raster_vorhersage = predict(modell_xgboost, newdata = as.matrix(raster))
			
		} else if (input$select_model == "Gradient Boosted LogReg") {
			
			nrounds = input$xgblinear_slider_nrounds
			if (is.null(nrounds)) nrounds = 4
			
			daten2use$zielvar = as.integer(as.character(daten2use$zielvar))
			
			params_xgb = list(booster = "gblinear",
												subsample = 1)
			
			modell_xgboost = xgboost(data = as.matrix(daten2use[1:3]), label = daten2use$zielvar,
															 nrounds = nrounds, verbose = 0, params = params_xgb)
			raster_vorhersage = predict(modell_xgboost, newdata = as.matrix(raster))
			
		} else if (input$select_model == "Support Vector Machine") {
			
			kernel_gamma = input$svm_slider_gamma
			if (is.null(kernel_gamma)) kernel_gamma = 10
			
			modell_svm = svm(x = daten2use[1:3], y = daten2use$zielvar, probability = TRUE,
											 gamma = kernel_gamma)
			
			raster_vorhersage = attr(predict(modell_svm, newdata = raster, probability = TRUE),
															 which = "probabilities")[, 2]
		}
		
		if (input$select_model != "No model") {
			raster_vorhersage = array(raster_vorhersage, dim = rep(length(rasterliste[[1]]), 3))
			list(rasterliste = rasterliste, raster_vorhersage = raster_vorhersage)
		} else {
			NULL
		}
	})
	
	
	
	
	# Plot all output (data and decision boundary)
	observe({
		
		input$ab_fit_model
		
		open3d()
		plot3d(x = datensatz$x[idcs_auto],
					 y = datensatz$y[idcs_auto],
					 z = datensatz$z[idcs_auto],
					 ylab = "", "", "",
					 col = "green", radius = 70, type = "s")
		spheres3d(x = datensatz$x[!idcs_auto],
							y = datensatz$y[!idcs_auto],
							z = datensatz$z[!idcs_auto],
							col = "red", radius = 70)
		isolate({
			if (!is.null(raster_out())) {
				contour3d(f = raster_out()$raster_vorhersage,
									x = raster_out()$rasterliste$x,
									y = raster_out()$rasterliste$y,
									z = raster_out()$rasterliste$z,
									level = 0.5, add = TRUE)
			}
		})
		
		scene1 <- scene3d()
		rgl.close()
		
		save <- options(rgl.inShiny = TRUE)
		on.exit(options(save))
		
		output$thewidget1 <- renderRglwidget(rglwidget(scene1))
		
	})
	
	
#	observe({
#		input$ab_fit_model
#		plotDecBound()
#		
#		updateActionButton(session = session, inputId = "ab_fit_model",
#											 label = "Compute Decision Boundary")
#		
#		
#		
#	})
	
	
}

# Run the application 
shinyApp(ui = ui, server = server)

