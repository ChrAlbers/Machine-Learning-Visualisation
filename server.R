# - Add Action button for model fitting
# - Add Accuracy, AUC
# - Add xgboost-linear
# - Add keras
# - Add ranger
# - Rather than redraw whole plot, remove old surface and add new one.
# - After Clicking fit_model, deactivate the button and only make it accessible
#   after model finished.
# - Add description box for information on model.


library(shiny)
library(shinyjs)
library(rgl)
library(xgboost)
library(misc3d)
library(class)
library(e1071)

# Erzeugen der Daten
datenErzeugen = function(anzahl_punkte = 1000, rausch_ein = 0.001, radius = 50) {
	bevoelkerungBerechnen = function(anzahl_punkte, einkommen, distanz, dichte) {
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
		return(data.frame(Einkommen = punkteZiehen(anzahl_punkte, einkommen$hoch, einkommen$tief, einkommen$tau),
											Distanz = punkteZiehen(anzahl_punkte, distanz$hoch, distanz$tief, distanz$tau),
											Einwohnerdichte = punkteZiehen(anzahl_punkte, dichte$hoch, dichte$tief, dichte$hoch)))
	}
	
	seeed = 54321
	
	# Parameter für die Verteilung der Bevölkerung
	einkommen = list(tief = 0, hoch = 10000, tau = 3300)
	distanz = list(tief = 0, hoch = 60, tau = 25)
	dichte = list(tief = 30, hoch = 3000, tau = 1500)
	
	w0 = -6500
	w_ein = 1/1000
	w_dist = 1/10
	w_dichte = -1
	w_ein_dist = 0.1
	
	set.seed(seeed)
	autobesitz = bevoelkerungBerechnen(anzahl_punkte, einkommen, distanz, dichte)
	
	# Parabel mit einer Blase rechts oben von der Parabel.
	p_Auto = 1/(1 + exp(-rausch_ein*(w0 + w_ein*autobesitz$Einkommen + 
																	 	w_dist*autobesitz$Distanz + w_dichte*autobesitz$Einwohnerdichte + 
																	 	w_ein_dist * autobesitz$Distanz * autobesitz$Einkommen))) 
	p_Auto = p_Auto - exp(-((autobesitz$Einkommen - 7000)/1500)^4)*exp(-((autobesitz$Distanz - 40)/12)^4)
	
	Auto = p_Auto > runif(anzahl_punkte)
	
	autobesitz = data.frame(autobesitz[1:3], Autobesitz = as.factor(as.numeric(Auto)))
	return(autobesitz)
}


rasterlaenge = 50

autodaten = datenErzeugen()

autodaten_scaled = autodaten
meanvek = sapply(autodaten_scaled[1:3], mean)
sdvek = sapply(autodaten_scaled[1:3], sd)

for (zaehler in 1:3) {
	autodaten_scaled[, zaehler] = (autodaten_scaled[, zaehler] - meanvek[zaehler])/sdvek[zaehler]
}

idcs_auto = autodaten$Autobesitz == "1"


# rasterliste = lapply(autodaten[c("Einkommen", "Distanz", "Einwohnerdichte")],
# 										 function(x) seq(min(x), max(x), length = rasterlaenge))
# raster = expand.grid(rasterliste)
# 
# raster_scaled = raster
# for (zaehler in 1:3) {
# 	raster_scaled[, zaehler] = (raster_scaled[, zaehler] - meanvek[zaehler])/sdvek[zaehler]
# }



options(rgl.useNULL = TRUE)
shinyServer(function(input, output, session) {
	
	# datlag = reactiveValues(currently_computing = FALSE)
	
	# To be implemented
	#observe({
	#	datlag$currently_computing
	#	actionButton(inputId = "ab_fit_model", label = "Compute the Decision Boundary")
	#})
	
	chainStart = reactive({
		input$ab_fit_model
		
		updateActionButton(session = session, inputId = "ab_fit_model",
											 label = "Computing, please wait")
		
		disable(id = "ab_fit_model")
		
		return(1)
	})
	
	
	# Wait for Input from the grid length selector and change grid
	raster_gen = reactive({
		chainStart()
		
		rasterliste = lapply(autodaten[c("Einkommen", "Distanz", "Einwohnerdichte")], 
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
	
	
	
	
	

	
	# Main reactive object to compute models
	raster_out = reactive({
		rasterliste = raster_gen()$rasterliste
		raster = raster_gen()$raster
		raster_scaled = raster_gen()$raster_scaled
		
		daten2use = autodaten
		
		if (input$check_scale) {
			raster = raster_scaled
			daten2use = autodaten_scaled
		}
		
		if (input$select_model == "K-Nearest Neighbors") {
			k = input$knn_slider_k
			
			if (is.null(k)) k = 1
			
			predict_knn = knn(train = daten2use[1:3], test = raster,
												cl = daten2use$Autobesitz, k = k, prob = TRUE)
			
			predprobs = attributes(predict_knn)$prob
			predlabels = as.integer(as.character(predict_knn))
			
			raster_vorhersage = predprobs*predlabels + (1 - predlabels)*(1 - predprobs)
			
		} else if (input$select_model == "Logistic Regression") {
			
			formel = as.formula("Autobesitz ~ Einkommen + Distanz + Einwohnerdichte")
			modell_logreg = glm(formula = formel, daten2use, family = "binomial")
			
			raster_vorhersage = predict(modell_logreg, newdata = raster, type = "response")
			
		} else if (input$select_model == "Third Order LogReg") {
		  
		  formel = as.formula(paste0("Autobesitz ~ Einkommen * Distanz * Einwohnerdichte + ",
		                             "I(Einkommen^2*Distanz) + I(Einkommen^2*Einwohnerdichte) + ",
		                             "I(Distanz^2*Einwohnerdichte) + I(Distanz^2*Einkommen) + ",
		                             "I(Einwohnerdichte^2*Distanz) + I(Einwohnerdichte^2*Einkommen) + ",
		                             "I(Distanz^3) + I(Einwohnerdichte^3) + I(Einkommen^3)"))
		  modell_logreg = glm(formula = formel, daten2use, family = "binomial")
		  
		  raster_vorhersage = predict(modell_logreg, newdata = raster, type = "response")
		  
		} else if (input$select_model == "Second Order LogReg") {
		  
		  formel = as.formula(paste0("Autobesitz ~ Einkommen * Distanz * Einwohnerdichte + ",
		  													 "I(Einkommen^2) + I(Distanz^2) + I(Einwohnerdichte^2)"))
		  modell_logreg = glm(formula = formel, daten2use, family = "binomial")
		  
		  raster_vorhersage = predict(modell_logreg, newdata = raster, type = "response")
		  
		} else if (input$select_model == "Gradient Boosted Trees") {
			
			nrounds = input$xgbtrees_slider_nrounds
			if (is.null(nrounds)) nrounds = 4
			max_depth = input$xgbtrees_slider_maxdepth
			if (is.null(max_depth)) max_depth = 4
			
			daten2use$Autobesitz = as.integer(as.character(daten2use$Autobesitz))
			
			params_xgb = list(booster = "gbtree",
												max_depth = max_depth,
												subsample = 1)
			
			modell_xgboost = xgboost(data = as.matrix(daten2use[1:3]), label = daten2use$Autobesitz,
															 nrounds = nrounds, verbose = 0, params = params_xgb)
			raster_vorhersage = predict(modell_xgboost, newdata = as.matrix(raster))
			
		} else if (input$select_model == "Gradient Boosted LogReg") {
		  
		  nrounds = input$xgblinear_slider_nrounds
		  if (is.null(nrounds)) nrounds = 4
		  
		  daten2use$Autobesitz = as.integer(as.character(daten2use$Autobesitz))
		  
		  params_xgb = list(booster = "gblinear",
		                    subsample = 1)
		  
		  modell_xgboost = xgboost(data = as.matrix(daten2use[1:3]), label = daten2use$Autobesitz,
		                           nrounds = nrounds, verbose = 0, params = params_xgb)
		  raster_vorhersage = predict(modell_xgboost, newdata = as.matrix(raster))

		} else if (input$select_model == "Support Vector Machine") {
			
			kernel_gamma = input$svm_slider_gamma
			if (is.null(kernel_gamma)) kernel_gamma = 10
			
			modell_svm = svm(x = daten2use[1:3], y = daten2use$Autobesitz, probability = TRUE,
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
		plot3d(x = autodaten$Einkommen[idcs_auto],
					 y = autodaten$Distanz[idcs_auto],
					 z = autodaten$Einwohnerdichte[idcs_auto],
					 ylab = "", "", "",
					 col = "green", radius = 70, type = "s")
		spheres3d(x = autodaten$Einkommen[!idcs_auto],
							y = autodaten$Distanz[!idcs_auto],
							z = autodaten$Einwohnerdichte[!idcs_auto],
							col = "red", radius = 70)
		isolate({
			if (!is.null(raster_out())) {
				contour3d(f = raster_out()$raster_vorhersage,
									x = raster_out()$rasterliste$Einkommen,
									y = raster_out()$rasterliste$Distanz,
									z = raster_out()$rasterliste$Einwohnerdichte,
									level = 0.5, add = TRUE)
			}
		})
		
		scene1 <- scene3d()
		rgl.close()
		
		save <- options(rgl.inShiny = TRUE)
		on.exit(options(save))
		
		output$thewidget1 <- renderRglwidget(rglwidget(scene1))
		
		updateActionButton(session = session, inputId = "ab_fit_model",
											 label = "Compute Decision Boundary")
		
		enable(id = "ab_fit_model")
	})
})

