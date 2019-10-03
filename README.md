# Machine-Learning-Visualisation

## Abstract

This is a Shiny app that sets up a simple, three dimensional example dataset with two distinct nonlinearities. The user has the option to pick one of several common machine learning algorithms, choose hyperparameters and then fit the algorithm to the data. The app computes and displays the decision boundary on the actual data.

## Statement of Mission

Binary labelled data with metric predictor variables can be understood as living in an euclidian space, which means that if the number of variables does not exceed three, one can plot the data in a 3D plot. What is nice is that this actually allows to add the output of a classification algorithm like, let's say, logistic regression or a decision tree. All classifiers introduce the so-called "decision boundary" into the space of the data, a two dimensional boundary that separates the elements of one class from the other. The shape of the decision boundary strongly depends on the type of the classifier. Understanding that shape can help to understand what the classifier is doing and where its strengths and weaknesses are. For example, a logistic regression can only produce a decision boundary that is a flat plane and that fails when the data is nonlinear. On the other hand, a decision tree can be so complex that after training it correctly predicts the class of any point in the training data, which however is really the sign of gross overfitting.

I think it is instructive to be able to visualize the output of classification algorithms on 3D data, in particular to be able to play around with the hyperparameters and see how the decision boundary changes. This is the purpose of this app.

## Usage

Download both R scripts into the same folder and open one of them with RStudio. Install all required packages; see server.R for which ones are needed. Run the app. If it gives an error, manually load all packages before running the app again. In the app you will see a 3D plot of the data. Use your mouse to drag and turn the plot. Hold left mouse button to rotate, hold right mouse button to zoom. In the left panel, there is a pull-down menu from which you can choose the algorithm, a slider with which you can select the resolution on which the decision boundary is computed (the higher, the better it is rendered, but the longer it takes to compute) and a checkbox to scale the data before fitting. After you pick the algorithm, you will get UI elements to pick hyperparameters specific to the algorithm. Make your choice, click "Compute Decision Boundary" and then sit back and relax. Some computations take up to two minutes. But it is worth it!

Currently implemented algorithms:
- Logistic regression without interactions
- Logistic regression with interactions (quadratic terms)
- Logistic regression with third-order interactions (cubic terms)
- KNN (hyperparameter k)
- Support Vector machine (Radial kernel, hyperparameter kernel size)
- Gradient Boosted Trees (hyperparameters number of iterations, maximum tree depth)
- Gradient Boosted logistic regression (hyperparameter number of iterations

## Scaling

Scaling centers and normalizes each dimension, which brings them all on the same scale. Some algorithms depend on that. Of the current algorithms only KNN and higher-order logistic regressions are affected by scaling of the data. Support vector machines also suffer, but the implementation in e1071 per default scales the data. Switching it off leads to very bad performance (actually it leads to an error, since the threshold of 0.5 does not lie within the cube).

## Link to working app 

This app is also hosted by the good people from RStudio. You can find it here: https://chralbers.shinyapps.io/machine-learning-visualisation/. Unfortunately, KNN crashes the app when grid size is set above 100.

## To-Do

- Make GUI unresponsive during computation.
- Add another Option: Compute Accuracy on training data using k-fold CV
- Add description box to give some background information on the algorithm
- Add simple Decision Tree
- Add Random Forest
- Add neural network (nnet? Keras?)
- Make it so that computing a new model does not redraw the whole plot, but just replaces the decision boundary. Hope is that this won't reset the view. If there are other ways, use them
- Make option to use different datasets (linear, weakly nonlinear, ...?)
- replace knn from library class with more efficient method.





