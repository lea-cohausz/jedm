## JEDM

The following files belong to my [EDM-publication](https://educationaldatamining.org/edm2022/proceedings/2022.EDM-short-papers.33/) and my JEDM-publication. 

This repositoy contains six files:

- data.csv: This is the artificial data I simulated and useds in one part of the evaluation. It can be used as a basis for the remaining files in this repository or your own analysis.
- Regression.R: When this is run, it shows the true effects we intended the artifical data to have. Can be used to understand data.csv.
- GetExplanations.ipynb: Can be seen as an examplary script to show how to get LIME explanations from model. The file trains a MLP on the data and then gets explanations for the test set. We only consider the cases where the target is predicted to be 1.
- Exp.csv: The file GetExplanations.ipynb might return.
- PostProcessing.R: Processes the data in the Exp.csv file. Returns Step2_InputData.csv. Note that features that already have a value associated with the opposite prediction will not be considered here.
- Step2_InputData.csv: Can be used to compute what features actually change the value. This is straighforward and can simply be done by changing the values one by one and running the prediction. Note that the model weights are stored in GetExplanations.ipynb.
