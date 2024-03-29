# Random Forests for Regression with Missing Entries
These are specific codes used in the articles:

* <a href="https://doi.org/10.1080/10485252.2023.2219783" target="_blank"> Irving Gómez-Méndez & Emilien Joly (2023). On the consistency of a random forest algorithm in the presence of missing entries,
Journal of Nonparametric Statistics. </a>
* <a href="https://doi.org/10.1080/00949655.2022.2163646" target="_blank"> Irving Gómez-Méndez & Emilien Joly (2023). Regression with missing data, a comparison study of techniques based on random forests,
Journal of Statistical Computation and Simulation. </a>

Examples on the use of these codes as well as updated versions can be found in https://github.com/IrvingGomez/Random_forests_with_missing_values

---
In Source it can be found "random_forests_with_missing_values.R" which contains the needed functions to construct the random forests with missing values using the approach proposed in "On the consistency of a random forest algorithm in the presence of missing entries".

The file Create_datasets contains the codes to create the training datasets and the testing datasets. While the file Create_RF_and_Predict_RF contains the codes to create the Random Forests using our approach and to predict new observations with missing values accordingly to the procedure described in "Regression with Missing Data, a Comparison Study of Techniques Based on Random Forests"

Finally, the file Calculate_MSE contains the codes to calculate the MSE.

---
**License**: All the codes are under the [GNU GPL v3](https://www.gnu.org/licenses/gpl.html) license or any posterior version.

:copyright: (21-05-2021) Irving Gómez Méndez.
