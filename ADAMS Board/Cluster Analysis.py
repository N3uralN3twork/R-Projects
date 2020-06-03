#########################################################
###           1. Set the working directory            ###
#########################################################
import os
abspath = os.path.abspath("C:/Users/miqui/OneDrive/R Projects/ADAMS Board")
os.chdir(abspath)
#########################################################
###           2. Import Data and Libraries            ###
########################################################
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.cluster import KMeans, AgglomerativeClustering, DBSCAN
from sklearn.preprocessing import scale
from sklearn import metrics
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis, QuadraticDiscriminantAnalysis
sns.set()

"Import the Dataset:"
Numeric = pd.read_csv("Numeric.csv", header=0)
y = Numeric["Category"]
X = Numeric[["ZipCode", "Age"]]
#There are 4 actual categories:
    # Board Only
    # Medicaid Only
    # Both
    # Neither

# Priors:
y.value_counts()/13458

"Perform Cluster Analysis:"

# KMeans Clustering
km = KMeans(n_clusters=4).fit(Numeric)
Numeric["Labels"] = km.labels_

plt.figure(figsize=(12, 8))
sns.scatterplot(Numeric["ZipCode"], Numeric["Age"], hue=Numeric["Labels"],
                palette=sns.color_palette("hls", 3))
plt.title("KMeans with 3 Clusters")
plt.show()

# Hierarchical Clustering

agglom = AgglomerativeClustering(n_clusters=4, linkage='average').fit(Numeric)
Numeric['Labels'] = agglom.labels_

plt.figure(figsize=(12, 8))
sns.scatterplot(Numeric["TotalNum"], Numeric["Age"], hue=Numeric["Labels"],
                palette=sns.color_palette("hls", 4))
plt.title("Hierarchical with 4 Clusters")
plt.show()



"Perform Discriminant Analysis:"

# Compute DBSCAN:
db = DBSCAN(eps=0.3, min_samples=10).fit(X)
core_samples_mask = np.zeros_like(db.labels_, dtype=bool)
core_samples_mask[db.core_sample_indices_] = True
labels = db.labels_

# Number of clusters in labels, ignoring noise if present
n_clusters_ = len(set(labels)) - (1 if -1 in labels else 0)
n_noise_ = list(labels).count(-1)

# Metrics
print(f"Estimated number of clusters: {n_clusters_}")
print(f"Estimated number of noise points: {n_noise_}")
print(f"Homogeneity: {metrics.homogeneity_score(y, labels):.3f}")
print(f"Completeness: {metrics.completeness_score(y, labels):.3f}")
print(f"V-measure: {metrics.v_measure_score(y, labels):.3f}")
print(f"Adjusted Rand Index: {metrics.adjusted_rand_score(y, labels):.3f}")
print(f"Adjusted Mutual Information: {metrics.adjusted_mutual_info_score(y, labels):.3f}")
print(f"Silhouette Coefficient: {metrics.silhouette_score(X, labels):.3f}")


qda = QuadraticDiscriminantAnalysis().fit(X, y)
lda = LinearDiscriminantAnalysis(priors=).fit(X, y)

# Mean Accuracy
print(lda.score(X, y))
print(qda.score(X,y))

# Priors
print(lda.priors_) # Just the proportions of each class