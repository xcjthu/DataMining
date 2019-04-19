import sklearn
import numpy as np
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA
from sklearn.manifold import TSNE


def readvec(path):
	fin = open(path, 'r')
	wordvecs = {}
	for line in fin:
		word, vec = line.split('\t')
		vec = np.array([float(d) for d in vec.split(' ')])
		wordvecs[word] = vec
	return wordvecs


if __name__ == '__main__':
	path = '../../HW2/100_word_vector.txt'
	wordvecs = readvec(path)

	X = [wordvecs[key] for key in wordvecs]

	plt.figure()
	p1 = plt.subplot(121)

	p1.set_title('PCA')
	pca = PCA(n_components=2)
	X_pca = pca.fit_transform(X)
	plt.scatter(X_pca[:, 0], X_pca[:, 1])

	
	p2 = plt.subplot(122)
	p2.set_title('T-SNE')
	tsne = TSNE(n_components=2, learning_rate = 100)
	X_tsne = tsne.fit_transform(X)
	plt.scatter(X_tsne[:, 0], X_tsne[:, 1])

	plt.savefig('PCA_TSNE.png')
	

	# plt.show()

