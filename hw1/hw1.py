import os
import re
from nltk.corpus import stopwords
import json
import math
import numpy as np

#英文停止词，set()集合函数消除重复项
list_stopWords=list(set(stopwords.words('english')))
def to_word_list(doc):
	# 用正则表达式取出符合规范的部分
    doc = re.sub("[^a-zA-Z]", " ", doc)
    # 小写化所有的词，删除停用词，并转成词list
    words = [v for v in doc.lower().split() if not v in list_stopWords]
    # 返回words
    return words


def read_all_doc():
	inpath = '../../hw1/nyt_corp0/'

	corpus = []
	for i in range(300):
		fin = open(os.path.join(inpath, str(i)), 'r')
		corpus.append(to_word_list(fin.read()))

	return corpus


def cal_idf(corpus):
	idf = []
	word2id = {}
	for doc in corpus:
		for word in set(doc):
			try:
				idf[word2id[word]] += 1
			except:
				word2id[word] = len(word2id)
				idf.append(1)
	N = len(corpus)
	for v in range(len(idf)):
		idf[v] = math.log10(N/idf[v])

	return idf, word2id


def tfidf(corpus, idf, word2id):
	ret = []
	for doc in corpus:
		tmp = np.zeros(len(word2id))
		for v in doc:
			tmp[word2id[v]] += idf[word2id[v]]/len(doc)
		ret.append(tmp)
	ret = np.vstack(ret)
	return ret



def cooccurence(corpus, word2id):
	word_vecs = np.zeros((len(word2id), len(word2id)))
	for doc in corpus:
		words = set(doc)
		for w1 in words:
			for w2 in words:
				word_vecs[word2id[w1]][word2id[w2]] += 1
	for i in range(len(word2id)):
		word_vecs[i][i] = 0
	return word_vecs


def save(d, fname):
	fout = open(fname, 'w')
	print(json.dumps(d, ensure_ascii = False), file = fout)
	fout.close()


def load_tfidf():
	fin = open('idf.txt', 'r')
	idf = json.loads(fin.read())
	fin.close()
	fin = open('word2id.txt', 'r')
	word2id = json.loads(fin.read())
	fin.close()
	return idf, word2id


def cosine_similar(vec, all_vec):
	# vec.shape: vec_length
	# all_vec.shape: num, vec_length
	norm = np.linalg.norm(all_vec, axis = 1)
	dot = np.matmul(vec, all_vec.transpose(1, 0))
	dot = dot / np.linalg.norm(vec)
	dot = dot / np.linalg.norm(all_vec, axis = 1)

	argsort = np.argsort(dot)
	argsort = argsort[::-1]
	return argsort[1:6] # 去除本身

def distance_similar(vec, all_vec):
	distance = all_vec - vec
	distance = np.linalg.norm(distance, axis = 1)
	argsort = np.argsort(distance)
	return argsort[1:6] #去除本身


import random

if __name__ == '__main__':
	# 统计所有数据
	corpus = read_all_doc()
	idf, word2id = cal_idf(corpus)
	save(idf, 'idf.txt')
	save(word2id, 'word2id.txt')
	word_vecs = cooccurence(corpus, word2id)
	np.save('word_vecs.npy', word_vecs)
	docs = tfidf(corpus, idf, word2id)

	# 随机一篇文档，并求余弦相似与欧氏距离相似的各5篇
	index = random.randint(0, len(corpus))
	doc = docs[index]
	print('选择文档:', index)
	print('余弦相似度:', cosine_similar(doc, docs))
	print('欧氏距离短:', distance_similar(doc, docs))

	# 随机一个单词，并求余弦相似与欧氏距离相似的各5个词语
	word = 'money'
	index = word2id[word]
	word_vec = word_vecs[index]
	print('选择词语:', word)
	print('余弦相似度:', cosine_similar(word_vec, word_vecs))
	print('欧氏距离短:', distance_similar(word_vec, word_vecs))

