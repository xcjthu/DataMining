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
	files = os.listdir(inpath)
	for f in files:
		fin = open(os.path.join(inpath, f), 'r')
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


def tfidf(doc, idf, word2id):
	doc = to_word_list(doc)
	ret = numpy.zeros(len(word2id))
	for v in doc:
		ret[word2id[v]] += idf[word2id[v]]/len(doc)
	return ret

def save(d, fname):
	fout = open(fname, 'w')
	print(json.dumps(d, ensure_ascii = False), file = fout)
	fout.close()

def load():
	fin = open('idf.txt', 'r')
	idf = json.loads(fin.read())
	fin.close()
	fin = open('word2id.txt', 'r')
	word2id = json.loads(fin.read())
	fin.close()
	return idf, word2id


if __name__ == '__main__':
	corpus = read_all_doc()
	idf, word2id = cal_idf(corpus)
	save(idf, 'idf.txt')
	save(word2id, 'word2id.txt')



