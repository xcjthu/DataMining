library('XML')
library('NLP')
library('tm')
library('stringr')
library('wordcloud')
library('RColorBrewer')
library('ggplot2')

dir_path = '../../HW2/nyt_corpus/samples_500/'
filenames = list.files(path = dir_path)

# (1)
news_data = data.frame()
for (i in 1:length(filenames)){
	filename = filenames[i]
	file = xmlParse(file = paste(dir_path, filename, sep = ""))
	metaName = xpathSApply(file, '//meta', xmlGetAttr, 'name')
	metaContent = xpathSApply(file, '//meta', xmlGetAttr, 'content')

	full_text = xpathSApply(file, "//block[@class='full_text']", xmlValue)
	lead_paragraph = xpathSApply(file, "//block[@class='lead_paragraph']", xmlValue)

	classes = xpathSApply(file, '//classifier', xmlValue)
	
	if (length(full_text) == 0){
		next
	}
	news_data[filename, 'full_text'] = full_text[1]
	if (length(lead_paragraph) != 0){
		news_data[filename, 'lead_paragraph'] = lead_paragraph[1]
	}
	else{
		news_data[filename, 'lead_paragraph'] = NA
	}
	for (i in 1:length(metaName)){
		news_data[filename, metaName[i]] = metaContent[i]
	}

	classes = unique(str_extract(classes, '((?<=Top/News/).*?((?=/)))|((?<=Top/Features/).*?((?=/)))'))
	for (c in classes){
		if (!is.na(c)){
			news_data[filename, c] = 1
		}
	}
}


# (2)

pre_process <- function(corpus){
	corpus = tm_map(corpus, stripWhitespace) # 消除空格
	corpus = tm_map(corpus, removePunctuation) # 去除标点符号
	corpus = tm_map(corpus, content_transformer(tolower)) #小写
	corpus = tm_map(corpus, removeWords, stopwords('en')) #停用词
	corpus = tm_map(corpus, removeNumbers)# 数字
	corpus = tm_map(corpus, stemDocument)# 词干化
	return (corpus)
}

corpus = VCorpus(VectorSource(news_data$full_text))
corpus = pre_process(corpus)


# (3)
bag_of_words = DocumentTermMatrix(corpus)

bag_of_words = as.matrix(bag_of_words)
# print(bag_of_words[1:3, 100:120])

# (4)
word_count = sort(colSums(bag_of_words), decreasing = TRUE)
most_100 = word_count[1:100]
wordcloud(names(most_100), most_100, colors = brewer.pal(8, "Dark2"))


png(filename = "p.png", width = 1200,height = 900)
leng = sapply(names(word_count), nchar)
leng_frame = data.frame(lll = leng)
ggplot(leng_frame, aes(lll)) + geom_bar()

dev.off()

# (5)
article_count = sort(rowSums(bag_of_words), decreasing = TRUE)


