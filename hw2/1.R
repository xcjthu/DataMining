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
all_class = c()
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
			all_class[length(all_class) + 1] = c
		}
	}
}
print(colnames(news_data))
all_class = levels(factor(all_class))
print(all_class)
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

# (5)
png(filename = "p.png", width = 1200,height = 900)
leng = sapply(names(word_count), nchar)
leng_frame = data.frame(lll = leng)
ggplot(leng_frame, aes(lll)) + geom_bar()

dev.off()

#(6)
article_count = sapply(corpus, function(x) return( length( str_split(x$content, pattern = ' ')[[1]] )) )
# 等深分箱、等宽分箱

width = cut(article_count, 10)

cut_depth = function(x, n)
{
  cut(rank(x)/length(x)*n,breaks = 0:n)
}
depth = cut_depth(article_count, 10)

data_length = data.frame(depth_cut = depth, width_cut = width)
png(filename = "depth.png", width = 1000,height = 900)
ggplot(data_length, aes(depth_cut)) + geom_bar() + xlab("depth") 
dev.off()

png(filename = "width.png", width = 1000,height = 900)
ggplot(data_length, aes(width_cut)) + geom_bar() + xlab("width") 
dev.off()

# (7)
class_count = data.frame(name = all_class)
for (c in all_class){
	class_count[class_count$name == c, 'count'] = length(news_data[, c]) - sum(is.na(news_data[, c]))
}
png(filename = "class_count.png", width = 1000,height = 900)
ggplot(class_count, aes(x=name, y=count)) + geom_bar(stat = "identity")
dev.off()


# (8)
for (i in 1:9)
{
  news_data$publication_month[news_data$publication_month == as.character(i)] = paste("0", as.character(i), sep = "")
}
png(filename = "month_count.png", width = 1000,height = 900)
ggplot(news_data, aes(x=publication_month)) + geom_bar()
dev.off()


