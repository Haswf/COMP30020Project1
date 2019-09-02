import re

with open("./log.txt", 'r') as f:
	content = f.read()
	quality = re.findall("\d+\.\d*", content)
	total = 0

	for i in quality:
		total += float(i)
	print(total/len(quality))