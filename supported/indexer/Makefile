all: medium.index medium.fsim

medium.fsim: medium.crawl
	efind_similar_files medium

medium.index: medium.in
	eindex -crawl medium
	eindex -index medium

clean:
	rm -rf medium.crawl medium.files medium.index medium.fsim medium.hashes


 





