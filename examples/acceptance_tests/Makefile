all:
	@for t in $(shell find . -regex ".*[0-9]\{3\}" -type d | sort); do \
		aiken check -d $${t}; \
		echo ""; \
	done
