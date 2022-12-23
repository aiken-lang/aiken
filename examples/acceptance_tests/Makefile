all:
	@for t in $(shell find . -regex ".*[0-9]\{3\}" -type d | sort); do \
		cargo run --quiet -- check $${t}; \
		echo ""; \
	done
