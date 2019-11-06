bench:
	docker run -it --rm \
		-v $(CURDIR):$(CURDIR) \
		--workdir $(CURDIR) \
		--net host \
		erlang:21 \
		tools/run-benchmark.sh

results: tests/current/results

tests/%/results: tests/%
	docker run --rm -p 8080:8080 \
		-v $(CURDIR)/tools:/tools:ro \
		-v $(abspath $(shell readlink $<)):/src:ro \
		python:2 \
		/tools/run-results-browser.sh
