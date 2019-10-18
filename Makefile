.PHONY: help install-dev test-unit test

.DEFAULT_GOAL := help
SHELL = bash


help::
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

dev: ## runs the project and restarts it when changes are detected in the source code
	spago run --watch

install-dev: ## install project with all its dependencies, including development ones
	spago install

test-unit: ## run unit tests
	spago test

test: test-unit ## run all tests
