COMPOSE_BIN=docker compose
BASE_COMPOSE_COMMAND=$(COMPOSE_BIN) --project-name blog-hs
COMPOSE_FILE=deployment/docker-compose.yml
DOCKERFILE_PATH=deployment/Dockerfile
DOCKER_IMAGE_NAME=blog-hs

deploy-dev: $(COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(COMPOSE_FILE) --profile dev up -d

destroy-dev: $(COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(COMPOSE_FILE) --profile dev down

deploy-dev-app: $(COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(COMPOSE_FILE) --profile dev-app up -d

destroy-dev-app: $(COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(COMPOSE_FILE) --profile dev-app down

deploy: $(COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(COMPOSE_FILE) --profile app up -d

destroy: $(COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(COMPOSE_FILE) --profile app down

build-image: $(DOCKERFILE_PATH)
	docker build -t $(DOCKER_IMAGE_NAME) -f $(DOCKERFILE_PATH) .
