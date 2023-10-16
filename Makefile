COMPOSE_BIN=docker compose
BASE_COMPOSE_COMMAND=$(COMPOSE_BIN) --project-name blog-hs
DEV_COMPOSE_FILE=deployment/dev.docker-compose.yml
DOCKERFILE_PATH=deployment/Dockerfile
DOCKER_IMAGE_NAME=blog-hs

deploy-dev: $(DEV_COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(DEV_COMPOSE_FILE) up -d

destroy-dev: $(DEV_COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(DEV_COMPOSE_FILE) down

build-image: $(DOCKERFILE_PATH)
	docker build -t $(DOCKER_IMAGE_NAME) -f $(DOCKERFILE_PATH) .
