COMPOSE_BIN=docker compose
BASE_COMPOSE_COMMAND=$(COMPOSE_BIN) --project-name blog-hs
DEV_COMPOSE_FILE=deployment/dev.docker-compose.yml

deploy-dev: $(DEV_COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(DEV_COMPOSE_FILE) up -d

destroy-dev: $(DEV_COMPOSE_FILE)
	$(BASE_COMPOSE_COMMAND) -f $(DEV_COMPOSE_FILE) down
