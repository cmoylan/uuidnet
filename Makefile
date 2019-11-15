.PHONY: up
up:
	docker-compose up -d

.PHONY: down
down:
	docker-compose down

.PHONY: ps
ps:
	docker-compose ps

.PHONY: db
db:
	#psql -U postgres -h localhost -d uuidnet_dev
	psql -U postgres -h 0.0.0.0 -d uuidnet_dev
