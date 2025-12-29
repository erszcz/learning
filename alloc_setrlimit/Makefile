alloc_setrlimit.linux: alloc_setrlimit.c
	cc $< -o $@

alloc_setrlimit.macos: alloc_setrlimit.c
	cc $< -o $@

# See https://github.com/erszcz/make-linux-shell
.PHONY: linux-shell
linux-shell:
	docker compose up -d
	docker exec -it $$COMPOSE_PROJECT_NAME-linux_shell-1 bash
