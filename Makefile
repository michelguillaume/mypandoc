##
## EPITECH PROJECT, 2025
## Makefile
## File description:
## it makes files
##

.PHONY: all build install clean fclean re test

all: install

build:
	stack build

install: build
	stack install --local-bin-path .

clean:
	stack clean

fclean: clean
	rm -f mypandoc

re: fclean all

test:
	stack test
