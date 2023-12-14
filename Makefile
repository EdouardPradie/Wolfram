##
## EPITECH PROJECT, 2021
## Makefile
## File description:
## Wolfram
##

OBJ		=	$(shell stack path --local-install-root)

NAME	=	wolfram


all:	$(NAME)

$(NAME):
	stack build
	cp $(OBJ)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)
	stack purge

re:	fclean all

.PHONY: re fclean clean all
