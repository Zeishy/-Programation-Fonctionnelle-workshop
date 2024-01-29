##
## EPITECH PROJECT, 2024
## workshop-functional-prog/Makefile
## File description:
## Step 3
##

MAIN = Main.hs

BIN = DoOp

all:
	ghc --make $(MAIN) -o $(BIN)

clean:
	rm -f *.o *.hi $(BIN)
