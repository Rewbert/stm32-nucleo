
This target builds the MicroHaskell runtime, so that it can be flashed to the board. You need to replace `gen.c` with your own `gen.c` containing your own program. The one included here will flash the red LED and print to the terminal.

The runtime included here is taken from my fork, that I use for my work in partial evaluation.