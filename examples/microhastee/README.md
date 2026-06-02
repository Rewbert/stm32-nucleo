# MicroHasTEE

Larger research prototype: MicroHs programs run in both TrustZone worlds, with a secure callable path from non-secure Haskell into secure Haskell.

## Purpose

This example explores how a Haskell-level API can describe secure and non-secure effects while compiling down to two firmware images.

## TrustZone Split

The secure side owns board setup, secure LEDs, and the non-secure callable entry point. The non-secure side owns UART use and calls into secure world through the generated secure gateway path.

## Notes

The `haskell/` directory contains the Haskell source package used to generate `S/gen2.c` and `NS/gen2.c`. The external MicroHs checkout still lives at the repository root as `MicroHs/`.

This is paper-adjacent and less tutorial-shaped than the small examples.
