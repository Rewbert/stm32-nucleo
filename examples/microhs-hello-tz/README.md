# MicroHs Hello TZ

Small MicroHs-on-TrustZone example.

## Purpose

This builds one MicroHs runtime for secure world and one for non-secure world. Each side runs a tiny Haskell program generated into C by MicroHs.

## TrustZone Split

The secure side configures the board, toggles the red LED, prints over UART, and then hands execution to non-secure world. The non-secure side toggles the green LED and prints its own message.
