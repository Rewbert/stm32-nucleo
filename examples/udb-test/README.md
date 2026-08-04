# udb-test

A TrustZone example that runs the `udb/` micro key-value database on real
on-chip flash. The secure and non-secure worlds each run their **own** database,
stored in the last two erase pages of their **own** flash bank.

## What it does

Both `S/main.c` and `NS/main.c` run the same stub smoke test: build a
`udb_driver_t` for the current board, `udb_mount` (or `udb_format` on first
boot), then a `udb_put`/`udb_get` round-trip, and print `secure udb: OK/FAIL` /
`nonsecure udb: OK/FAIL` over the console (LPUART1, 115200 8N1).

The secure side configures the clocks and console, prints its result, releases
the console to the non-secure world via TZSC, and returns; the bootloader then
launches the non-secure side, which prints its own result.

## Files

- `shared/flash-drv.{c,h}` — generic on-chip flash backend for udb. `read` is a
  copy from memory-mapped flash, `write` programs whole program-units, `reset`
  page-erases a segment. STM32L5 and STM32U5 share the program/erase register
  layout, so one implementation serves both via a small params struct.
- `shared/stm32l5-drv.c`, `shared/stm32u5-drv.c` — per-MCU geometry and
  `udb_drv_create()`. Compiled twice (secure/non-secure); `-DSECURE` selects the
  secure flash alias + `SECxx` registers, otherwise the non-secure alias +
  `NSxx` registers.
- `shared/udb-drv.h` — declares `udb_drv_create()`.
- `S/main.c`, `NS/main.c` — the stub applications.

## Flash placement (important)

The two segments live in the **last `2 × erase_page`** of each world's flash:

| MCU | prog_size | erase page | secure segments      | non-secure segments  |
|-----|-----------|-----------|----------------------|----------------------|
| L5  | 8 bytes   | 2 KB      | top of 0x0C00_0000   | top of 0x0808_0000   |
| U5  | 16 bytes  | 8 KB      | top of 0x0C20_0000   | top of 0x0840_0000   |

The `UDB_FLASH_END` `#define`s in `stm32l5-drv.c` / `stm32u5-drv.c` must match
**2 erase pages reserved at the end of flash** in the corresponding linker
script (`firmware/bootloader/<mcu>/{S,NS}/ls-*.ld`). On the secure side this
must be reconciled with the existing `PERSIST` and `FLASH_NSC` regions already
at the top of secure flash. This example does **not** edit the linker scripts.

Also note: erasing/programming a flash bank while executing from that same bank
stalls the core until the operation finishes. That is fine for this one-shot
test but worth keeping in mind.

## Build

From the repo root (the `all` target must be named — the bare default goal
resolves to the shared driver archive):

```sh
make -f examples/udb-test/Makefile all                  # STM32L5 (default)
make clean
make -f examples/udb-test/Makefile all BOARD=stm32u5    # STM32U5
```

Each build produces `secure.elf` + `nonsecure.elf`. Flash the pair with the
top-level `make flash_tz` target.
