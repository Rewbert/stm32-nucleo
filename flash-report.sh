#!/usr/bin/env bash
# Per-component flash breakdown for secure.elf/nonsecure.elf (examples/microhastee).
# Feeds paper/main.tex table:flash-requirement (around line 1065).
#
# Method: pull the exact object/archive list and flags make would use for the
# real link (via `make print-VAR`, so it can't drift from the Makefile), then
# redo that link into a scratch dir with -Wl,-Map=... added. No recompilation.
# The map file records which input object contributed each linked section, so
# flash bytes can be attributed back to a component -- including code pulled
# in from libc/libgcc, which a per-.o `size` summary would miss entirely.
#
# Usage: ./flash-report.sh [BOARD]   (default: stm32u5)

set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"

BOARD="${1:-stm32u5}"
APP_MK="examples/microhastee/Makefile"
WORKDIR="$(mktemp -d)"
trap 'rm -rf "$WORKDIR"' EXIT

echo "== building secure.elf / nonsecure.elf (BOARD=$BOARD) ==" >&2
make -f "$APP_MK" BOARD="$BOARD" secure.elf nonsecure.elf >&2

mkvar() { make -f "$APP_MK" -f <(printf 'print-%%:\n\t@echo "$($*)"\n') BOARD="$BOARD" "print-$1"; }

CC=$(mkvar CC)
TZ_CFLAGS=$(mkvar TZ_CFLAGS)
SECURE_CPPFLAGS=$(mkvar SECURE_CPPFLAGS)
NONSECURE_CPPFLAGS=$(mkvar NONSECURE_CPPFLAGS)
SECURE_LDFLAGS=$(mkvar SECURE_LDFLAGS)
NONSECURE_LDFLAGS=$(mkvar NONSECURE_LDFLAGS)
SECURE_LIB=$(mkvar SECURE_LIB)
SECURE_BOOT_O=$(mkvar SECURE_BOOT_O)
NONSECURE_BOOT_O=$(mkvar NONSECURE_BOOT_O)
BOARD_O=$(mkvar BOARD_O)
BOARD_NS_O=$(mkvar BOARD_NS_O)
SECURE_O=$(mkvar SECURE_O)
NONSECURE_O=$(mkvar NONSECURE_O)
TZ_LIB_DRV_SECURE_A=$(mkvar TZ_LIB_DRV_SECURE_A)
TZ_LIB_DRV_NONSECURE_A=$(mkvar TZ_LIB_DRV_NONSECURE_A)
NOR_UDB_A=$(mkvar NOR_UDB_A)

echo "== re-linking with -Wl,-Map (scratch copies, real elf files untouched) ==" >&2

$CC $TZ_CFLAGS $SECURE_CPPFLAGS $SECURE_LDFLAGS \
  -o "$WORKDIR/secure.elf" \
  $SECURE_BOOT_O $BOARD_O $SECURE_O $TZ_LIB_DRV_SECURE_A $NOR_UDB_A \
  -Wl,--cmse-implib,--out-implib="$WORKDIR/secure_cmse_import.lib" \
  -Wl,-Map="$WORKDIR/secure.map"

$CC $TZ_CFLAGS $NONSECURE_CPPFLAGS $NONSECURE_LDFLAGS \
  -o "$WORKDIR/nonsecure.elf" \
  $NONSECURE_BOOT_O $BOARD_NS_O $NONSECURE_O $TZ_LIB_DRV_NONSECURE_A $NOR_UDB_A "$SECURE_LIB" \
  -Wl,-Map="$WORKDIR/nonsecure.map"

echo "== sanity check: scratch relink must match the real build exactly ==" >&2
arm-none-eabi-size secure.elf nonsecure.elf "$WORKDIR/secure.elf" "$WORKDIR/nonsecure.elf"

cat > "$WORKDIR/mapsize.py" << 'PYEOF'
import re, sys, collections

SEC_ADDR_SIZE_OBJ = re.compile(r'^ \.(\S+)\s+0x([0-9a-fA-F]+)\s+0x([0-9a-fA-F]+)\s+(\S.*)$')
SEC_ONLY = re.compile(r'^ \.(\S+)\s*$')
ADDR_SIZE_OBJ_ONLY = re.compile(r'^\s+0x([0-9a-fA-F]+)\s+0x([0-9a-fA-F]+)\s+(\S.*)$')

def parse(mapfile):
    entries = []
    lines = open(mapfile).readlines()
    n, i = len(lines), 0
    while i < n:
        line = lines[i].rstrip('\n')
        m = SEC_ADDR_SIZE_OBJ.match(line)
        if m:
            secname, _addr, sizehex, obj = m.groups()
            entries.append((secname, int(sizehex, 16), obj.strip()))
            i += 1
            continue
        m2 = SEC_ONLY.match(line)
        if m2 and i + 1 < n:
            m3 = ADDR_SIZE_OBJ_ONLY.match(lines[i + 1].rstrip('\n'))
            if m3:
                secname = m2.group(1)
                _addr, sizehex, obj = m3.groups()
                entries.append((secname, int(sizehex, 16), obj.strip()))
                i += 2
                continue
        i += 1
    return entries

def categorize(obj):
    if 'MHS_DIR' in obj or 'runtime/eval.o' in obj:
        return 'MicroHs runtime'
    if 'tz-lib-drivers' in obj or 'firmware/drivers/src/drivers' in obj:
        return 'Peripheral drivers'
    if 'udb.a' in obj or re.search(r'(^|/)udb\.o', obj):
        return 'udb (flash KV store)'
    if 'firmware/boards' in obj:
        return 'Board glue'
    if 'firmware/bootloader' in obj:
        return 'Bootloader/TZ init'
    if 'examples/microhastee' in obj:
        return 'Example program'
    if 'arm-gnu-toolchain' in obj or 'arm-none-eabi' in obj:
        return 'C library (libc/libgcc)'
    return 'OTHER:' + obj

def flash_bucket(secname):
    base = secname.split('.')[0]
    if base in ('isr_vector', 'gnu', 'nsc_veneers'):
        return 'tzboundary'
    if base in ('text', 'rodata'):
        return 'code'
    if base == 'data':
        return 'data'
    if base == 'ARM' and (secname.startswith('ARM.extab') or secname.startswith('ARM.exidx')):
        return 'code'
    return None  # debug_*, comment, ARM.attributes: not SHF_ALLOC, never reaches flash

def main():
    mapfile, label = sys.argv[1], sys.argv[2]
    totals = collections.defaultdict(int)
    for secname, size, obj in parse(mapfile):
        b = flash_bucket(secname)
        if b is None:
            continue
        cat = 'Bootloader/TZ init' if b == 'tzboundary' else categorize(obj)
        totals[cat] += size

    grand = sum(totals.values())
    print(f"=== {label} ===")
    for cat, size in sorted(totals.items(), key=lambda x: -x[1]):
        print(f"  {cat:28s} {size:7d} B  {size/1024:7.2f} KiB")
    print(f"  {'sum of attributed rows':28s} {grand:7d} B  {grand/1024:7.2f} KiB")
    print()

if __name__ == '__main__':
    main()
PYEOF

python3 "$WORKDIR/mapsize.py" "$WORKDIR/secure.map" "SECURE"
python3 "$WORKDIR/mapsize.py" "$WORKDIR/nonsecure.map" "NON-SECURE"

echo "== ground truth (flash = text+data, excludes .bss which is SRAM-only) ==" >&2
arm-none-eabi-size secure.elf nonsecure.elf
