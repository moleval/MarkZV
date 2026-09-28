#!/usr/bin/env python3
"""Баланс скобок в .lsp (аналог TEST 00 / MK_CHECK, но вне AutoCAD).

Использование:
    python3 tools/check_parens.py MarkZV.lsp
Код возврата 0 — ok, 1 — дисбаланс.
"""
import sys
from pathlib import Path


def scan(text):
    depth = 0
    in_str = False
    in_com = False
    line = 1
    extra_line = None
    i = 0
    n = len(text)
    while i < n:
        c = text[i]
        if c == "\n":
            line += 1
            in_com = False
            i += 1
            continue
        if in_com:
            i += 1
            continue
        if in_str:
            if c == "\\":
                i += 2
                continue
            if c == '"':
                in_str = False
            i += 1
            continue
        if c == ";":
            in_com = True
        elif c == '"':
            in_str = True
        elif c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
            if depth < 0 and extra_line is None:
                extra_line = line
        i += 1
    return depth, in_str, extra_line, line


def main(argv):
    paths = argv[1:] or ["MarkZV.lsp"]
    rc = 0
    for p in paths:
        text = Path(p).read_text(encoding="utf-8")
        depth, in_str, extra_line, lines = scan(text)
        if extra_line is not None:
            print(f"{p}: ERROR лишняя ')' на строке {extra_line}")
            rc = 1
        elif depth != 0:
            print(f"{p}: ERROR дисбаланс depth={depth}")
            rc = 1
        elif in_str:
            print(f"{p}: ERROR не закрыта кавычка")
            rc = 1
        else:
            print(f"{p}: bal ok, lines {lines}")
    return rc


if __name__ == "__main__":
    sys.exit(main(sys.argv))
