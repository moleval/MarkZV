#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Разбор XLS(X)-базы профильной системы -> LISP-таблица «артикул -> габарит».

Работает с любой системой, не только СИАЛ: задайте префиксы артикулов и имя
системы.

Использование:
  1) посмотреть структуру книги:
       python3 tools/sial_parse.py "База СИАЛ.xlsx"
  2) сгенерировать блок регистрации базы:
       python3 tools/sial_parse.py "База АЛЮТЕХ.xlsx" --emit \
           --system АЛЮТЕХ --prefix ALT --article-col 1 --size-col 5 \
           >> MarkZV-bases.lsp

Результат --emit — готовый файл базы для MarkZV:
  (mk:register-base "АЛЮТЕХ" '(("ALT-W72-01" . 72.0) ...))
  (foreach p '("ALT") (if (not (member p *mk:article-prefixes*)) ...))

Зависимость: pip install openpyxl
"""
import argparse
import re
import sys

try:
    import openpyxl
except ImportError:
    sys.exit("Нужен openpyxl:  pip install openpyxl")

DEFAULT_PREFIXES = ["КП", "KP"]
ART_RE = None


def build_re(prefixes):
    alt = "|".join(re.escape(p) for p in prefixes)
    # префикс + до двух букв исполнения + пробелы + цифры (+ исполнение через дефис)
    return re.compile(r"^\s*(?:%s)[A-Za-zА-Яа-я-]{0,3}\s*\d{2,6}(-\d+)?\s*$" % alt,
                      re.IGNORECASE)


def norm_article(v):
    if v is None:
        return None
    s = str(v).strip()
    if not ART_RE.match(s):
        return None
    return s.replace(" ", "").upper()


def to_float(v):
    if v is None:
        return None
    if isinstance(v, (int, float)):
        return float(v)
    s = str(v).strip().replace(",", ".")
    m = re.search(r"-?\d+(\.\d+)?", s)
    return float(m.group(0)) if m else None


def inspect(wb):
    for ws in wb.worksheets:
        print(f"=== ЛИСТ: {ws.title}  ({ws.max_row} строк x {ws.max_column} колонок)")
        for r, row in enumerate(ws.iter_rows(min_row=1, max_row=min(8, ws.max_row),
                                             values_only=True), 1):
            cells = ["" if c is None else str(c)[:22] for c in row[:14]]
            print(f"  {r:>3}: " + " | ".join(cells))
        print()


def collect(wb, art_col, size_col):
    data = {}
    for ws in wb.worksheets:
        for row in ws.iter_rows(values_only=True):
            if art_col and size_col:
                art = norm_article(row[art_col - 1]) if len(row) >= art_col else None
                size = to_float(row[size_col - 1]) if len(row) >= size_col else None
            else:  # автопоиск: первая ячейка-артикул и первое разумное число
                art = next((norm_article(c) for c in row if norm_article(c)), None)
                nums = [to_float(c) for c in row]
                nums = [n for n in nums if n and 10.0 <= n <= 400.0]
                size = nums[0] if nums else None
            if art and size and art not in data:
                data[art] = (size, ws.title)
    return data


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("xlsx")
    ap.add_argument("--emit", action="store_true")
    ap.add_argument("--article-col", type=int, default=0)
    ap.add_argument("--size-col", type=int, default=0)
    ap.add_argument("--system", default="СИАЛ", help="имя базы в MarkZV")
    ap.add_argument("--prefix", action="append", default=[],
                    help="префикс артикулов (можно повторять); по умолчанию КП/KP")
    a = ap.parse_args()

    global ART_RE
    ART_RE = build_re(a.prefix or DEFAULT_PREFIXES)

    wb = openpyxl.load_workbook(a.xlsx, data_only=True, read_only=True)
    if not a.emit:
        inspect(wb)
        return

    data = collect(wb, a.article_col, a.size_col)
    if not data:
        sys.exit("Артикулы не распознаны — задайте --article-col/--size-col явно.")
    print(";;; Сгенерировано tools/sial_parse.py из " + a.xlsx)
    print(f";;; База «{a.system}»: артикул -> габарит сечения, мм")
    print(f'(mk:register-base "{a.system}"')
    print("  '(")
    for art, (size, sheet) in sorted(data.items(), key=lambda kv: kv[1][0]):
        print(f'    ("{art}" . {size:.1f})')
    print("  ))")
    prefixes = a.prefix or DEFAULT_PREFIXES
    print("")
    print(";;; префиксы артикулов этой системы")
    print("(foreach p '(" + " ".join(f'"{p}"' for p in prefixes) + ")")
    print("  (if (not (member p *mk:article-prefixes*))")
    print("    (setq *mk:article-prefixes* (cons p *mk:article-prefixes*))))")


if __name__ == "__main__":
    main()
