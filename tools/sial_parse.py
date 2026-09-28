#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Разбор «База СИАЛ.xlsx» -> таблица артикул → габарит сечения → маркер м/б.

Использование:
  1) посмотреть структуру книги:
       python3 tools/sial_parse.py "База СИАЛ.xlsx"
  2) сгенерировать LISP-таблицу для MarkZV.lsp (колонки берутся по заголовкам
     или задаются вручную, нумерация с 1):
       python3 tools/sial_parse.py "База СИАЛ.xlsx" --emit \
           --article-col 1 --size-col 5 > sial_table.lsp

Результат --emit — готовый блок для *mk:article-size*:
  (setq *mk:article-size* '(("КП45551" . "м") ("КП45364" . "б")))
плюс справочная таблица «артикул -> габарит».

Зависимость: pip install openpyxl
"""
import argparse
import re
import sys

try:
    import openpyxl
except ImportError:
    sys.exit("Нужен openpyxl:  pip install openpyxl")

ART_RE = re.compile(r"^\s*(КП|KP)\s*\d{4,6}", re.IGNORECASE)


def norm_article(v):
    if v is None:
        return None
    s = str(v).strip().replace(" ", "")
    return s.upper() if ART_RE.match(s) else None


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
    a = ap.parse_args()

    wb = openpyxl.load_workbook(a.xlsx, data_only=True, read_only=True)
    if not a.emit:
        inspect(wb)
        return

    data = collect(wb, a.article_col, a.size_col)
    if not data:
        sys.exit("Артикулы не распознаны — задайте --article-col/--size-col явно.")
    print(";;; Сгенерировано tools/sial_parse.py из " + a.xlsx)
    print(";;; артикул -> габарит сечения, мм")
    for art, (size, sheet) in sorted(data.items(), key=lambda kv: kv[1][0]):
        print(f";;;   {art:<12} {size:>7.1f}   [{sheet}]")
    small = min(data.items(), key=lambda kv: kv[1][0])[0]
    big = max(data.items(), key=lambda kv: kv[1][0])[0]
    print("\n;;; Пример ручной таблицы м/б (крайние по габариту):")
    print(f"(setq *mk:article-size* '((\"{small}\" . \"м\") (\"{big}\" . \"б\")))")


if __name__ == "__main__":
    main()
