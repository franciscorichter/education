#!/usr/bin/env python3
import os
import re
import sys
import gzip
import json
from datetime import datetime

import pandas as pd

ROOT = "/Users/pancho/Library/CloudStorage/Dropbox/Pancho/25 - CSG/04 - Education"
XLSX_DIR = os.path.join(ROOT, "01 - Data", "xlsx")
APP_DIR = os.path.dirname(os.path.abspath(__file__))
OUT_DIR = os.path.join(APP_DIR, "data_prepared")
os.makedirs(OUT_DIR, exist_ok=True)

ITEM_COL_RE = re.compile(r"^p\d{1,2}(?:_\d{1,2})?$")

AE_MAP = {"A":1, "B":2, "C":3, "D":4, "E":5,
          "a":1, "b":2, "c":3, "d":4, "e":5}


def coerce_resp(series: pd.Series) -> pd.Series:
    if pd.api.types.is_numeric_dtype(series):
        return pd.to_numeric(series, errors="coerce")
    s = series.astype(str).str.strip()
    s = s.replace(AE_MAP)
    return pd.to_numeric(s, errors="coerce")


def read_excel_sheet(path: str, prefer: str = "BD") -> pd.DataFrame:
    try:
        xl = pd.ExcelFile(path, engine="openpyxl")
        sheets = xl.sheet_names
    except Exception as e:
        print(f"[read_excel_sheet] Failed to open {path}: {e}", file=sys.stderr)
        return None
    # Case-insensitive matching for preferred sheet and fallback 'base'
    sheet = None
    for s in sheets:
        if s.lower() == prefer.lower():
            sheet = s
            break
    if sheet is None:
        for s in sheets:
            if s.lower() == "base":
                sheet = s
                break
    # Heuristic: choose sheet with the most pXX columns if none matched
    if sheet is None and sheets:
        best_s = None
        best_cnt = -1
        for s in sheets:
            try:
                df_tmp = xl.parse(s, nrows=50)
            except Exception as e:
                print(f"[read_excel_sheet] Failed to parse sheet {s} in {path}: {e}", file=sys.stderr)
                continue
            cnt = sum(ITEM_COL_RE.match(str(c)) is not None for c in df_tmp.columns)
            if cnt > best_cnt:
                best_cnt = cnt
                best_s = s
        sheet = best_s if best_s is not None else sheets[0]
    if sheet is None:
        return None
    try:
        df = xl.parse(sheet)
    except Exception as e:
        print(f"[read_excel_sheet] Failed to parse final sheet {sheet} in {path}: {e}", file=sys.stderr)
        return None
    return df


def standardize_keys(df: pd.DataFrame) -> pd.DataFrame:
    if df is None:
        return df
    cols = {c: c for c in df.columns}
    low = {c.lower(): c for c in df.columns}
    if "id_estudiante" in low:
        cols[low["id_estudiante"]] = "ID_ESTUDIANTE"
    if "cod_mod7" in low:
        cols[low["cod_mod7"]] = "cod_mod7"
    if "medida500_l" in low:
        cols[low["medida500_l"]] = "medida500_L"
    if "medida500_m" in low:
        cols[low["medida500_m"]] = "medida500_M"
    return df.rename(columns=cols)


def preprocess_questionnaire(xlsx_path: str) -> dict:
    base = os.path.splitext(os.path.basename(xlsx_path))[0]
    if re.search(r"base_web2", base, flags=re.I):
        return {"skipped": True, "base": base}
    df = read_excel_sheet(xlsx_path, prefer="BD")
    if df is None or df.empty:
        return {"ok": False, "base": base, "reason": "No BD sheet"}
    df = standardize_keys(df)
    item_cols = [c for c in df.columns if ITEM_COL_RE.match(str(c))]
    keep_cols = [c for c in ["ID_ESTUDIANTE", "cod_mod7"] if c in df.columns]
    if len(item_cols) < 2:
        return {"ok": False, "base": base, "reason": "<2 item cols"}
    out = df[keep_cols + item_cols].copy()
    # Coerce responses
    for c in item_cols:
        out[c] = coerce_resp(out[c])
    out_file = os.path.join(OUT_DIR, f"bd_{base}.csv.gz")
    out.to_csv(out_file, index=False, compression="gzip")
    return {
        "ok": True,
        "base": base,
        "rows": len(out),
        "item_cols": len(item_cols),
        "keys": keep_cols,
        "file": out_file,
    }


def preprocess_em() -> dict:
    path = os.path.join(XLSX_DIR, "EM_6P_2024_alumnos_innominados.xlsx")
    df = read_excel_sheet(path, prefer="BD")
    if df is None or df.empty:
        return {"ok": False, "reason": "EM BD/base not found"}
    df = standardize_keys(df)
    keep = [c for c in ["ID_ESTUDIANTE", "cod_mod7", "medida500_L", "medida500_M"] if c in df.columns]
    if not keep:
        return {"ok": False, "reason": "EM missing expected cols"}
    out = df[keep].copy()
    for c in ["medida500_L", "medida500_M"]:
        if c in out.columns:
            out[c] = pd.to_numeric(out[c], errors="coerce")
    out_file = os.path.join(OUT_DIR, "em_alumnos.csv.gz")
    out.to_csv(out_file, index=False, compression="gzip")
    return {"ok": True, "rows": len(out), "cols": keep, "file": out_file}


def main():
    results = {"time": datetime.now().isoformat()}
    # EM first
    results["em"] = preprocess_em()
    # Questionnaires
    qfiles = [
        f for f in os.listdir(XLSX_DIR)
        if f.lower().endswith(".xlsx") and f.lower().startswith("enla") and "~$" not in f
    ]
    qpaths = [os.path.join(XLSX_DIR, f) for f in qfiles]
    qpaths = [p for p in qpaths if not re.search(r"base_web2", os.path.basename(p), flags=re.I)]
    results["questionnaires"] = []
    for p in qpaths:
        res = preprocess_questionnaire(p)
        results["questionnaires"].append(res)
    # Write manifest
    manifest = os.path.join(OUT_DIR, "manifest.json")
    with open(manifest, "w", encoding="utf-8") as f:
        json.dump(results, f, ensure_ascii=False, indent=2)
    # Print summary
    print(json.dumps(results, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    sys.exit(main())
