# Portfolio Modernization API

This directory contains a FastAPI-based reimplementation of the COBOL Investment Portfolio Management benchmark. It condenses the online inquiry, batch reporting, and portfolio maintenance flows documented in the legacy system into a modern service.

## Features

- **Portfolio CRUD** – manage master portfolio records with validation and status management.
- **Positions & transactions** – capture holdings and trade activity tied to each portfolio.
- **Inquiry endpoint** – return aggregated market value and recent activity similar to the CICS inquiry transactions (`INQONLN`/`INQPORT`/`INQHIST`).
- **Batch reporting** – enqueue asynchronous portfolio summary jobs mirroring the nightly reporting JCL.
- **Error capture** – persist HTTP errors to an `ErrorLog` table analogous to `ERRPROC`/`ERRHNDL`.

## Getting started

```bash
cd modern_app
python -m venv .venv
source .venv/bin/activate
pip install -e .[dev]
uvicorn modern_app.app.main:app --reload
```

Interact with the API via `http://localhost:8000/docs` using the `X-Access-Token` header (default value `secret-token`).

## Running tests

```bash
cd modern_app
pytest
```
