from __future__ import annotations

from contextlib import asynccontextmanager
from typing import Any

from fastapi import FastAPI, HTTPException, Request
from fastapi.responses import JSONResponse

from .database import init_db, session_scope
from .models import ErrorLog
from .routers import batch, inquiries, portfolios, positions, transactions


@asynccontextmanager
def lifespan(_: FastAPI):
    init_db()
    yield


app = FastAPI(title="Portfolio Modernization API", lifespan=lifespan)


@app.exception_handler(HTTPException)
async def http_exception_handler(request: Request, exc: HTTPException) -> JSONResponse:
    with session_scope() as session:
        error = ErrorLog(context=request.url.path, message=exc.detail if isinstance(exc.detail, str) else str(exc.detail), severity="ERROR")
        session.add(error)
        session.commit()
    return JSONResponse(status_code=exc.status_code, content={"detail": exc.detail})


@app.get("/health")
def health_check() -> dict[str, str]:
    return {"status": "ok"}


app.include_router(portfolios.router)
app.include_router(positions.router)
app.include_router(transactions.router)
app.include_router(inquiries.router)
app.include_router(batch.router)
