from __future__ import annotations

from contextlib import contextmanager
from datetime import datetime

from fastapi import HTTPException, status
from sqlmodel import Session, select

from .database import session_scope
from .models import BatchJob, ErrorLog, Portfolio, Position, Transaction


@contextmanager
def managed_session():
    with session_scope() as session:
        yield session


def record_error(session: Session, context: str, message: str, severity: str = "ERROR") -> ErrorLog:
    entry = ErrorLog(context=context, message=message, severity=severity)
    session.add(entry)
    session.commit()
    session.refresh(entry)
    return entry


def get_portfolio_or_404(session: Session, portfolio_id: int) -> Portfolio:
    portfolio = session.get(Portfolio, portfolio_id)
    if not portfolio:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Portfolio not found")
    return portfolio


def update_batch_job(session: Session, job: BatchJob, status_text: str, *, details: str | None = None, result_path: str | None = None) -> BatchJob:
    job.status = status_text
    job.details = details
    if status_text == "IN_PROGRESS":
        job.started_at = datetime.utcnow()
    if status_text in {"COMPLETED", "FAILED"}:
        job.completed_at = datetime.utcnow()
    if result_path:
        job.result_path = result_path
    session.add(job)
    session.commit()
    session.refresh(job)
    return job


def aggregate_portfolio_value(session: Session, portfolio_id: int) -> float:
    statement = select(Position).where(Position.portfolio_id == portfolio_id)
    return sum(row.market_value for row in session.exec(statement))


def get_recent_transactions(session: Session, portfolio_id: int, limit: int = 10) -> list[Transaction]:
    statement = (
        select(Transaction)
        .where(Transaction.portfolio_id == portfolio_id)
        .order_by(Transaction.trade_date.desc())
        .limit(limit)
    )
    return list(session.exec(statement))
