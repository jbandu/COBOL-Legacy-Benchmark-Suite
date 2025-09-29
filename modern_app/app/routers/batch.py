from __future__ import annotations

import json
from datetime import datetime
from pathlib import Path
from typing import Any

from fastapi import APIRouter, BackgroundTasks, Depends, HTTPException, status

from ..models import BatchJob
from ..schemas import BatchJobRead
from ..security import authorize
from ..services import aggregate_portfolio_value, get_portfolio_or_404, managed_session, update_batch_job

router = APIRouter(prefix="/batch", tags=["batch"], dependencies=[Depends(authorize)])

REPORT_DIR = Path("modern_app/outputs")
REPORT_DIR.mkdir(parents=True, exist_ok=True)


def _generate_report(job_id: int, portfolio_id: int) -> None:
    from ..database import session_scope

    with session_scope() as session:
        job = session.get(BatchJob, job_id)
        if job is None:
            return
        update_batch_job(session, job, "IN_PROGRESS")
        portfolio = get_portfolio_or_404(session, portfolio_id)
        total_value = aggregate_portfolio_value(session, portfolio_id)
        report = {
            "portfolio": portfolio.code,
            "generated_at": datetime.utcnow().isoformat(),
            "total_market_value": total_value,
        }
        output_path = REPORT_DIR / f"portfolio_{portfolio.id}_summary.json"
        output_path.write_text(json.dumps(report, indent=2), encoding="utf-8")
        update_batch_job(session, job, "COMPLETED", result_path=str(output_path))


@router.post("/portfolio-report", response_model=BatchJobRead, status_code=status.HTTP_202_ACCEPTED)
def enqueue_portfolio_report(portfolio_id: int, tasks: BackgroundTasks):
    with managed_session() as session:
        portfolio = get_portfolio_or_404(session, portfolio_id)
        job = BatchJob(name=f"Portfolio report {portfolio.code}")
        session.add(job)
        session.commit()
        session.refresh(job)
        tasks.add_task(_generate_report, job.id, portfolio.id)
        return job


@router.get("/{job_id}", response_model=BatchJobRead)
def get_job(job_id: int):
    with managed_session() as session:
        job = session.get(BatchJob, job_id)
        if not job:
            raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Job not found")
        return job
