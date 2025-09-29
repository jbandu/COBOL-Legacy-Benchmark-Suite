from __future__ import annotations

from datetime import datetime

from fastapi.testclient import TestClient

from modern_app.app.config import Settings
from modern_app.app.database import init_db
from modern_app.app.main import app

client = TestClient(app)


def _headers() -> dict[str, str]:
    return {"X-Access-Token": Settings().access_token}


def setup_module(module):  # noqa: D401
    """Ensure database is initialised for tests."""

    init_db()


def test_portfolio_lifecycle():
    response = client.post(
        "/portfolios/",
        json={"code": "PORT1", "name": "Alpha", "owner": "Trader", "risk_rating": "LOW"},
        headers=_headers(),
    )
    assert response.status_code == 201
    portfolio = response.json()

    response = client.get("/portfolios/", headers=_headers())
    assert response.status_code == 200
    assert len(response.json()) == 1

    response = client.patch(
        f"/portfolios/{portfolio['id']}",
        json={"status": "SUSPENDED"},
        headers=_headers(),
    )
    assert response.status_code == 200
    assert response.json()["status"] == "SUSPENDED"


def test_position_and_transaction_flow():
    portfolio_id = client.post(
        "/portfolios/",
        json={"code": "PORT2", "name": "Beta", "owner": "Ops", "risk_rating": "MEDIUM"},
        headers=_headers(),
    ).json()["id"]

    response = client.post(
        f"/portfolios/{portfolio_id}/positions/",
        json={"symbol": "IBM", "quantity": 10, "market_value": 1350.0},
        headers=_headers(),
    )
    assert response.status_code == 201

    response = client.post(
        f"/portfolios/{portfolio_id}/transactions/",
        json={
            "trade_date": datetime.utcnow().isoformat(),
            "symbol": "IBM",
            "quantity": 10,
            "price": 135.0,
            "type": "BUY",
        },
        headers=_headers(),
    )
    assert response.status_code == 201

    response = client.get(f"/inquiries/portfolios/{portfolio_id}", headers=_headers())
    assert response.status_code == 200
    body = response.json()
    assert body["total_market_value"] == 1350.0
    assert len(body["recent_transactions"]) == 1


def test_batch_report_generation():
    portfolio_id = client.post(
        "/portfolios/",
        json={"code": "PORT3", "name": "Gamma", "owner": "Ops", "risk_rating": "HIGH"},
        headers=_headers(),
    ).json()["id"]

    response = client.post(
        "/batch/portfolio-report",
        params={"portfolio_id": portfolio_id},
        headers=_headers(),
    )
    assert response.status_code == 202
    job = response.json()

    job_status = client.get(f"/batch/{job['id']}", headers=_headers()).json()
    assert job_status["status"] == "COMPLETED"
    assert job_status["result_path"].endswith(".json")
