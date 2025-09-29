from __future__ import annotations

from fastapi import APIRouter, Depends

from ..schemas import PortfolioRead, TransactionRead
from ..security import authorize
from ..services import aggregate_portfolio_value, get_portfolio_or_404, get_recent_transactions, managed_session

router = APIRouter(prefix="/inquiries", tags=["inquiries"], dependencies=[Depends(authorize)])


@router.get("/portfolios/{portfolio_id}")
def portfolio_overview(portfolio_id: int) -> dict[str, object]:
    with managed_session() as session:
        portfolio = get_portfolio_or_404(session, portfolio_id)
        value = aggregate_portfolio_value(session, portfolio_id)
        transactions = get_recent_transactions(session, portfolio_id, limit=5)
        return {
            "portfolio": PortfolioRead.model_validate(portfolio).model_dump(),
            "total_market_value": value,
            "recent_transactions": [TransactionRead.model_validate(item).model_dump() for item in transactions],
        }
