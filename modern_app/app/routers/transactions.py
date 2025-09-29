from __future__ import annotations

from fastapi import APIRouter, Depends, HTTPException, status
from sqlmodel import select

from ..models import Transaction
from ..schemas import TransactionCreate, TransactionRead
from ..security import authorize
from ..services import get_portfolio_or_404, managed_session

router = APIRouter(prefix="/portfolios/{portfolio_id}/transactions", tags=["transactions"], dependencies=[Depends(authorize)])


@router.post("/", response_model=TransactionRead, status_code=status.HTTP_201_CREATED)
def add_transaction(portfolio_id: int, payload: TransactionCreate):
    with managed_session() as session:
        get_portfolio_or_404(session, portfolio_id)
        transaction = Transaction(portfolio_id=portfolio_id, **payload.model_dump())
        session.add(transaction)
        session.commit()
        session.refresh(transaction)
        return transaction


@router.get("/", response_model=list[TransactionRead])
def list_transactions(portfolio_id: int):
    with managed_session() as session:
        get_portfolio_or_404(session, portfolio_id)
        statement = select(Transaction).where(Transaction.portfolio_id == portfolio_id).order_by(Transaction.trade_date)
        return list(session.exec(statement))


@router.delete("/{transaction_id}", status_code=status.HTTP_204_NO_CONTENT)
def delete_transaction(portfolio_id: int, transaction_id: int):
    with managed_session() as session:
        get_portfolio_or_404(session, portfolio_id)
        transaction = session.get(Transaction, transaction_id)
        if not transaction or transaction.portfolio_id != portfolio_id:
            raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Transaction not found")
        session.delete(transaction)
        session.commit()
        return None
