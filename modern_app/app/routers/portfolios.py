from __future__ import annotations

from fastapi import APIRouter, Depends, HTTPException, status
from sqlmodel import select

from ..models import Portfolio
from ..schemas import PortfolioCreate, PortfolioRead, PortfolioUpdate
from ..security import authorize
from ..services import managed_session

router = APIRouter(prefix="/portfolios", tags=["portfolios"], dependencies=[Depends(authorize)])


@router.post("/", response_model=PortfolioRead, status_code=status.HTTP_201_CREATED)
def create_portfolio(payload: PortfolioCreate):
    with managed_session() as session:
        if session.exec(select(Portfolio).where(Portfolio.code == payload.code)).first():
            raise HTTPException(status_code=status.HTTP_409_CONFLICT, detail="Portfolio code already exists")
        portfolio = Portfolio(**payload.model_dump())
        session.add(portfolio)
        session.commit()
        session.refresh(portfolio)
        return portfolio


@router.get("/", response_model=list[PortfolioRead])
def list_portfolios():
    with managed_session() as session:
        return list(session.exec(select(Portfolio)))


@router.get("/{portfolio_id}", response_model=PortfolioRead)
def get_portfolio(portfolio_id: int):
    with managed_session() as session:
        portfolio = session.get(Portfolio, portfolio_id)
        if not portfolio:
            raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Portfolio not found")
        return portfolio


@router.patch("/{portfolio_id}", response_model=PortfolioRead)
def update_portfolio(portfolio_id: int, payload: PortfolioUpdate):
    with managed_session() as session:
        portfolio = session.get(Portfolio, portfolio_id)
        if not portfolio:
            raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Portfolio not found")
        update_data = payload.model_dump(exclude_unset=True)
        for key, value in update_data.items():
            setattr(portfolio, key, value)
        session.add(portfolio)
        session.commit()
        session.refresh(portfolio)
        return portfolio


@router.delete("/{portfolio_id}", status_code=status.HTTP_204_NO_CONTENT)
def delete_portfolio(portfolio_id: int):
    with managed_session() as session:
        portfolio = session.get(Portfolio, portfolio_id)
        if not portfolio:
            raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Portfolio not found")
        session.delete(portfolio)
        session.commit()
        return None
