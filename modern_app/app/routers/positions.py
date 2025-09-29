from __future__ import annotations

from fastapi import APIRouter, Depends, HTTPException, status
from sqlmodel import select

from ..models import Position
from ..schemas import PositionCreate, PositionRead
from ..security import authorize
from ..services import get_portfolio_or_404, managed_session

router = APIRouter(prefix="/portfolios/{portfolio_id}/positions", tags=["positions"], dependencies=[Depends(authorize)])


@router.post("/", response_model=PositionRead, status_code=status.HTTP_201_CREATED)
def add_position(portfolio_id: int, payload: PositionCreate):
    with managed_session() as session:
        get_portfolio_or_404(session, portfolio_id)
        position = Position(portfolio_id=portfolio_id, **payload.model_dump())
        session.add(position)
        session.commit()
        session.refresh(position)
        return position


@router.get("/", response_model=list[PositionRead])
def list_positions(portfolio_id: int):
    with managed_session() as session:
        get_portfolio_or_404(session, portfolio_id)
        statement = select(Position).where(Position.portfolio_id == portfolio_id)
        return list(session.exec(statement))


@router.delete("/{position_id}", status_code=status.HTTP_204_NO_CONTENT)
def delete_position(portfolio_id: int, position_id: int):
    with managed_session() as session:
        get_portfolio_or_404(session, portfolio_id)
        position = session.get(Position, position_id)
        if not position or position.portfolio_id != portfolio_id:
            raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Position not found")
        session.delete(position)
        session.commit()
        return None
