from __future__ import annotations

from datetime import datetime
from typing import Optional

from sqlmodel import Field, Relationship, SQLModel


class Portfolio(SQLModel, table=True):
    id: Optional[int] = Field(default=None, primary_key=True)
    code: str = Field(index=True, unique=True)
    name: str
    owner: str
    status: str = Field(default="ACTIVE")
    risk_rating: str = Field(default="MEDIUM")

    positions: list["Position"] = Relationship(back_populates="portfolio")
    transactions: list["Transaction"] = Relationship(back_populates="portfolio")


class Position(SQLModel, table=True):
    id: Optional[int] = Field(default=None, primary_key=True)
    portfolio_id: int = Field(foreign_key="portfolio.id")
    symbol: str
    quantity: float
    market_value: float

    portfolio: Optional[Portfolio] = Relationship(back_populates="positions")


class Transaction(SQLModel, table=True):
    id: Optional[int] = Field(default=None, primary_key=True)
    portfolio_id: int = Field(foreign_key="portfolio.id")
    trade_date: datetime
    symbol: str
    quantity: float
    price: float
    type: str

    portfolio: Optional[Portfolio] = Relationship(back_populates="transactions")


class ErrorLog(SQLModel, table=True):
    id: Optional[int] = Field(default=None, primary_key=True)
    context: str
    message: str
    severity: str = Field(default="ERROR")
    created_at: datetime = Field(default_factory=datetime.utcnow)


class BatchJob(SQLModel, table=True):
    id: Optional[int] = Field(default=None, primary_key=True)
    name: str
    status: str = Field(default="QUEUED")
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    result_path: Optional[str] = None
    details: Optional[str] = None
