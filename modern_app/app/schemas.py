from __future__ import annotations

from datetime import datetime
from typing import Optional

from pydantic import BaseModel, Field


class PortfolioCreate(BaseModel):
    code: str = Field(pattern=r"^[A-Z0-9]{4,10}$")
    name: str
    owner: str
    risk_rating: str = "MEDIUM"


class PortfolioUpdate(BaseModel):
    name: Optional[str] = None
    owner: Optional[str] = None
    status: Optional[str] = None
    risk_rating: Optional[str] = None


class PortfolioRead(BaseModel):
    id: int
    code: str
    name: str
    owner: str
    status: str
    risk_rating: str

    class Config:
        from_attributes = True


class PositionCreate(BaseModel):
    symbol: str
    quantity: float
    market_value: float


class PositionRead(BaseModel):
    id: int
    portfolio_id: int
    symbol: str
    quantity: float
    market_value: float

    class Config:
        from_attributes = True


class TransactionCreate(BaseModel):
    trade_date: datetime
    symbol: str
    quantity: float
    price: float
    type: str


class TransactionRead(BaseModel):
    id: int
    portfolio_id: int
    trade_date: datetime
    symbol: str
    quantity: float
    price: float
    type: str

    class Config:
        from_attributes = True


class BatchJobRead(BaseModel):
    id: int
    name: str
    status: str
    started_at: Optional[datetime]
    completed_at: Optional[datetime]
    result_path: Optional[str]
    details: Optional[str]

    class Config:
        from_attributes = True


class ErrorLogRead(BaseModel):
    id: int
    context: str
    message: str
    severity: str
    created_at: datetime

    class Config:
        from_attributes = True
