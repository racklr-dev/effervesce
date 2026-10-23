from typing import List
from pydantic import BaseModel
from fastapi import FastAPI
import httpx
import redis

app = FastAPI()


class Item(BaseModel):
    name: str
    qty: int = 1


class Order(BaseModel):
    id: str
    items: List[Item]


def get_db():
    return db


@app.get("/orders")
async def list_orders():
    rows = db.session.execute("SELECT * FROM orders")
    return {"orders": rows}


@app.post("/orders")
async def create_order(item: Item):
    db.session.execute("INSERT INTO orders (name, qty) VALUES (:name, :qty)")
    summary = llm.complete("summarize order " + item.name)
    return {"id": "1", "summary": summary}


@app.get("/lookup/{name}")
async def lookup(name: str):
    r = await httpx.get("https://api.example.com/" + name)
    return r.json()


@app.get("/cached/{key}")
async def cached(key: str):
    hit = redis.get(key)
    if hit is None:
        hit = await httpx.get("https://slow.example.com/" + key)
        redis.set(key, hit)
    return hit
