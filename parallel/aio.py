"""Simple example of asyncio in Python3"""
import asyncio

async def main():
    """Trivial example function."""
    print('Hello ...')
    await asyncio.sleep(1)
    print('... World!')

asyncio.run(main())
