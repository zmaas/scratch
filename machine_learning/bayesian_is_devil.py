'''Naive Implementation of the Monty Hall Problem'''
import random

doors = ["goat"] * 2 + ["car"]

win = 0
loss = 0

for i in range(100000):
    # Shuffle Doors
    random.shuffle(doors)
    # Pick a random choice
    n = random.randrange(3)
    sequence = list(range(3))
    random.shuffle(sequence)
    if doors[n] == "car":
        loss += 1
    else:
        win += 1

## This is 2/3, showing that bayesian statistics is witchcraft
print("Switching Win Rate: ", (100*win)/(win+loss), "%")
