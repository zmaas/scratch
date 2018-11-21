"""Trivial Magic 8-Ball Program, for Madre."""

# Import so we can get random numbers
import random

#Our list of responses that might be given by the magic 8 ball. We
#can add arbitrarily many items to this and the program will still
#work.
responses = ["Yes", "No", "Maybe", "Signs Point to Yes", "Probably Not"]
# The while loop will continue to run while the user enters anything
# before hitting enter, since nonempty strings return a True value
# while empty strings return a False value.
while input("(Enter To Quit) Ask a question: "):
    # Print some random response from our array.
    print(random.choice(responses))
