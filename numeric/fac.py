"""Memoized factorial in python"""

def factorial(i, lookup_table):
    """ Memoized factorial function

    :param i: Current i, int
    :param lookup_table: Lookup table
    :returns: i's factorial
    :rtype: int

    """
    if i in lookup_table:
        return lookup_table[i]
    if i in (0, 1):
        return 1
    temp = i * factorial(i - 1, lookup_table)
    lookup_table[i] = temp
    return temp

LOOKUP_TABLE = {}
F = factorial(199, LOOKUP_TABLE)
print(F)
