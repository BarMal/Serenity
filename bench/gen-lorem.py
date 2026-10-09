#!/usr/bin/env python3
"""Generate a ~300-paragraph lorem ipsum benchmark file."""
import random, sys

words = (
    "lorem ipsum dolor sit amet consectetur adipiscing elit sed do eiusmod tempor "
    "incididunt ut labore et dolore magna aliqua ut enim ad minim veniam quis nostrud "
    "exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat duis aute "
    "irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
    "pariatur excepteur sint occaecat cupidatat non proident sunt in culpa qui officia "
    "deserunt mollit anim id est laborum"
).split()

random.seed(42)
paras = []
for _ in range(300):
    n = random.randint(4, 8)
    sentences = []
    for _ in range(n):
        wc = random.randint(8, 20)
        s = " ".join(random.choices(words, k=wc))
        sentences.append(s.capitalize() + ".")
    paras.append(" ".join(sentences))

print("\n\n".join(paras))
