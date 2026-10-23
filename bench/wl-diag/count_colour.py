import sys
from PIL import Image
path, r, g, b = sys.argv[1], *map(int, sys.argv[2].split(','))
im = Image.open(path).convert('RGB')
n = sum(1 for px in im.getdata() if px == (r, g, b))
print(n)
