import networkx

G = networkx.Graph()

INPUT = "input"

with open(INPUT, "r") as f:
  for ln in f:
    xs = ln.strip().split(" <-> ")
    fr = int(xs[0])
    tos = [int(s) for s in xs[1].split(", ")]
    for t in tos:
      G.add_edge(fr, t)

r = networkx.descendants(G, 0)
print(len(r))
print(networkx.number_connected_components(G))
