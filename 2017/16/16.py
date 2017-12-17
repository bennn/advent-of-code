import sys

def commands_of_file(fname):
  with open(fname, "r") as f:
    return f.read().strip().split(",")

def collapse_others(others):
  nums = list(range(0,16))
  for o in others:
    nums = apply_command(nums, o)
  return nums

def apply_command(nums, s):
  if "x" == s[0]:
    ss = s[1:].split("/")
    n0 = int(ss[0])
    n1 = int(ss[1])
    return apply_exchange(nums, n0, n1)
  elif "s" == s[0]:
    return apply_spin(nums, int(s[1:]))
  else:
    raise ValueError("oh noes")

def apply_spin(nums, i):
  ii = (len(nums)) - i
  return nums[ii::] + nums[:ii]

def apply_exchange(nums, n0, n1):
  nums[n0],nums[n1] = nums[n1],nums[n0]
  return nums

def is_partner(x):
  return "p" == x[0]

LETTERS = "abcdefghijklmnop"

def collapse_partners(ps):
  lets = list(LETTERS)
  for p in ps:
    ss = p[1::].split("/")
    p0 = ss[0]
    p1 = ss[1]
    i0 = lets.index(p0)
    i1 = lets.index(p1)
    lets[i0],lets[i1] = lets[i1],lets[i0]
  lmap = {}
  for i, letter in enumerate(LETTERS):
    lmap[letter] = lets[i]
  return lmap

def expo(omap):
  return [omap[omap[i]] for i in range(len(omap))]

def expp(pmap):
  hh = {}
  for k,v in pmap.items():
    hh[k] = pmap[v]
  return hh

def apply_pmap(pm, lets):
  tmp = list(lets)
  for let0, let1 in pm.items():
    tmp[lets.index(let0)] = let1
  return tmp

def apply_omap(om, lets):
  tmp = list(lets)
  for i in range(len(om)):
    tmp[i] = lets[om[i]]
  return tmp

def apply1(lets, pmap, omap):
  return apply_pmap(pmap, apply_omap(omap, lets))

def applyN(lets, pm, om, N):
  pms = [pm]
  oms = [om]
  ii = 0
  while (2 ** ii <= N):
    pms.append(expp(pms[-1]))
    oms.append(expo(oms[-1]))
    ii += 1
    continue
  numleft = N
  while (ii > 0):
    to_subtract = 2 ** ii
    if to_subtract < numleft:
      numleft -= to_subtract
      lets = apply1(lets, pms[ii], oms[ii])
    ii -= 1
    continue
  for _dontcare in range(numleft):
    lets = apply1(lets, pm, om)
  return lets

def go(fname):
  strs = commands_of_file(fname)
  partners = [x for x in strs if is_partner(x)]
  others = [x for x in strs if not is_partner(x)]
  omap = collapse_others(others)
  pmap = collapse_partners(partners)
  lets = list(LETTERS)
  lets1 = apply1(lets, pmap, omap)
  lets2 = applyN(lets, pmap, omap, 10 ** 9)
  print("part 1 : %s" % "".join(lets1))
  print("part 2 : %s" % "".join(lets2))
  return

if __name__ == "__main__":
  go(sys.argv[1])
