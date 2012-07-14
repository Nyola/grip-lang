discard """
  file: "tclosure3.nim"
  output: "success"
  disabled: true
"""

proc main =
  const n = 30
  for iterations in 0..50_000:
    var s: seq[proc(): int {.closure.}] = @[]
    for i in 0 .. n-1:
      let ii = i
      s.add(proc(): int = return ii*ii)
    for i in 0 .. n-1:
      let val = s[i]()
      if val != i*i: echo "bug  ", val
    
    if getOccupiedMem() > 3000_000: quit("still a leak!")
  echo "success"

main()
