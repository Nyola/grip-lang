#
#
#            Nimrod's Runtime Library
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## The ``intsets`` module implements an efficient int set implemented as a
## sparse bit set.
## **Note**: Since Nimrod currently does not allow the assignment operator to
## be overloaded, ``=`` for int sets performs some rather meaningless shallow
## copy; use ``assign`` to get a deep copy.

import
  os, hashes, math

type
  TBitScalar = int

const 
  InitIntSetSize = 8         # must be a power of two!
  TrunkShift = 9
  BitsPerTrunk = 1 shl TrunkShift # needs to be a power of 2 and
                                  # divisible by 64
  TrunkMask = BitsPerTrunk - 1
  IntsPerTrunk = BitsPerTrunk div (sizeof(TBitScalar) * 8)
  IntShift = 5 + ord(sizeof(TBitScalar) == 8) # 5 or 6, depending on int width
  IntMask = 1 shl IntShift - 1

type
  PTrunk = ref TTrunk
  TTrunk {.final.} = object 
    next: PTrunk             # all nodes are connected with this pointer
    key: int                 # start address at bit 0
    bits: array[0..IntsPerTrunk - 1, TBitScalar] # a bit vector
  
  TTrunkSeq = seq[PTrunk]
  TIntSet* {.final.} = object ## an efficient set of 'int' implemented as a
                              ## sparse bit set
    counter, max: int
    head: PTrunk
    data: TTrunkSeq

proc mustRehash(length, counter: int): bool {.inline.} = 
  assert(length > counter)
  result = (length * 2 < counter * 3) or (length - counter < 4)

proc nextTry(h, maxHash: THash): THash {.inline.} = 
  result = ((5 * h) + 1) and maxHash 

proc IntSetGet(t: TIntSet, key: int): PTrunk = 
  var h = key and t.max
  while t.data[h] != nil: 
    if t.data[h].key == key: 
      return t.data[h]
    h = nextTry(h, t.max)
  result = nil

proc IntSetRawInsert(t: TIntSet, data: var TTrunkSeq, desc: PTrunk) = 
  var h = desc.key and t.max
  while data[h] != nil: 
    assert(data[h] != desc)
    h = nextTry(h, t.max)
  assert(data[h] == nil)
  data[h] = desc

proc IntSetEnlarge(t: var TIntSet) = 
  var n: TTrunkSeq
  var oldMax = t.max
  t.max = ((t.max + 1) * 2) - 1
  newSeq(n, t.max + 1)
  for i in countup(0, oldmax): 
    if t.data[i] != nil: IntSetRawInsert(t, n, t.data[i])
  swap(t.data, n)

proc IntSetPut(t: var TIntSet, key: int): PTrunk = 
  var h = key and t.max
  while t.data[h] != nil: 
    if t.data[h].key == key: 
      return t.data[h]
    h = nextTry(h, t.max)
  if mustRehash(t.max + 1, t.counter): IntSetEnlarge(t)
  inc(t.counter)
  h = key and t.max
  while t.data[h] != nil: h = nextTry(h, t.max)
  assert(t.data[h] == nil)
  new(result)
  result.next = t.head
  result.key = key
  t.head = result
  t.data[h] = result

proc contains*(s: TIntSet, key: int): bool =
  ## returns true iff `key` is in `s`.  
  var t = IntSetGet(s, `shr`(key, TrunkShift))
  if t != nil: 
    var u = key and TrunkMask
    result = (t.bits[`shr`(u, IntShift)] and `shl`(1, u and IntMask)) != 0
  else: 
    result = false
  
proc incl*(s: var TIntSet, key: int) = 
  ## includes an element `key` in `s`.
  var t = IntSetPut(s, `shr`(key, TrunkShift))
  var u = key and TrunkMask
  t.bits[`shr`(u, IntShift)] = t.bits[`shr`(u, IntShift)] or
      `shl`(1, u and IntMask)

proc excl*(s: var TIntSet, key: int) = 
  ## excludes `key` from the set `s`.
  var t = IntSetGet(s, `shr`(key, TrunkShift))
  if t != nil: 
    var u = key and TrunkMask
    t.bits[`shr`(u, IntShift)] = t.bits[`shr`(u, IntShift)] and
        not `shl`(1, u and IntMask)

proc containsOrIncl*(s: var TIntSet, key: int): bool = 
  ## returns true if `s` contains `key`, otherwise `key` is included in `s`
  ## and false is returned.
  var t = IntSetGet(s, `shr`(key, TrunkShift))
  if t != nil: 
    var u = key and TrunkMask
    result = (t.bits[`shr`(u, IntShift)] and `shl`(1, u and IntMask)) != 0
    if not result: 
      t.bits[`shr`(u, IntShift)] = t.bits[`shr`(u, IntShift)] or
          `shl`(1, u and IntMask)
  else: 
    incl(s, key)
    result = false
    
proc initIntSet*: TIntSet =
  ## creates a new int set that is empty.
  newSeq(result.data, InitIntSetSize)
  result.max = InitIntSetSize-1
  result.counter = 0
  result.head = nil

proc assign*(dest: var TIntSet, src: TIntSet) =
  ## copies `src` to `dest`. `dest` does not need to be initialized by
  ## `initIntSet`. 
  dest.counter = src.counter
  dest.max = src.max
  newSeq(dest.data, src.data.len)
  
  var it = src.head
  while it != nil:
    
    var h = it.key and dest.max
    while dest.data[h] != nil: h = nextTry(h, dest.max)
    assert(dest.data[h] == nil)

    var n: PTrunk
    new(n)
    n.next = dest.head
    n.key = it.key
    n.bits = it.bits
    dest.head = n
    dest.data[h] = n

    it = it.next

iterator items*(s: TIntSet): int {.inline.} =
  ## iterates over any included element of `s`.
  var r = s.head
  while r != nil:
    var i = 0
    while i <= high(r.bits):
      var w = r.bits[i] 
      # taking a copy of r.bits[i] here is correct, because
      # modifying operations are not allowed during traversation
      var j = 0
      while w != 0:         # test all remaining bits for zero
        if (w and 1) != 0:  # the bit is set!
          yield (r.key shl TrunkShift) or (i shl IntShift +% j)
        inc(j)
        w = w shr 1
      inc(i)
    r = r.next

template dollarImpl(): stmt =
  result = "{"
  for key in items(s):
    if result.len > 1: result.add(", ")
    result.add($key)
  result.add("}")

proc `$`*(s: TIntSet): string =
  ## The `$` operator for int sets.
  dollarImpl()

proc empty*(s: TIntSet): bool {.inline.} =
  ## returns true if `s` is empty. This is safe to call even before
  ## the set has been initialized with `initIntSet`.
  result = s.counter == 0

when isMainModule:
  var x = initIntSet()
  x.incl(1)
  x.incl(2)
  x.incl(7)
  x.incl(1056)
  for e in items(x): echo e

  var y: TIntSet
  assign(y, x)
  for e in items(y): echo e

