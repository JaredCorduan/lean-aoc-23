# Advent of Code 2023 in Lean

## Running

Run the `lean-aoc-23` executable with:

```shell
lean-aoc-23 [real/test] [num] [a/b]
```

where
* "real" ("test") runs the real (test) data
* num is the day (Dec 1 is `1`, etc)
* "a" is for part A, "b" is for part B

For example, to run the real data for day 1, part A:

```shell
lake exec lean-aoc-23 real 1 a
```

## Adding new days

To add day XY:

1. Add a file `LeanAoc23/DayXY.lean` with:
   ```
   namespace DayXY

   -- Part A

   def a (_lines : List String) : String := "dunno"

   -- Part B

   def b (_lines : List String) : String := "dunno"

   namespace DayXY
   ```
   where `a` is the answer to part A, and `b` is the answer to part B.
   The return types of `a` and `b` can be anything with a `ToString` instance.
2. Add `import LeanAoc23.DayXY` to `LeanAoc23.lean`
3. Add pattern matching lines to the `run` function inside `Main.lean`.
4. Add data files:
  * `files/real/XY.txt`
  * `files/test/XYa.txt`
  * `files/test/XYb.txt`
5. Solve puzzle :)
