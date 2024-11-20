import Lake
open Lake DSL

package "lean-aoc-23" where
  -- add package configuration options here

lean_lib «LeanAoc23» where
  -- add library configuration options here

@[default_target]
lean_exe "lean-aoc-23" where
  root := `Main
