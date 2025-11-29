(** Register the solution functions here (one for each day). *)
let register_all_days () =
  Lib.FuncRegister.add ~day:1 (Day01.part1, Day01.part2);
  Lib.FuncRegister.add ~day:2 (Day02.part1, Day02.part2);
  Lib.FuncRegister.add ~day:3 (Day03.part1, Day03.part2);
  Lib.FuncRegister.add ~day:4 (Day04.part1, Day04.part2);
  Lib.FuncRegister.add ~day:5 (Day05.part1, Day05.part2);
  Lib.FuncRegister.add ~day:6 (Day06.part1, Day06.part2);
  Lib.FuncRegister.add ~day:7 (Day07.part1, Day07.part2);
  Lib.FuncRegister.add ~day:8 (Day08.part1, Day08.part2);
  Lib.FuncRegister.add ~day:9 (Day09.part1, Day09.part2);
  Lib.FuncRegister.add ~day:10 (Day10.part1, Day10.part2);
  Lib.FuncRegister.add ~day:11 (Day11.part1, Day11.part2);
  Lib.FuncRegister.add ~day:12 (Day12.part1, Day12.part2);
