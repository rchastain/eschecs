
uses
  mater;

const
  SEARCH_ALL_MOVES = TRUE;
  SEARCH_CHECK_ONLY = FALSE;

begin
  WriteLn('b7a8' = SolveMate('b7/PP6/8/8/7K/6B1/6N1/4R1bk w KQkq -', 3, SEARCH_ALL_MOVES));
  WriteLn('h6c1' = SolveMate('8/8/1p5B/4p3/1p2k1P1/1P3n2/P4PB1/K2R4 w KQkq -', 3, SEARCH_ALL_MOVES));
  WriteLn('d6c7' = SolveMate('2N5/8/k2K4/8/p1PB4/P7/8/8 w KQkq -', 4, SEARCH_ALL_MOVES));
  WriteLn('f6d5' = SolveMate('rnbK2R1/p6p/p1kNpN1r/P3B1Q1/3P1p1p/5p2/5p1b/8 w KQkq -', 4, SEARCH_ALL_MOVES));
  WriteLn('h7g7' = SolveMate('8/1n2P2K/3p2p1/2p3pk/6pr/4ppr1/6p1/1b6 w KQkq -', 3, SEARCH_ALL_MOVES));
  WriteLn('b7b8' = SolveMate('4K1R1/PP2P3/2k5/3pP3/3B4/6P1/8/8 w KQkq -', 3, SEARCH_ALL_MOVES));
  WriteLn('e7e8' = SolveMate('8/2P1P1P1/3PkP2/8/4K3/8/8/8 w Qkq -', 3, SEARCH_ALL_MOVES));
  WriteLn('h3g5' = SolveMate('3nn3/2p2p1k/1p1pp1p1/p2B3p/r2B2N1/7N/8/7K w KQkq -', 12, SEARCH_CHECK_ONLY));
end.
