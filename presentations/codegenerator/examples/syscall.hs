data Syscall
    = PopPrintInt  -- trap 0
    | PopPrintChar -- trap 1
    derive Eq
