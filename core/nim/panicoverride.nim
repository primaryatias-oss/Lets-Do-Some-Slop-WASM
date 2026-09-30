# Required by --os:standalone: what to do on a fatal error (nothing to print to).
proc rawoutput(s: string) = discard
proc panic(s: string) {.noreturn.} =
  while true: discard
