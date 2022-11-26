MODULE primes;

FROM Terminal IMPORT WriteString,WriteLn,Read;
FROM SmallIO IMPORT WriteCard;

CONST
  size = 8190;

VAR
  flags : ARRAY [ 0 .. size ] OF BOOLEAN;
  i, prime, k, count, iter : CARDINAL;
  ch : CHAR;

BEGIN
  WriteString("Type Return"); Read(ch);
  WriteString("10 iterations"); WriteLn;
  FOR iter := 1 TO 10 DO
    count := 0;
    FOR i := 0 TO size DO flags[i] := TRUE END;
    FOR i := 0 TO size DO
      IF flags[i] THEN
        prime := i + i + 3;
        k := i + prime;
        WHILE k <= size DO
          flags[k] := FALSE;
          k := k + prime;
        END;
        count := count + 1;
      END;
    END;
  END;
  WriteCard(count, 1); WriteString(" primes"); WriteLn;
  WriteString("done"); WriteLn;
END primes.
