procedure Stupid_Sort
is
   type A_Type is array (Natural range 1 .. 5) of Natural;

   procedure Sort (A : in out A_Type)
   is
   begin
      for I in A'Range loop
         for J in A'Range loop
            if A (I) < A (J) then
               declare
                  Tmp : constant Natural := A (I);
               begin
                  A (I) := A (J);
                  A (J) := Tmp;
               end;
           end if;
         end loop;
      end loop;
   end Sort;

   A : A_Type;
begin
   for I in A'Range loop 
     A (I) := A'Last - I + 1;
   end loop;

   Sort (A);
end Stupid_Sort;
