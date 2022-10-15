pragma Ada_2022;

procedure Stupid_Sort with SPARK_Mode => On
is
   type I_Type_Base is new Integer range 0 .. Integer'Last;
   subtype I_Type is I_Type_Base range 1 .. I_Type_Base'Last;
   type A_Type is array (I_Type range <>) of Natural;

   function Sorted (A : A_Type; From, To : I_Type_Base) return Boolean is
      (for all I in From .. To =>
         (for all J in I .. To =>
            A(I) <= A(J)))
   with
     Pre => From in A'Range
       and then To <= A'Last;

   function Is_Max (M : I_Type; A : A_Type; From, To : I_Type_Base) return Boolean is
     (for all I in From .. To => A (I) <= A (M))
   with
     Pre => M in From .. To
       and then From in A'Range
       and then To in A'Range;

   procedure Sort (A : in out A_Type)
   with
     Post => (if A'Length > 0 then Sorted (A, A'First, A'Last))
   is
   begin
      for I in A'range loop
         for J in A'range loop
            if A(I) < A(J) then
               declare
                  Tmp : constant Natural := A(I);
               begin
                  A (I) := A (J);
                  A (J) := Tmp;
               end;
            end if;

            pragma Loop_Invariant (if J < I then A(J) <= A(I));
            pragma Loop_Invariant (Sorted (A, A'First, I-1));
            pragma Loop_Invariant
              (if I = A'First then
                Is_Max (I, A, A'First, J)
              else
                (declare
                   M : constant I_Type := (if J < I-1 then I-1 else I);
                 begin
                   Is_Max (M, A, A'First, A'Last)));
         end loop;

         pragma Loop_Invariant (Sorted (A, A'First, I));
         pragma Loop_Invariant (Is_Max (I, A, A'First, A'Last));
      end loop;
   end Sort;

   A : A_Type (1 .. 1000);
begin
   for I in A'Range loop
     A (I) := Integer (A'Last - I + 1);
   end loop;

   Sort (A);
end Stupid_Sort;
