with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
procedure Checkpres is
   --  input file:  signups
   People : File_Type;
   --  output files: times.html, names.html
   Times : File_Type;
   Pos, Lno : Integer := 0;
   Buf : String (1 .. 1024) := (others => ' ');
   Last, First : Integer := 0;

   type Day_Record is record
      Month : Integer;
      Day : Integer;
   end record;

   type Day_Array is array (Natural range <>) of Day_Record;
   Presentation_Days : constant Day_Array := ((9, 23), (9, 30), (10, 7), (10, 14), (10, 21),
                                              (10, 28), (11, 04), (11, 18), (12, 2), (12, 9));
   type String_Access is access String;

   type Student_Index is new Integer range -1 .. 52;
   Student_Names : array (Student_Index range 0 .. 52) of String_Access;
   Last_Student : Student_Index := Student_Names'First - 1;

   Name_Order : array (Student_Names'Range) of Student_Index;

   type Presentation is record
      Student : Student_Index := -1;
      Topic   : String_Access := null;
   end record;

   type Slot_Index is new Integer;
   Slots : array (Slot_Index range 0 .. Presentation_Days'Length * 8 - 1) of Presentation;

   subtype Time_String is String (1 .. 5);
   Presentation_Times : constant array (0 .. 7) of Time_String :=
     ("12:20", "12:30", "12:40", "12:50", "01:25", "01:35", "01:45", "01:55");

   function Time (I : Slot_Index) return String is
   begin
      return Presentation_Times (Integer (I) mod 8);
   end Time;

   function Recitation (I : Slot_Index) return Positive is
   begin
      return 1 + (Integer (I) / 4) mod 2;
   end Recitation;

   function Month (I : Slot_Index) return Positive is
   begin
      return Presentation_Days (Integer (I) / 8).Month;
   end Month;

   function Day (I : Slot_Index) return Positive is
   begin
      return Presentation_Days (Integer (I) / 8).Day;
   end Day;

   procedure Error (Msg : String) is
   begin
      Put_Line (Msg);
      raise Data_Error;
   end Error;

   function Find_Slot (Month : Positive;
                       Day : Positive;
                       Recitation : Positive;
                       Slot : Positive) return Slot_Index is
      J, K : Slot_Index;
   begin
      for I in Presentation_Days'range loop
         if Presentation_Days (I).Month = Month and then
           Presentation_Days (I).Day = Day then
            J := Slot_Index( I - Presentation_Days'First);
            K := J * 8 + Slot_Index (Recitation - 1) * 4 + Slot_Index (Slot) - 1;
            return Slots'First + Slot_Index (K);
         end if;
      end loop;
      Error ("problem with slot identification");
      return -1;
   end Find_Slot;

   function Find_Slot (S : Student_Index) return Slot_Index is
   begin
      for I in Slots'range loop
         if Slots (I).Student = S then return I;
         end if;
      end loop;
      return -1;
   end Find_Slot;

   procedure Check_Topic (Topic : String; Section : Integer) is
   begin
      for I in Slots'range loop
         if Recitation (I) = Section then
            declare
               T : String_Access renames Slots (I).Topic;
            begin
               if T /= null and then T.all /= "TBD" and then
                 T.all (T.all'First .. T.all'First + 2) =
                 Topic (Topic'First .. Topic'First + 2) then
                  Error ("""" & Topic & """" & " apparently duplicates """ &
                         T.all & """");
               end if;
            end;
         end if;
      end loop;
   end Check_Topic;

   procedure Sort_Names is
      I, J, K : Student_Index;
   begin
      for L in Name_Order'Range loop
         Name_Order (L) := L;
      end loop;
      I := Student_Names'First;
  Sort:
      while I < Last_Student loop
         --  items 1 .. I are sorted
         while Student_Names (Name_Order (I)).all <= Student_Names (Name_Order (I+1)).all
         loop
            -- items 1 .. I + 1 are sorted
            I := I + 1;
            if I >= Last_Student then exit Sort; end if;
         end loop;
         -- items 1 .. I are sorted; item I+1 < item I
         K := Name_Order (I + 1); J := I; I := J + 1;
         loop
            -- items 1 .. J are sorted; tsk < items J+1 .. I are sorted
            Name_Order (J + 1) := Name_Order (J);
            exit when J = 1 or else
              Student_Names (Name_Order (J - 1)).all  <= Student_Names (K).all;
            J := J - 1;
         end loop;
         Name_Order (J) := K;
      end loop Sort;
   end Sort_Names;

begin

   --  read in data from signup file
   --  line format: "student name" section month day section slot "topic"

   Open (People, In_File, "signups");
   declare
      Month : Integer range 9 .. 12;
      Day : Integer range 1 .. 31;
      Section : Integer range 1 .. 2;
      Slot : Integer range 1 .. 4;
      I : Slot_Index;
   begin
      loop
         Get_Line (People, Buf, Last);
         exit when Last < Buf'First;
         Lno := Lno + 1;
         --   extract student name field
         if Last = Buf'Last then
            Error ("input line longer than " & Integer'Image (Last - 1));
         end if;
         Pos := Buf'First;
         if Buf (Pos) /= '"' then
            Error ("missing "" at start of name field");
         end if;
         Pos := Pos + 1; First := Pos;
         while Pos  <= Last and then Buf (Pos) /= '"' loop
            Pos := Pos + 1;
         end loop;
         if Buf (Pos) /= '"' then
            Error ("missing "" at end of name field");
         end if;
         Last_Student := Last_Student + 1;
         Student_Names (Last_Student) := new String'(Buf (First .. Pos - 1));
         Get (Buf (Pos + 1 .. Last), Section, Pos);
         if Pos < Last then
            --  read month, day, section, and slot as integers
            Get (Buf (Pos + 1 .. Last), Month, Pos);
            Get (Buf (Pos + 1 .. Last), Day, Pos);
            Get (Buf (Pos + 1 .. Last), Slot, Pos);
            I := Find_Slot (Month, Day, Section, Slot);
            --  read topic
            loop
               Pos := Pos + 1;
               exit when Pos  = Last or else Buf (Pos) /= ' ';
            end loop;
            if Buf (Pos) /= '"' then
               Error ("missing "" at start of topic field");
            end if;
            Pos := Pos + 1; First := Pos;
            while Pos  <= Last and then Buf (Pos) /= '"' loop
               Pos := Pos + 1;
            end loop;
            if Buf (Pos) /= '"' then
               Error ("missing "" at end of topic field");
            end if;
            Check_Topic (Buf (First .. Pos - 1), Section);
            if Slots (I).Student /= -1 then
               Error ("conflicting assignments: "
                         & Student_Names (Last_Student).all & "vs."
                         & Student_Names (Slots (I).Student).all);
               --  don't bother freeing Name_String,
               --  since this is a one-shot program
               --  and the number of names is few
            else
               Slots (I).Student := Last_Student;
               Slots (I).Topic := new String' (Buf (First .. Pos - 1));
            end if;
         end if;
      end loop;
   exception
      when End_Error => null;
   end;

   begin
      Open (Times, Out_File, "times.html");
   exception
      when Name_Error =>
         Create (Times, Out_File, "times.html");
   end;

   Put_Line (Times, "<!DOCTYPE HTML PUBLIC ""-//W3C//DTD HTML 4.01 Transitional//EN""><HTML><HEAD>");
   Put_Line (Times, "<TITLE>times.html</TITLE>");
   Put_Line (Times, "<META HTTP-EQUIV=""CONTENT-TYPE"" CONTENT=""TEXT/HTML; CHARSET=ISO-8859-1"">");
   Put_Line (Times, "<LINK REL=""stylesheet"" TYPE=""text/css"" href=""style.css"" title=""stylesheet"">");
   Put_Line (Times, "</HEAD><BODY><TABLE WIDTH=""100%""><TR><TD BGCOLOR=""#660000"" ALIGN=RIGHT>");
   Put_Line (Times, "<A HREF=""../index.html""><U><FONT COLOR=""CCCC99""><FONT SIZE=2>");
   Put_Line (Times, "COP 4020: Programming Languages</FONT>&uarr;</FONT></U></A></TD></TR></TABLE>");
   Put_Line (Times, "<CENTER><H1 ALIGN=""CENTER"">Presentation Schedule</H1></CENTER>");
   Put_Line (Times, "<TABLE WIDTH=""100%""><TR><TD BGCOLOR=""#CCCC99""><B>Fall Term 2005</B></TD></TR></TABLE>");
   Put_Line (Times, "<P></P><UL><LI><A HREF=""times.html#Name"">by name</A></LI>");
   Put_Line (Times, "<LI><A HREF=""times.html#Time"">by time</A></LI></UL><HR>");
   Put_Line (Times, "<A NAME=""Time"">");
   Put_Line (Times, "<P></P><TABLE BORDER>");
   Put_Line (Times, "<TR><TH>Month</TH><TH>Day</TH><TH>Time</TH><TH>Student</TH><TH>Topic</TH></TR>");

   for I in Slots'Range loop
      if I mod 8 = 0 then
        Put (Times, "<TR><TD COLSPAN=5 BGCOLOR=""#ff6600""></TD></TR>");
      end if;
      --  create html line for this slot, showing the person and topic
      Put (Times, "<TR><TD>"); Put (Times, Month (I));
      Put (Times, "</TD><TD>"); Put (Times, Day (I));
      Put (Times, "</TD><TD>"); Put (Times, Time (I));
      if Slots(I).Student /= -1 then
         Put (Times, "</TD><TD>"); Put (Times, Student_Names (Slots(I).Student).all);
         Put (Times, "</TD><TD>"); Put (Times, Slots(I).Topic.all);
      end if;
      Put_Line (Times, "</TD></TR>");
   end loop;

   Sort_Names;

   Put_Line (Times, "</TABLE><HR><TABLE BORDER><A NAME=""Name"">");
   Put_Line (Times, "<TR><TH>Name</TH><TH>Month</TH><TH>Day</TH><TH>Time</TH><TH>Topic</TH></TR>");
   declare
      I : Slot_Index;
   begin
      for S in Student_Names'First .. Last_Student loop
         --  create html line for this person, showing time and topic
         Put (Times, "<TR><TD>");  Put (Times, Student_Names (Name_Order (S)).all);
         I := Find_Slot (Name_Order(S));
         if I >= Slots'First then
            Put (Times, "</TD><TD>"); Put (Times, Month (I));
            Put (Times, "</TD><TD>"); Put (Times, Day (I));
            Put (Times, "</TD><TD>"); Put (Times, Time (I));
            Put (Times, "</TD><TD>"); Put (Times, Slots(I).Topic.all);
         end if;
         Put_Line (Times, "</TD></TR>");
      end loop;
   end;

   Put_Line (Times, "</TABLE>");
   Put_Line (Times, "<TABLE WIDTH=""100%""><TR><TD BGCOLOR=""#CCCC99""><FONT SIZE=""1"">&copy; 2005");
   Put_Line (Times, "<A HREF=""http://www.cs.fsu.edu/~baker"">T. P. Baker</A>. <BR> ($Id: checkpres.adb,v 1.2 2005/09/08 20:36:47 baker Exp baker $)");
   Put_Line (Times, "</FONT>&nbsp;</TD></TR></TABLE> </BODY> </HTML>");

   Close (Times);

exception
   when E : others =>
      Put (Lno, 4); Put (": '"); Put (Buf (Buf'First .. Last));
      Put_Line ("'");  Put ("       ");
      for I in 1 .. Pos loop
         Put (' ');
      end loop;
      Put_Line ("^");
      Put_Line (Ada.Exceptions.Exception_Name (E));  New_Line;
      raise;
end Checkpres;