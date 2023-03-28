--  with GNAT.Sockets;
with Ada.Text_IO;
with Ada.Command_Line;
with DNS;

procedure Grave is
   Question : DNS.Question;
   Answer : DNS.Answer;
begin
   for I in 1 .. Ada.Command_Line.Argument_Count loop
      declare
         Tmp : String := DNS.Resolve (Ada.Command_Line.Argument (I));
      begin
         null;
      end;
   end loop;
end Grave;
