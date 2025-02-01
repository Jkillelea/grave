--  with GNAT.Sockets;
with Ada.Text_IO;
with Ada.Command_Line;
with DNS;

procedure Grave is
begin
   for I in 1 .. Ada.Command_Line.Argument_Count loop
      declare
         Response : constant String := DNS.Resolve (Ada.Command_Line.Argument (I));
      begin
         Ada.Text_IO.Put_Line ("Answer: " & Response);
      end;
   end loop;
end Grave;
