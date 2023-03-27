--  with GNAT.Sockets;
with Ada.Text_IO;
with Ada.Command_Line;
with DNS;

procedure Grave is
   Hdr : DNS.Header;
   Question : DNS.Question;
   Answer : DNS.Answer;
begin
   Ada.Text_IO.Put_Line (Hdr'Img);

   for I in 1 .. Ada.Command_Line.Argument_Count loop
      declare
         Tmp : String := DNS.Resolve (Ada.Command_Line.Argument (I));
      begin
         null;
      end;
   end loop;
end Grave;
