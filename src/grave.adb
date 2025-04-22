--  with GNAT.Sockets;
with Ada.Text_IO;
with Ada.Command_Line;
with DNS; use DNS;

procedure Grave is
   Response : DNS.DNS_Response;
begin
   for I in 1 .. Ada.Command_Line.Argument_Count loop
      declare
         Domain : constant String := Ada.Command_Line.Argument (I);
      begin
         DNS.Logging_Level := DNS.Debug;
         DNS.Resolve (Domain, Response);
         if Response.Status = DNS.Response_Ok then
            for J in DNS.IP_Count range 1 .. Response.Count loop
               Ada.Text_IO.Put_Line ("  " & Response.IPs (J));
            end loop;
         else
            Ada.Text_IO.Put_Line ("Error resolving domain");
         end if;
         Ada.Text_IO.New_Line;
      end;
   end loop;
end Grave;
