with Ada.Text_IO;
with Ada.Command_Line;
with DNS; use DNS;
with Log_Level;
with Log;

procedure Grave is
   Response : DNS.DNS_Response;
   package Logger is new Log (Local_Log_Level => Log_Level.Info);
begin
   for I in 1 .. Ada.Command_Line.Argument_Count loop
      DNS.Resolve (Ada.Command_Line.Argument (I), Response);
      if Response.Status = DNS.Response_Ok then
         for J in DNS.IP_Count range 1 .. Response.Count loop
            Logger.Info ("  " & Response.IPs (J));
         end loop;
      else
         Logger.Error ("Error resolving domain");
      end if;
      Ada.Text_IO.New_Line;
   end loop;
end Grave;
