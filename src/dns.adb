with Ada.Text_IO;

package body DNS is
   function Query (Domain : String) return String is
   begin
      Ada.Text_IO.Put_Line ("Requested: " & Domain);
      delay 1.0;
      return Domain;
   end Query;
end DNS;
