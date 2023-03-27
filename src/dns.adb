with Ada.Text_IO;

package body DNS is
   function Resolve (Domain : String) return String is
       Request : Question := (
           Hdr =>
               (Id => Last_Id,
               Qr => Q,
               Opcode => 0,
               Aa => 0,
               Tc => 0,
               Rd => 0,
               Ra => 0,
               Z => 0,
               Rcode => 0,
               QdCount => 1, -- Question
               AnCount => 0,
               NsCount => 0,
               ArCount => 0)
           );
   begin
      Last_Id := Last_Id + 1;
      Ada.Text_IO.Put_Line ("Requested: " & Domain);
      delay 1.0;
      return Domain;
   end Resolve;

end DNS;
