with System;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

package DNS is
   type Query_Response is (Q, R);
   for Query_Response use (Q => 0, R => 1);
   for Query_Response'Size use 1; -- bit

   type Header is
        record
            Id      : Unsigned_16;
            Qr      : Query_Response;
            Opcode  : Unsigned_4;
            Aa      : Unsigned_1;
            Tc      : Unsigned_1;
            Rd      : Unsigned_1;
            Ra      : Unsigned_1;
            Z       : Unsigned_3;
            Rcode   : Unsigned_4;
            QdCount : Unsigned_16;
            AnCount : Unsigned_16;
            NsCount : Unsigned_16;
            ArCount : Unsigned_16;
        end record;

   --  Bitfield layout in 12 bytes (96 bits)
   for Header'Size use 12 * System.Storage_Unit;

   -- byte align
   for Header'Alignment use 1;

   for Header use
        record
            Id      at 0 range  0 .. 15;
            Qr      at 0 range 16 .. 16;
            Opcode  at 0 range 17 .. 20;
            Aa      at 0 range 21 .. 21;
            Tc      at 0 range 22 .. 22;
            Rd      at 0 range 23 .. 23;
            Ra      at 0 range 24 .. 24;
            Z       at 0 range 25 .. 27;
            Rcode   at 0 range 28 .. 31;
            QdCount at 0 range 32 .. 47;
            AnCount at 0 range 48 .. 63;
            NsCount at 0 range 64 .. 79;
            ArCount at 0 range 80 .. 95;
        end record;

   type Question is
       record
           -- TODO
           Hdr : Header;
       end record;

   type Answer is
       record
           -- TODO
           Hdr : Header;
       end record;

   function Resolve (Domain : String) return String;

private
    Last_Id : Unsigned_16 := 0;

end DNS;
