with System;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

package DNS is

   --  type IP_Address_Variant is (V4, V6);
   --  type IP_Address_Bytes is array (Positive range <>) of Unsigned_1;
   --  type IP_Address (Variant : IP_Address_Variant) is record
   --      case Variant is
   --      when V4 =>
   --          V4_Bytes : IP_Address_Bytes (1 .. 4);
   --      when V6 =>
   --          V6_Bytes : IP_Address_Bytes (1 .. 32);
   --      end case;
   --  end record;

   type Query_Type is (Query, Response);
   for Query_Type use (Query => 0, Response => 1);
   for Query_Type'Size use 1; -- bit

   type Resolver is record
      Id : Integer;
      Flags : Integer;
      Num_Questions : Integer := 0;
      Num_Answers : Integer := 0;
      Num_Authorities : Integer := 0;
      Num_Additionals : Integer := 0;
   end record;

   type Header_Type is
        record
            Id      : Unsigned_16;
            Qr      : Query_Type; -- query or response
            Opcode  : Unsigned_4; -- usually zero
            Aa      : Unsigned_1; -- authoritative
            Tc      : Unsigned_1; -- truncated
            Rd      : Unsigned_1; -- recursion desired
            Ra      : Unsigned_1; -- recursion available
            Z       : Unsigned_3; -- reserved
            Rcode   : Unsigned_4; -- response code
            QdCount : Unsigned_16; -- question count
            AnCount : Unsigned_16; -- answer count
            NsCount : Unsigned_16; -- authority count
            ArCount : Unsigned_16; -- additional count
        end record;

   --  Bitfield layout in 12 bytes (96 bits)
   for Header_Type'Size use 12 * System.Storage_Unit;

   --  byte align
   for Header_Type'Alignment use 1;

   for Header_Type use
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

   type Question_Type (Name_Len : Positive) is
       record
           Qname : String (1 .. Name_Len); -- The domain name
           Qtype : Unsigned_16; -- The type of the query (PTR or A, etc.)
           Qclass : Unsigned_16; -- The class of the query (IN, etc.)
       end record;

   --  The request that we send, with a question, to the DNS server
   --  TODO: Name_Len is a discriminant for the buffer size and not part of the struct! How do we make it
   --  not be represented in the struct's data values?
   type Request_Type (Name_Len : Positive) is
       record
           Hdr   : Header_Type;
           Rtype : Unsigned_16;
           Class : Unsigned_16;
           Question : Question_Type (Name_Len);
       end record;

   --  The response we expect back
   type Response_Type (Name_Len : Positive) is
       record
           Hdr : Header_Type;

           --  Is this an overflow risk? We don't know the length of the response domain name before we get it.
           --  We may need to parse this section on the fly.
           Question : Question_Type (Name_Len);
           -- Answer : Answer_Type;
       end record;

   type Preamble_Type is
       record
           -- Name  : Label_Sequence; --  TODO
           Rtype : Unsigned_16;
           Class : Unsigned_16;
           TTL   : Unsigned_32;
           Len   : Unsigned_16;
       end record;

   --  Authoritative ("A") Record response
   type A_Record is
       record
           Preamble : Preamble_Type;
           IP : Unsigned_32;
       end record;

   function Resolve (Domain : String) return String;

private
   Last_Id : Unsigned_16 := 0;

   --  If the first two bits of the length field at set,
   --  we can expect a following byte (extended len)
   Extended_Len_Mask : Unsigned_16 := 16#C000#;

   --  Server_Address : GNAT.Sockets.Inet_Addr_Type := (Family_Inet, Sin_V4 : Inet_Addr_V4_Type)

end DNS;
