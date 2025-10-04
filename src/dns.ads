with Interfaces.C.Extensions; use Interfaces.C.Extensions;
with Ada.Streams; use Ada.Streams;

package DNS is

   type Query_Type is (Query, Response);
   for Query_Type use (Query => 0, Response => 1);
   for Query_Type'Size use 1; -- bit

   --  Maximum number of IP addresses we'll store
   Max_IPs : constant := 10;

   subtype IP_Count is Natural range 1 .. Max_IPs;
   type IP_Array is array (IP_Count) of String (1 .. 15);

   type Response_Status is (Ok, Error);
   --  Integer mapping
   for Response_Status use (
       Ok => 0,
       Error => 1
   );

   type DNS_Response is record
      IPs     : IP_Array;
      Count   : IP_Count;
      Status  : Response_Status;
   end record;

   --  DNS Request type
   type DNS_Request is record
      Buffer  : Stream_Element_Array (1 .. 512);
      Size    : Stream_Element_Offset;
   end record;

   --  Create a DNS request
   function Create_Request (Domain : String) return DNS_Request;

   --  Parse a DNS response
   procedure Parse_Response (Buffer : Stream_Element_Array;
                             Response : out DNS_Response);

   --  Resolve a domain name
   procedure Resolve (Domain : String; Result : out DNS_Response);

   -- function Resolve (Domain : String) return String;

private
   Query_Id : Unsigned_16 := 16#0F#;

end DNS;
