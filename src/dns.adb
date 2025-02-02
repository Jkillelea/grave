with Ada.Text_IO;
with GNAT.Sockets; use GNAT.Sockets;
with Ada.Streams; use Ada.Streams;

package body DNS is

   Server_Address : constant Sock_Addr_Type :=
        (Family => Family_Inet,
        Addr => Inet_Addr ("8.8.8.8"),
        Port => 53);

   type Packet_Buffer (Size : Stream_Element_Offset) is
       record
           Buffer : Stream_Element_Array (1 .. Size) := (others => 0);
       end record;

   function Pack_Request (Request : Request_Type) return Packet_Buffer is
      Result : Packet_Buffer (Request'Size / Stream_Element'Size);

      --  Encode the different fields as byte arrays
      type Header_Bytes_Type is array (1 .. Header_Type'Size / Stream_Element'Size) of Stream_Element;
      Header_Bytes : Header_Bytes_Type;
      for Header_Bytes'Address use Request.Hdr'Address;

      type Rtype_Bytes_Type is array (1 .. 2) of Stream_Element;
      Rtype_Bytes : Rtype_Bytes_Type;
      for Rtype_Bytes'Address use Request.Rtype'Address;

      type Class_Bytes_Type is array (1 .. 2) of Stream_Element;
      Class_Bytes : Class_Bytes_Type;
      for Class_Bytes'Address use Request.Class'Address;

      Idx : Stream_Element_Offset := 1;
   begin
      --  Pack header
      for Byte of Header_Bytes loop
         Result.Buffer (Idx) := Byte;
         Idx := Idx + 1;
      end loop;

      -- Pack Rtype
      for Byte of Rtype_Bytes loop
         Result.Buffer (Idx) := Byte;
         Idx := Idx + 1;
      end loop;

      -- Pack Class
      for Byte of Class_Bytes loop
         Result.Buffer (Idx) := Byte;
         Idx := Idx + 1;
      end loop;

      -- Pack Question Name
      for C of Request.Question.Qname loop
         Result.Buffer (Idx) := Stream_Element (Character'Pos (C));
         Idx := Idx + 1;
      end loop;


      return Result;
   end Pack_Request;

   function Resolve (Domain : String) return String is
      Socket : GNAT.Sockets.Socket_Type;
      Receive_Addr : Sock_Addr_Type;

      Request : Request_Type := (
           Name_Len => Domain'Length,
           Hdr => (
               Id => Last_Id,
               Qr => Query,
               Opcode => 0,
               Aa => 0,
               Tc => 0,
               Rd => 0,
               Ra => 0,
               Z => 0,
               Rcode => 0,
               QdCount => 1,
               AnCount => 0,
               NsCount => 0,
               ArCount => 0
               ),
           Rtype => 0,
           Class => 0,
           Question => (
               Name_Len => Domain'Length,
               Qname => Domain,
               Qtype => 1, -- A
               Qclass => 1 -- IN
               )
       );

      --  --  Stream the Question packet: requires us to create stream buffer, which is best done with a subtype
      --  --  that is an array of Stream_Element. We then use the 'Address attribute to get the address of
      --  --  Question, and use that to set the address of the subtype.
      --  subtype Request_Buffer_Type is Stream_Element_Array (1 .. Request'Size / Stream_Element'Size);
      --  Request_Buffer : Request_Buffer_Type;
      --  for Request_Buffer'Address use Request'Address;
      Request_Length : Stream_Element_Offset := Request'Size;
      Request_Buffer : Packet_Buffer := Pack_Request (Request);

      --  Answer result
      Response : Response_Type := (Name_Len => Domain'Length, others => <>);
      subtype Response_Buffer_Type is Stream_Element_Array (1 .. Response'Size / Stream_Element'Size);
      --  subtype Response_Buffer_Type is Stream_Element_Array (1 .. 512);
      Response_Length : Stream_Element_Offset := 0;
      Response_Buffer : Response_Buffer_Type;
      for Response_Buffer'Address use Response'Address;

   begin
      Last_Id := Last_Id + 1;

      --  Check if the request is too large. If it is, set the Truncated bit. For now, also raise an exception since we
      --  don't support requests that large.
      if Request'Size > 512 * System.Storage_Unit then
         Request.Hdr.Tc := 1;
         raise Program_Error with "Request too large";
      end if;

      Create_Socket (Socket, Family_Inet, Socket_Datagram);

      GNAT.Sockets.Send_Socket (Socket, Request_Buffer.Buffer, Request_Length, Server_Address);
      Ada.Text_IO.Put_Line ("Sent: " & Request_Length'Img & " bytes");
      Ada.Text_IO.Put_Line ("Question" & Request'Img);
      for I in 1 .. Request_Length loop
         Ada.Text_IO.Put (Request_Buffer.Buffer (I)'Img & "");
      end loop;

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("====================================");
      Ada.Text_IO.New_Line;

      GNAT.Sockets.Receive_Socket (Socket, Response_Buffer, Response_Length, Receive_Addr);

      Ada.Text_IO.Put_Line ("Received: " & Response_Length'Img & " bytes");
      Ada.Text_IO.Put_Line ("From: " & Image (Receive_Addr.Addr));
      for I in 1 .. Response_Length loop
         Ada.Text_IO.Put (Response_Buffer (I)'Img & "");
      end loop;

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Received" & Response'Img);

      return Domain;
   end Resolve;
end DNS;
