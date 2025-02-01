with Ada.Text_IO;
with GNAT.Sockets; use GNAT.Sockets;
with Ada.Streams; use Ada.Streams;

package body DNS is

   Server_Address : constant Sock_Addr_Type :=
        (Family => Family_Inet,
        Addr => Inet_Addr ("8.8.8.8"),
        Port => 53);

   function Resolve (Domain : String) return String is
      Socket : GNAT.Sockets.Socket_Type;
      Receive_Addr : Sock_Addr_Type;

      Request : Request_Type := (
          Name_Len => Domain'Length, -- Disciminant for buffer size

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
               QdCount => 1, -- 1 Question
               AnCount => 0,
               NsCount => 0,
               ArCount => 0
               ),

           Rtype => 0,
           Class => 0,

           Question => (
               Name_Len => Domain'Length, -- Disciminant for buffer size
               Qname => Domain,
               Qtype => 1, -- A
               Qclass => 1 -- IN
               )
       );

      --  Stream the Question packet: requires us to create stream buffer, which is best done with a subtype
      --  that is an array of Stream_Element. We then use the 'Address attribute to get the address of
      --  Question, and use that to set the address of the subtype.
      subtype Request_Buffer_Type is Stream_Element_Array (1 .. Request'Size / Stream_Element'Size);
      Request_Length : Stream_Element_Offset := 0;
      Request_Buffer : Request_Buffer_Type;
      for Request_Buffer'Address use Request'Address;

      --  Answer result
      Response : Response_Type := (Name_Len => Domain'Length, others => <>);
      subtype Response_Buffer_Type is Stream_Element_Array (1 .. Response'Size / Stream_Element'Size);
      --  subtype Response_Buffer_Type is Stream_Element_Array (1 .. 512);
      Response_Length : Stream_Element_Offset := 0;
      Response_Buffer : Response_Buffer_Type;
      for Response_Buffer'Address use Response'Address;

   begin
      Last_Id := Last_Id + 1;

      -- Check if the request is too large. If it is, set the Truncated bit. For now, also raise an exception since we
      -- don't support requests that large.
      if Request'Size > 512 * System.Storage_Unit then
          Request.Hdr.Tc := 1;
          raise Program_Error with "Request too large";
      end if;

      Create_Socket (Socket, Family_Inet, Socket_Datagram);

      GNAT.Sockets.Send_Socket (Socket, Request_Buffer, Request_Length, Server_Address);
      Ada.Text_IO.Put_Line ("Sent: " & Request_Length'Img & " bytes");
      Ada.Text_IO.Put_Line ("Question" & Request'Img);
      for I in 1 .. Request_Length loop
         Ada.Text_IO.Put (Request_Buffer (I)'Img & "");
      end loop;

      Ada.Text_IO.New_Line;
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
