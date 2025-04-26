with Ada.Calendar;
with GNAT.Sockets;
with Log_Level;
with Log;

package body DNS is
   use Ada.Calendar;
   use GNAT.Sockets;
   
   package Logger is new Log (Local_Log_Level => Log_Level.Error);

   type DNS_Providers is (Google, Cloudflare);

   Server_Addresses : constant array (DNS_Providers) of Sock_Addr_Type := (
      Google => (Family => Family_Inet,
                Addr   => Inet_Addr ("8.8.8.8"),
                Port   => 53),
      Cloudflare => (Family => Family_Inet,
                     Addr   => Inet_Addr ("1.1.1.1"),
                     Port   => 53)
   );

   --  Create a DNS request
   procedure Create_Request (Domain : String; Request : out DNS_Request) is
   begin
      Logger.Debug ("Creating DNS request for domain: " & Domain);

      --  Increment the Query ID
      Query_Id := Query_Id + 1;

      --  Set up the DNS header
      Request.Buffer (1) := Stream_Element (Query_Id / 256);
      Request.Buffer (2) := Stream_Element (Query_Id mod 256);
      Request.Buffer (3) := 16#01#;  --  Standard query
      Request.Buffer (4) := 16#01#;  --  Recursion desired
      Request.Buffer (5) := 0;  --  QDCOUNT high byte
      Request.Buffer (6) := 1;  --  QDCOUNT low byte (1 question)
      Request.Buffer (7 .. 12) := (0, 0, 0, 0, 0, 0);  --  ANCOUNT, NSCOUNT, ARCOUNT

      --  Convert domain name to DNS format
      declare
         Pos : Stream_Element_Offset := 13;
         Start : Positive := Domain'First;
         Dot_Pos : Natural;
      begin
         loop
            Dot_Pos := Start;
            while Dot_Pos <= Domain'Last and then Domain (Dot_Pos) /= '.' loop
               Dot_Pos := Dot_Pos + 1;
            end loop;

            --  Write length of label
            Request.Buffer (Pos) := Stream_Element (Dot_Pos - Start);
            Pos := Pos + 1;

            --  Write label
            for I in Start .. Dot_Pos - 1 loop
               Request.Buffer (Pos) := Stream_Element'Val (Character'Pos (Domain (I)));
               Pos := Pos + 1;
            end loop;

            exit when Dot_Pos > Domain'Last;
            Start := Dot_Pos + 1;
         end loop;

         --  Terminating zero
         Request.Buffer (Pos) := 0;
         Pos := Pos + 1;

         --  Type A
         Request.Buffer (Pos) := 0;
         Request.Buffer (Pos + 1) := 1;

         --  Class IN
         Request.Buffer (Pos + 2) := 0;
         Request.Buffer (Pos + 3) := 1;

         Request.Size := Pos + 3;
         Logger.Debug ("Request size:" & Request.Size'Image & " bytes");
      end;
   end Create_Request;

   --  Skip a domain name in DNS message format
   function Skip_Name (Buffer : Stream_Element_Array; Start : Stream_Element_Offset) return Stream_Element_Offset is
      Pos : Stream_Element_Offset := Start;
      Length : Stream_Element := Buffer (Pos);
   begin
      loop
         exit when Pos > Buffer'Last;

         Length := Buffer (Pos);
         exit when Length = 0;

         --  Check for compression pointer
         if (Length and 16#C0#) = 16#C0# then
            return Pos + 2;  -- Skip the 2-byte pointer
         end if;

         --  Skip label
         Pos := Pos + Stream_Element_Offset (Length) + 1;
      end loop;
      return Pos + 1;  --  Skip the terminating zero
   end Skip_Name;

   --  Parse a DNS response
   procedure Parse_Response (Buffer : Stream_Element_Array; Response : out DNS_Response) is
      Pos : Stream_Element_Offset := 1;
      Current_Count : Natural := 0;
   begin
      Logger.Debug ("Parsing DNS response of" & Buffer'Length'Image & " bytes");

      --  Initialize response
      Response.Count := 0;
      Response.Status := Response_Ok;

      --  Check response size
      if Buffer'Length < 12 then
         Response.Status := Response_Error;
         Logger.Error ("Response too short");
         return;
      end if;

      --  Check response flags
      declare
         Flags : constant Stream_Element := Buffer (4);
         Rcode : constant Stream_Element := Flags and 16#0F#;
      begin
         if Rcode /= 0 then
            Response.Status := Response_Error;
            Logger.Error ("DNS response code:" & Rcode'Image);
            return;
         end if;
      end;

      --  Get answer count
      declare
         AnCount : constant Natural := Natural (Buffer (7)) * 256 + Natural (Buffer (8));
      begin
         Logger.Debug ("Number of answers:" & AnCount'Image);
         if AnCount = 0 then
            Response.Status := Response_Error;
            Logger.Error ("No answers in response");
            return;
         end if;
      end;

      --  Skip question section
      Pos := Skip_Name (Buffer, 13);
      Pos := Pos + 4;  --  Skip QTYPE and QCLASS

      --  Parse all answers
      for I in 1 .. Max_IPs loop
         exit when Pos + 10 > Buffer'Last;

         --  Skip name field (could be a pointer or full name)
         Pos := Skip_Name (Buffer, Pos);

         --  Check type (should be 1 for A record)
         if Buffer (Pos) = 0 and then Buffer (Pos + 1) = 1 then
            --  Skip class and TTL
            Pos := Pos + 8;

            --  Check data length (should be 4 for IPv4)
            declare
               Data_Length : constant Natural := Natural (Buffer (Pos)) * 256 + Natural (Buffer (Pos + 1));
            begin
               if Data_Length = 4 then
                  Pos := Pos + 2;

                  --  Convert IP address to string
                  declare
                     IP : String (1 .. 15);
                     IP_Len : Natural := 0;
                  begin
                     for J in 0 .. 3 loop
                        if IP_Len > 0 then
                           IP_Len := IP_Len + 1;
                           IP (IP_Len) := '.';
                        end if;
                        declare
                           Num : constant String := Natural'Image (Natural (Buffer (Pos + Stream_Element_Offset (J))));
                        begin
                           for C of Num (2 .. Num'Last) loop
                              IP_Len := IP_Len + 1;
                              IP (IP_Len) := C;
                           end loop;
                        end;
                     end loop;
                     Current_Count := Current_Count + 1;
                     Response.IPs (Current_Count) := (others => ' ');  --  Initialize with spaces
                     Response.IPs (Current_Count)(1 .. IP_Len) := IP (1 .. IP_Len);
                     Response.Count := IP_Count (Current_Count);
                     Logger.Debug ("Found IP address: " & IP (1 .. IP_Len));
                  end;
               else
                  Logger.Error ("Invalid data length for A record:" & Data_Length'Image);
               end if;
            end;
         else
            Logger.Error ("Not an A record");
         end if;

         --  Move to next answer
         Pos := Pos + 4;  --  Skip the IP address
      end loop;

      if Response.Count = 0 then
         Response.Status := Response_Error;
         Logger.Error ("Could not parse any answers");
      end if;
   end Parse_Response;

   --  Resolve a domain name
   procedure Resolve (Domain : String; Result : out DNS_Response) is
      Socket : Socket_Type;
      Addr   : Sock_Addr_Type;
      Buffer : Stream_Element_Array (1 .. 512);
      Last   : Stream_Element_Offset;
      Request : DNS_Request;
      Timeout : constant Duration := 5.0;  --  5 second timeout
      Start_Time : Time;
   begin
      Logger.Info ("Resolving domain: " & Domain);

      --  Create UDP socket
      Create_Socket (Socket, Family_Inet, Socket_Datagram);

      --  Set socket timeout
      Set_Socket_Option (Socket, Socket_Level, (Receive_Timeout, Timeout));

      --  Set up server address using Cloudflare
      Addr := Server_Addresses (Cloudflare);

      Logger.Debug ("Sending request to DNS server: " & Image (Addr.Addr) & ":" & Addr.Port'Image);

      --  Create and send request
      Create_Request (Domain, Request);
      Send_Socket (Socket, Request.Buffer (1 .. Request.Size), Last, Addr);

      Logger.Debug ("Sent" & Last'Image & " bytes");

      --  Record start time
      Start_Time := Clock;

      --  Try to receive response with timeout
      while Clock - Start_Time < Timeout loop
         begin
            Receive_Socket (Socket, Buffer, Last, Addr);
            Logger.Debug ("Received" & Last'Image & " bytes from " & Image (Addr.Addr));
            Parse_Response (Buffer (1 .. Last), Result);
            Close_Socket (Socket);
            return;
         exception
            when Socket_Error =>
               if Clock - Start_Time >= Timeout then
                  Close_Socket (Socket);
                  Logger.Error ("DNS request timed out");
                  raise Socket_Error with "DNS request timed out";
               end if;
               delay 0.1;  --  Small delay before retry
         end;
      end loop;

      Close_Socket (Socket);
      Result.Status := Response_Error;
      Result.Count := 0;
      Logger.Error ("DNS request failed");
   end Resolve;

   --  Legacy function for backward compatibility
   function Resolve (Domain : String) return String is
      Response : DNS_Response;
   begin
      Resolve (Domain, Response);
      if Response.Status = Response_Ok and then Response.Count > 0 then
         return Response.IPs (1);
      else
         return "Error resolving domain";
      end if;
   end Resolve;
end DNS;
