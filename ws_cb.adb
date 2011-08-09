------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2009, AdaCore                     --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Exceptions;

with GNAT.Calendar.Time_IO;

with AWS.Config;
with AWS.OS_Lib;
with AWS.Messages;
with AWS.MIME;
with AWS.Parameters;
with AWS.Services.Directory;
with AWS.Server.Push;
with AWS.Translator;
with AWS.Utils;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Templates_Parser;
with Client;

package body WS_CB is

   use AWS;
   use Ada.Strings.Unbounded;
   use Ada.Calendar;
   use Ada.Containers;

   type Client_Env is record
      Start   : Time;
      Picture : Unbounded_String;
   end record;

   function "=" (A, B : Client.Object) return Boolean is
   begin
      return A.Get_Client_Id = B.Get_Client_Id;
   end;

   package Clients_Map is new Indefinite_Hashed_Maps (
                                                      Key_Type        => String,
                                                      Element_Type    => Client.Object,
                                                      Hash            => Ada.Strings.Hash,
                                                      Equivalent_Keys => "="
                                                     );
   use Clients_Map;


   package Messages_Container is new Indefinite_Vectors (
                                                         Index_Type => Natural,
                                                         Element_Type => String
                                                        );
   use Messages_Container;

   Clients  : Clients_Map.Map;
   Messages : Messages_Container.Vector;

   --  Simple ID generator

   function Js (Code : String) return String is
   begin
      return "<script type=""text/javascript"">" & Code & "</script>";
   end;

   protected New_Connection_Id is
      procedure Get (New_Id : out String);
   private
      Id : Natural := 0;
   end New_Connection_Id;
   --  The push data are generated here

   function To_Array
     (Str  : Unbounded_String;
      Env  : Client_Env) return Ada.Streams.Stream_Element_Array;

   package Chat is new AWS.Server.Push
     (Client_Output_Type => Unbounded_String,
      Client_Environment => Client_Env,
      To_Stream_Array    => To_Array);

   SP : Chat.Object;

   ---------
   -- Get --
   ---------

   function Get (Request : AWS.Status.Data) return AWS.Response.Data is
      URI      : constant String := AWS.Status.URI (Request);
      package Tpl renames Templates_Parser;
      T        : Tpl.Translate_Set;
      Html     : Unbounded_String;
   begin
      if URI = "/" then
         Html := To_Unbounded_String (Tpl.Parse ("index.html", T));
         return AWS.Response.Build ("text/html", Html);
      elsif URI = "/send" then
         declare
            P_List              : constant AWS.Parameters.List
              := AWS.Status.Parameters (Request);
            Picture             : Unbounded_String
              := To_Unbounded_String (AWS.Parameters.Get_Value (P_List));
            Msg                 : Unbounded_String;
            Client_Object_Index : Clients_Map.Cursor;
            Client_Object       : Client.Object;
         begin
            Append (Msg, AWS.Parameters.Get (P_List, "clientId"));
            Append (Msg, ": ");
            Append (Msg, AWS.Parameters.Get (P_List, "msg"));
            Append (Msg, "<br>");
            -- Add the message to all clients
            if Clients_Map.Length (Clients) /= 0 then
               Client_Object_Index := Clients.First;
               while Client_Object_Index /= Clients_Map.No_Element loop
                  Ada.Text_IO.Put_Line ("Boop");
                  Client_Object := Element (Client_Object_Index);
                  Client_Object.Add_Buffer (To_String (Msg));
                  Clients.Replace_Element (Client_Object_Index, Client_Object);
                  Client_Object_Index := Clients_Map.Next (Client_Object_Index);
               end loop;
            end if;
            return AWS.Response.Build ("text/html",
                                       To_Unbounded_String ("Good")
                                      );
         end;
      elsif URI = "/server_push" then

         declare
            use GNAT.Calendar;
            use GNAT.Calendar.Time_IO;
            use Ada.Calendar;

            P_List        : constant AWS.Parameters.List
              := AWS.Status.Parameters (Request);
            Picture       : Unbounded_String
              := To_Unbounded_String (AWS.Parameters.Get_Value (P_List));
            Client_Id     : String (1 .. 16);
            Connection_Id : String (1 .. 32);
            Session       : Client.Object;
            Session_Index : Clients_Map.Cursor;
         begin
            New_Connection_Id.Get (Connection_Id);
            Client_Id := AWS.Parameters.Get (P_List, "clientId");
            -- Make sure the client doesn't already exist
            if Clients.Contains (Key => Client_Id) then
               Session_Index := Clients.Find (Client_Id);
               Session := Clients_Map.Element (Session_Index);
               Session.Is_Connected := True;
               Clients.Replace_Element (Session_Index, Session);
            else
               -- Add the client to the Clients list
               Session.Set_Client_Id (Client_Id);
               Session.Set_Connection_Id (Connection_Id);
               Session.Is_Connected := True;
               Clients.Insert (
                               Client_Id, Session
                              );
            end if;

            Chat.Register (
                           Server      => SP,
                           Client_Id   => Connection_Id,
                           Socket      => AWS.Status.Socket (Request),
                           Environment => (Clock, Picture),
                           Kind        => Chat.Chunked
                          );

         end;

         return AWS.Response.Socket_Taken;
      else
         return AWS.Response.Acknowledge
           (AWS.Messages.S404,
            "<p>Page '" & URI & "' Not found.");
      end if;
   end Get;

   ---------
   -- Put --
   ---------

   function Put (Request : AWS.Status.Data) return AWS.Response.Data is
      pragma Unreferenced (Request);
   begin
      return AWS.Response.Acknowledge (Status_Code => AWS.Messages.S200);
   end Put;

   -------------
   -- Service --
   -------------

   function Service (Request : AWS.Status.Data) return AWS.Response.Data is
      use type AWS.Status.Request_Method;
   begin
      if AWS.Status.Method (Request) = AWS.Status.GET
        or else AWS.Status.Method (Request) = AWS.Status.POST
        or else AWS.Status.Method (Request) = AWS.Status.HEAD
      then
         return Get (Request);

      elsif AWS.Status.Method (Request) = AWS.Status.PUT then
         return Put (Request);

      else
         return AWS.Response.Acknowledge (Status_Code => AWS.Messages.S405);
      end if;

   exception
      when E : others =>
         return AWS.Response.Build
           (Content_Type => "text/plain",
            Status_Code  => AWS.Messages.S500,
            Message_Body => Ada.Exceptions.Exception_Information (E));
   end Service;

   ----------------------
   -- Stop_Push_Server --
   ----------------------

   procedure Stop_Push_Server is
   begin
      null;
      --abort Server_Push_Task;
   end Stop_Push_Server;

   --------------
   -- To_Array --
   --------------

   function To_Array
     (Str  : Unbounded_String;
      Env  : Client_Env) return Ada.Streams.Stream_Element_Array
   is
   begin
      return Translator.To_Stream_Element_Array (
                                                 To_String (Str)
                                                );
   end To_Array;

   procedure Check_Client (Client_Id : String) is
   begin
      -- This is wherre you would do clean up
      Clients.Delete (Client_Id);
      Ada.Text_IO.Put_Line ("Client Gone: " & Client_Id);
   end;

   ----------------------
   -- Server_Push_Task --
   ----------------------

   task body Server_Push_Task_Type is
      use GNAT.Calendar;
      use Client.Buffer_Container;
      Now                 : Ada.Calendar.Time;
      Msg                 : Unbounded_String;
      Data                : Unbounded_String;
      Index               : Client.Buffer_Container.Cursor;
      -- Client Stuff
      Client_Object_Index : Clients_Map.Cursor;
      Client_Object       : Client.Object;
      Connection_Id       : String (1 .. 32);
   begin
      accept Push;
      Now := Ada.Calendar.Clock;

      -- Because IE does not impliment partial responses we have to disconnect
      -- every connection and ask for a new one... fun!
      Client_Object_Index := Clients.First;
      while Client_Object_Index /= Clients_Map.No_Element loop
         Client_Object := Element (Client_Object_Index);
         Connection_Id := Client_Object.Get_Connection_Id;

         if Client_Object.Is_Connected = True then
            Data  := To_Unbounded_String ("");
            Msg   := To_Unbounded_String ("");
            Index := Client_Object.Buffer.First;

            --Ada.Text_IO.Put_Line (Client.Buffer_Container.Length (Client_Object.Buffer)'Img);
            while Index /= Client.Buffer_Container.No_Element loop
               Append (Msg, To_Unbounded_String (Element (Index)));
               Client_Object.Buffer.Delete (Index);
               Index := Client.Buffer_Container.Next (Index);
            end loop;
            -- TODO: the buffer doesn't seem to be deleted?
            --Ada.Text_IO.Put_Line (Length (Msg)'Img);
            if Length (Msg) /= 0 then
               -- TODO: must escape double quotes
               Append (Data, "document.getElementById(""messages"").innerHTML += """);
               Append (Data, Msg);
               Append (Data, """;");
               Ada.Text_IO.Put_Line (To_String (Data));
               Ada.Text_IO.Put_Line ("Connection: " & Connection_Id);
               -- Send to the client
               Ada.Text_IO.Put_Line ("Sending");
               Chat.Send_To (
                                  Server       => SP,
                                  Client_Id    => Connection_Id,
                                  Data         => Data,
                                  Content_Type => "text/html"
                                 );
               -- Force a disconnect, to allow them to reconnect for other
               -- messages
               Ada.Text_IO.Put_Line ("Disconnecting");
               Chat.Unregister (
                                     Server    => SP,
                                     Client_Id => Connection_Id
                                    );
               Ada.Text_IO.Put_Line ("Disconnected");
               Client_Object.Is_Connected := False;
               Clients.Replace_Element (Client_Object_Index, Client_Object);
            end if;

            Client_Object_Index := Clients_Map.Next (Client_Object_Index);
         end if;
      end loop;
   end Server_Push_Task_Type;

   -------------------
   -- New_Connection_Id --
   -------------------

   protected body New_Connection_Id is

      procedure Get (New_Id : out String) is
      begin
         Id := Id + 1;
         Ada.Integer_Text_IO.Put (New_Id, Id);
      end Get;

   end New_Connection_Id;

end WS_CB;
