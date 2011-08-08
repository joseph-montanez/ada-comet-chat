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
   
   function "="(a, b: Client.Object) return Boolean is
   begin
      return a.Get_Id = b.Get_Id;
   end;
   
   
   package String_Client_Maps is new Ada.Containers.Indefinite_Hashed_Maps (
      Key_Type        => String,
      Element_Type    => Client.Object,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "="
   );
   use String_Client_Maps;
   
   Connections : String_Client_Maps.Map;
   Messages    : Client.Buffer_Container;

   --  Simple ID generator
   
   function js (code : String) return String is
   begin
      return "<script type=""text/javascript"">" & code & "</script>";
   end;

   protected New_Client_Id is
      procedure Get (New_Id : out String);
   private
      Id : Natural := 0;
   end New_Client_Id;
   --  The push data are generated here

   function To_Array
     (Str  : Unbounded_String;
      Env  : Client_Env) return Ada.Streams.Stream_Element_Array;

   package Chat_Push is new AWS.Server.Push
     (Client_Output_Type => Unbounded_String,
      Client_Environment => Client_Env,
      To_Stream_Array    => To_Array);

   SP : Chat_Push.Object;

   ---------
   -- Get --
   ---------

   function Get (Request : AWS.Status.Data) return AWS.Response.Data is
      URI      : constant String := AWS.Status.URI (Request);
      package Tpl renames Templates_Parser;
      T : Tpl.Translate_Set;
      Html : Unbounded_String;
   begin
      if URI = "/" then
         Html := To_Unbounded_String(Tpl.Parse ("index.html", T));
         return AWS.Response.Build ("text/html", Html);
      elsif URI = "/send" then
         declare
            P_List    : constant AWS.Parameters.List
              := AWS.Status.Parameters (Request);
            Picture   : Unbounded_String
              := To_Unbounded_String (AWS.Parameters.Get_Value (P_List));
            Msg       : Unbounded_String;
         begin
            Msg := To_Unbounded_String (AWS.Parameters.Get (P_List, "msg"));
            Messages.Append (Msg);
            return AWS.Response.Acknowledge
              (Messages.S404,
               "<p>Page '" & URI & "' Not found.");
         end;
      elsif URI = "/server_push" then

         declare
            use GNAT.Calendar;
            use GNAT.Calendar.Time_IO;
            use Ada.Calendar;
            
            P_List    : constant AWS.Parameters.List
              := AWS.Status.Parameters (Request);
            Picture   : Unbounded_String
              := To_Unbounded_String (AWS.Parameters.Get_Value (P_List));
            Client_Id : String (1 .. 32);
            Session   : Client.Object;
         begin
            New_Client_Id.Get (Client_Id);
            
            -- Add the client to the connections list
            Session.Set_Id (Client_Id);
            Session.Set_ConnectionId (AWS.Parameters.Get (P_List, "id"));
            Connections.Insert (
               Client_Id, Session
            );

            Chat_Push.Register
              (Server      => SP,
               Client_Id   => Client_Id,
               Socket      => AWS.Status.Socket (Request),
               Environment => (Clock, Picture),
               Kind        => Chat_Push.Chunked);
               
            -- Send back their connection id
            Chat_Push.Send_To (
               Server => SP,
               Client_Id => Client_Id,
               Data => To_Unbounded_String( 
                  js ("setClientId('" & Client_Id & "');")
               )
            );
            
         end;

         return AWS.Response.Socket_Taken;
      else
         return AWS.Response.Acknowledge
           (Messages.S404,
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
         return AWS.Response.Acknowledge (Status_Code => Messages.S405);
      end if;

   exception
      when E : others =>
         return AWS.Response.Build
           (Content_Type => "text/plain",
            Status_Code => AWS.Messages.S500,
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
      Connections.Delete (Client_Id);
      Ada.Text_IO.Put_Line ("Client Gone: " & Client_Id);
   end;

   ----------------------
   -- Server_Push_Task --
   ----------------------

   task body Server_Push_Task_Type is
      use GNAT.Calendar;
      Now : Ada.Calendar.Time;
      Msg : Unbounded_String;
   begin
      accept Push;
      Now := Ada.Calendar.Clock;
      Ada.Text_IO.Put_Line ("Tick " & Second (Now)'Img & " " & Chat_Push.Count(SP)'Img);
      -- This sends to all clients;
      Index := Messages.First;
      while Index /= No_Element loop
         Msg.Append (Element(Index));
         Messages.Delete(Index);
         Index := Next (Index);
      end loop;
      Messages.
      Chat_Push.Send (
         Server => SP, 
         Data => To_Unbounded_String (js (
            "document.getElementById(""writer"").innerHTML = """ &
            "Connections: " & Connections.Length'Img & "<br>" &
            "<br>Ping!<br>" & Second (Now)'Img & """;"
         )), 
         Content_Type => "text/html",
         Client_Gone => Check_Client'Access
      );
   end Server_Push_Task_Type;

   -------------------
   -- New_Client_ID --
   -------------------

   protected body New_Client_Id is

      procedure Get (New_Id : out String) is
      begin
         Id := Id + 1;
         Ada.Integer_Text_IO.Put (New_Id, Id);
      end Get;

   end New_Client_Id;

end WS_CB;
