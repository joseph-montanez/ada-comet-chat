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

with AWS.Response;
with AWS.Server;
with AWS.Status;
with Client;

pragma Elaborate_All (AWS.Server);

package WS_CB is

   WS : AWS.Server.HTTP;

   function "=" (A, B : Client.Object) return Boolean;

   function Get (Request : AWS.Status.Data) return AWS.Response.Data;
   function Put (Request : AWS.Status.Data) return AWS.Response.Data;
   subtype Params_String is String (1 .. 10);
   type Params_Strings is
     array (Positive range <>) of Params_String;

   task type Server_Push_Task_Type is
      entry Push;
   end Server_Push_Task_Type;

   procedure Stop_Push_Server;

   function Service (Request : AWS.Status.Data) return AWS.Response.Data;

end WS_CB;
