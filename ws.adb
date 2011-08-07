------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
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

--  This is a way to build a simple HTML page server. Just a part of a full
--  Web server.

with Ada.Text_IO;

with AWS.Server;
with AWS.Config;

with WS_CB;

procedure WS is

   use Ada;
   Conf : constant AWS.Config.Object := AWS.Config.Get_Current;
   task type Monitor_Push_Task is
      entry Start;
   end Monitor_Push_Task;
   
   task body Monitor_Push_Task is
   begin
      accept Start;

      loop
         declare
            Server_Push_Task : WS_CB.Server_Push_Task_Type;
         begin
            delay 1.0;
            Text_IO.Put_Line ("Tock");
            select
               Server_Push_Task.Push;
            or
               delay 1.0;
               Text_IO.Put_Line ("Too busy");
            end select;
         end;
      end loop;
   end Monitor_Push_Task;
   
   Monitor : Monitor_Push_Task;
begin
   Text_IO.Put_Line ("AWS " & AWS.Version);

   AWS.Server.Start (
                     Web_Server => WS_CB.WS,
                     Callback   => WS_CB.Service'Access,
                     Config     => Conf);

   Text_IO.Put_Line ("Press Q to terminate.");
   
   Monitor.Start;
   
   AWS.Server.Wait (AWS.Server.Q_Key_Pressed);

   AWS.Server.Shutdown (WS_CB.WS);
   WS_CB.Stop_Push_Server;
end WS;
