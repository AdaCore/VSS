------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

package body Magic.JSON.Streams.Writers is

   use type Magic.Streams.Output_Text_Stream_Access;

   ------------------
   -- End_Document --
   ------------------

   overriding procedure End_Document
     (Self : in out JSON_Writer; Success : in out Boolean) is
   begin
      if Self.Effective_Stream = null then
         Success := False;

      else
         Self.Effective_Stream := null;
      end if;
   end End_Document;

   ----------------
   -- Set_Stream --
   ----------------

   procedure Set_Stream
     (Self   : in out JSON_Writer'Class;
      Stream : not null Magic.Streams.Output_Text_Stream_Access) is
   begin
      Self.Configured_Stream := Stream;
   end Set_Stream;

   --------------------
   -- Start_Document --
   --------------------

   overriding procedure Start_Document
     (Self : in out JSON_Writer; Success : in out Boolean) is
   begin
      if Self.Effective_Stream /= null then
         Success := False;

      elsif Self.Configured_Stream /= null then
         Success := False;

      else
         Self.Effective_Stream := Self.Configured_Stream;
      end if;
   end Start_Document;

end Magic.JSON.Streams.Writers;
