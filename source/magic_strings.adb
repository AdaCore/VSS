------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with Magic_Strings.Texts;

package body Magic_Strings is

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Magic_String) is
      Aux : String_Access;

   begin
      if Self.Data /= null then
         Aux := Self.Data.Reference;

         if Aux /= Self.Data then
            Self.Data := Aux;
         end if;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Magic_String) is
   begin
      if Self.Data /= null then
         Self.Data.Unreference;
         Self.Data := null;
      end if;
   end Finalize;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Magic_String'Class) return Boolean is
   begin
      return Self.Data = null or else Self.Data.Is_Empty;
   end Is_Empty;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Self   : out Magic_String) is
   begin
      raise Program_Error with "Not implemented";
   end Read;

   -------------------
   -- To_Magic_Text --
   -------------------

   function To_Magic_Text
     (Self : Magic_String) return Magic_Strings.Texts.Magic_Text is
   begin
      return (Ada.Finalization.Controlled with
                Data => (if Self.Data = null
                         then null
                         else Self.Data.To_Text));
   end To_Magic_Text;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Self   : Magic_String) is
   begin
      raise Program_Error with "Not implemented";
   end Write;

end Magic_Strings;
