------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with VSS.Implementation.String_Configuration;
with VSS.Implementation.Null_String_Handlers;

package body VSS.Implementation.Strings is

   use type VSS.Unicode.UTF16_Code_Unit_Offset;
   use type VSS.Unicode.UTF8_Code_Unit_Offset;

   ---------
   -- "=" --
   ---------

   overriding function "="
     (Left  : String_Data;
      Right : String_Data) return Boolean is
   begin
      return
        VSS.Implementation.Strings.Handler (Left).Is_Equal
          (Left, VSS.Implementation.Strings.Handler (Right).all, Right);
   end "=";

   ------------------
   -- Fixup_Delete --
   ------------------

   function Fixup_Delete
     (Self  : in out Cursor;
      Start : Cursor;
      Size  : Cursor_Offset) return Boolean is
   begin
      if Size.Index_Offset = 0 then
         return True;
      end if;

      if Self.Index < Start.Index then
         --  Cursor's position is before deleted segment, only negative UTF*
         --  offsets need to be fixed.

         if Self.UTF8_Offset < 0 then
            Self.UTF8_Offset := Self.UTF8_Offset + Size.UTF8_Offset;
         end if;

         if Self.UTF16_Offset < 0 then
            Self.UTF16_Offset := Self.UTF16_Offset + Size.UTF16_Offset;
         end if;

         return True;

      elsif Self.Index < Start.Index + Size.Index_Offset then
         --  Cursor's position is inside deleted segment, invalidate position.

         Self := (others => <>);

         return False;

      else
         --  Cursor's position is after deleted segment, cursor's index and
         --  positive UTF* offset need to be fixed.

         Self.Index := Self.Index - Size.Index_Offset;

         if Self.UTF8_Offset >= 0 then
            Self.UTF8_Offset := Self.UTF8_Offset - Size.UTF8_Offset;
         end if;

         if Self.UTF16_Offset >= 0 then
            Self.UTF16_Offset := Self.UTF16_Offset - Size.UTF16_Offset;
         end if;

         return True;
      end if;
   end Fixup_Delete;

   ------------------
   -- Fixup_Insert --
   ------------------

   procedure Fixup_Insert
     (Self  : in out Cursor;
      Start : Cursor;
      Size  : Cursor_Offset) is
   begin
      if Size.Index_Offset = 0 then
         return;
      end if;

      if Self.Index < Start.Index then
         --  Cursor's position is before insertion point, only negative UTF*
         --  offsets need to be fixed.

         if Self.UTF8_Offset < 0 then
            Self.UTF8_Offset := Self.UTF8_Offset - Size.UTF8_Offset;
         end if;

         if Self.UTF16_Offset < 0 then
            Self.UTF16_Offset := Self.UTF16_Offset - Size.UTF16_Offset;
         end if;

      else
         --  Cursor's position is at or after insertion point, index of the
         --  cursor and positive UTF* offsets need to be fixed.

         Self.Index := Self.Index + Size.Index_Offset;

         if Self.UTF8_Offset >= 0 then
            Self.UTF8_Offset := Self.UTF8_Offset + Size.UTF8_Offset;
         end if;

         if Self.UTF16_Offset >= 0 then
            Self.UTF16_Offset := Self.UTF16_Offset + Size.UTF16_Offset;
         end if;
      end if;
   end Fixup_Insert;

   -------------
   -- Handler --
   -------------

   function Handler
     (Data : String_Data)
      return not null VSS.Implementation.Strings.String_Handler_Access is
   begin
      if Data.In_Place then
         return VSS.Implementation.String_Configuration.In_Place_Handler;

      elsif Data.Handler /= null then
         return Data.Handler;

      else
         return
           VSS.Implementation.Null_String_Handlers
             .Global_Null_String_Handler'Access;
      end if;
   end Handler;

   ----------------
   -- Is_Invalid --
   ----------------

   function Is_Invalid (Self : Cursor) return Boolean is
   begin
      return
        Self.Index = 0
          and Self.UTF8_Offset = VSS.Unicode.UTF8_Code_Unit_Offset'Last
          and Self.UTF16_Offset = VSS.Unicode.UTF16_Code_Unit_Offset'Last;
   end Is_Invalid;

   ---------------
   -- Reference --
   ---------------

   procedure Reference (Data : in out String_Data) is
   begin
      VSS.Implementation.Strings.Handler (Data).Reference (Data);
   end Reference;

   -----------------
   -- Unreference --
   -----------------

   procedure Unreference (Data : in out String_Data) is
   begin
      VSS.Implementation.Strings.Handler (Data).Unreference (Data);
      Data := Null_String_Data;
   end Unreference;

end VSS.Implementation.Strings;
