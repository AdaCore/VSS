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

package body VSS.Implementation.Strings is

   ---------
   -- "=" --
   ---------

   overriding function "="
     (Left  : String_Data;
      Right : String_Data) return Boolean
   is
      Left_Handler  : constant
        VSS.Implementation.Strings.String_Handler_Access :=
          Handler (Left);
      Right_Handler : constant
        VSS.Implementation.Strings.String_Handler_Access :=
          Handler (Right);

   begin
      if Left_Handler = null and Right_Handler = null then
         return True;

      elsif Left_Handler = null xor Right_Handler = null then
         return Is_Empty (Left) and Is_Empty (Right);

      else
         return
           Left_Handler.Is_Equal (Left, Right_Handler.all, Right);
      end if;
   end "=";

   -------------
   -- Handler --
   -------------

   function Handler
     (Data : String_Data)
      return VSS.Implementation.Strings.String_Handler_Access is
   begin
      if Data.In_Place then
         return VSS.Implementation.String_Configuration.In_Place_Handler;

      else
         return Data.Handler;
      end if;
   end Handler;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : String_Data) return Boolean is
      Handler : constant VSS.Implementation.Strings.String_Handler_Access :=
        VSS.Implementation.Strings.Handler (Self);

   begin
      return Handler = null or else Handler.Is_Empty (Self);
   end Is_Empty;

   ----------------
   -- Is_Invalid --
   ----------------

   function Is_Invalid (Self : Cursor) return Boolean is
      use type VSS.Unicode.UTF16_Code_Unit_Offset;
      use type VSS.Unicode.UTF8_Code_Unit_Offset;

   begin
      return
        Self.Index = 0
          and Self.UTF8_Offset = VSS.Unicode.UTF8_Code_Unit_Offset'Last
          and Self.UTF16_Offset = VSS.Unicode.UTF16_Code_Unit_Offset'Last;
   end Is_Invalid;

   ----------
   -- Move --
   ----------

   procedure Move
     (Self   : in out Cursor;
      Offset : Cursor_Offset)
   is
      use type VSS.Unicode.UTF16_Code_Unit_Offset;
      use type VSS.Unicode.UTF8_Code_Unit_Offset;

   begin
      Self.Index        := Self.Index + Offset.Index_Offset;
      Self.UTF8_Offset  := Self.UTF8_Offset + Offset.UTF8_Offset;
      Self.UTF16_Offset := Self.UTF16_Offset + Offset.UTF16_Offset;
   end Move;

   ---------------
   -- Reference --
   ---------------

   procedure Reference (Data : in out String_Data) is
      Handler : constant VSS.Implementation.Strings.String_Handler_Access :=
        VSS.Implementation.Strings.Handler (Data);

   begin
      if Handler /= null then
         Handler.Reference (Data);
      end if;
   end Reference;

   -----------------
   -- Unreference --
   -----------------

   procedure Unreference (Data : in out String_Data) is
      Handler : constant VSS.Implementation.Strings.String_Handler_Access :=
        VSS.Implementation.Strings.Handler (Data);

   begin
      if Handler /= null then
         Handler.Unreference (Data);
      end if;

      Data := Null_String_Data;
   end Unreference;

end VSS.Implementation.Strings;
