--
--  Copyright (C) 2020-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

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
        VSS.Implementation.Strings.Constant_Handler (Left).Is_Equal
          (Left,
           VSS.Implementation.Strings.Constant_Handler (Right).all,
           Right);
   end "=";

   ----------------------
   -- Constant_Handler --
   ----------------------

   function Constant_Handler
     (Data : String_Data) return not null Constant_Text_Handler_Access is
   begin
      if Data.In_Place then
         return
           Constant_Text_Handler_Access
             (VSS.Implementation.String_Configuration.In_Place_Handler);

      elsif Data.Handler /= null then
         return Constant_Text_Handler_Access (Data.Handler);

      else
         return
           VSS.Implementation.Null_String_Handlers
             .Global_Null_String_Handler'Access;
      end if;
   end Constant_Handler;

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
      Handler : Variable_Text_Handler_Access := Variable_Handler (Data);

   begin
      Handler.Reference (Data);
   end Reference;

   -----------------
   -- Unreference --
   -----------------

   procedure Unreference (Data : in out String_Data) is
      Handler : Variable_Text_Handler_Access := Variable_Handler (Data);

   begin
      Handler.Unreference (Data);
      Data := Null_String_Data;
   end Unreference;

   ----------------------
   -- Variable_Handler --
   ----------------------

   function Variable_Handler
     (Data : in out String_Data)
      return not null Variable_Text_Handler_Access is
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
   end Variable_Handler;

end VSS.Implementation.Strings;
