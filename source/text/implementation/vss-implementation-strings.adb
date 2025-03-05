--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with System.Address_To_Access_Conversions;

with VSS.Implementation.Text_Handlers;
with VSS.Implementation.Text_Handlers.Nul;

package body VSS.Implementation.Strings is

   use type System.Storage_Elements.Integer_Address;

   package Address_To_Text_Handler_Conversions is
     new System.Address_To_Access_Conversions
       (VSS.Implementation.Text_Handlers.Abstract_Text_Handler'Class);

   function Is_Initialized (Data : String_Data) return Boolean
     with Inline_Always;
   --  Return True when text data is initialized, and False overwise.

   procedure Unsafe_Initialize (Data : in out String_Data)
     with Pre => not Is_Initialized (Data);
   --  Initialize object to be null text.

   Global_Null_Handler : aliased
     VSS.Implementation.Text_Handlers.Nul.Null_Handler;
   --  Global null text handler object to be used to process uninitialized
   --  string data.

   function Internal_Variable_Handler
     (Data : in out String_Data) return not null Variable_Text_Handler_Access
        with Inline_Always;
   --  Like `Variable_Handler`, but storage must be initialized.

   ---------
   -- "=" --
   ---------

   overriding function "="
     (Left  : String_Data;
      Right : String_Data) return Boolean is
   begin
      return
        VSS.Implementation.Strings.Constant_Handler (Left).Is_Equal
          (VSS.Implementation.Strings.Constant_Handler (Right).all);
   end "=";

   ----------------------
   -- Constant_Handler --
   ----------------------

   function Constant_Handler
     (Data : String_Data) return not null Constant_Text_Handler_Access is
   begin
      if not Is_Initialized (Data) then
         return Global_Null_Handler'Access;

      else
         return
           Constant_Text_Handler_Access
             (Address_To_Text_Handler_Conversions.To_Pointer
                (Data.Storage'Address));
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

   -------------------------------
   -- Internal_Variable_Handler --
   -------------------------------

   function Internal_Variable_Handler
     (Data : in out String_Data)
      return not null Variable_Text_Handler_Access is
   begin
      return
        Variable_Text_Handler_Access
          (Address_To_Text_Handler_Conversions.To_Pointer
             (Data.Storage'Address));
   end Internal_Variable_Handler;

   --------------------
   -- Is_Initialized --
   --------------------

   function Is_Initialized (Data : String_Data) return Boolean is
      Tag : System.Storage_Elements.Integer_Address
        with Import, Convention => Ada, Address => Data.Storage'Address;

   begin
      return Tag /= 0;
   end Is_Initialized;

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
      if Is_Initialized (Data) then
         Internal_Variable_Handler (Data).Reference;
      end if;
   end Reference;

   -----------------
   -- Unreference --
   -----------------

   procedure Unreference (Data : in out String_Data) is
   begin
      if Is_Initialized (Data) then
         Internal_Variable_Handler (Data).Unreference;
         Data := Null_String_Data;
      end if;
   end Unreference;

   -----------------------
   -- Unsafe_Initialize --
   -----------------------

   procedure Unsafe_Initialize (Data : in out String_Data) is
   begin
      declare
         pragma Warnings (Off, """others"" choice is redundant");
         Overlay : Text_Handlers.Nul.Null_Handler := (others => <>)
           with Address => Data.Storage'Address;
         pragma Assert (Data.Storage'Size = Overlay'Size);
         pragma Warnings (On, """others"" choice is redundant");

      begin
         null;
      end;
   end Unsafe_Initialize;

   ----------------------
   -- Variable_Handler --
   ----------------------

   function Variable_Handler
     (Data : in out String_Data)
      return not null Variable_Text_Handler_Access is
   begin
      if not Is_Initialized (Data) then
         Unsafe_Initialize (Data);
      end if;

      return Internal_Variable_Handler (Data);
   end Variable_Handler;

end VSS.Implementation.Strings;
