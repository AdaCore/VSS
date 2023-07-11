--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Characters;

package body VSS.JSON.Implementation.Parsers is

   ------------
   -- At_End --
   ------------

   function At_End (Self : JSON_Parser_Base'Class) return Boolean is
      use type VSS.JSON.Pull_Readers.JSON_Reader_Error;

   begin
      return
        Self.Stack.Is_Empty and
          (Self.Stream.Is_End_Of_Stream
             or Self.Error = VSS.JSON.Pull_Readers.Not_Valid);
   end At_End;

   -------------------
   -- Boolean_Value --
   -------------------

   function Boolean_Value (Self : JSON_Parser_Base'Class) return Boolean is
   begin
      return Self.Boolean;
   end Boolean_Value;

   ------------------
   -- Element_Kind --
   ------------------

   function Element_Kind
     (Self : JSON_Parser_Base'Class)
      return VSS.JSON.Streams.JSON_Stream_Element_Kind is
   begin
      return Self.Event;
   end Element_Kind;

   -----------
   -- Error --
   -----------

   function Error
     (Self : JSON_Parser_Base'Class)
      return VSS.JSON.Pull_Readers.JSON_Reader_Error is
   begin
      return Self.Error;
   end Error;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message
     (Self : JSON_Parser_Base'Class) return VSS.Strings.Virtual_String is
   begin
      return Self.Message;
   end Error_Message;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Parse_Stack'Class) return Boolean is
   begin
      return Self.Head = 0;
   end Is_Empty;

   ------------------
   -- Number_Value --
   ------------------

   function Number_Value
     (Self : JSON_Parser_Base'Class) return VSS.JSON.JSON_Number is
   begin
      return Self.Number;
   end Number_Value;

   ---------
   -- Pop --
   ---------

   procedure Pop (Self : in out Parse_Stack'Class) is
   begin
      Self.Head := Self.Head - 1;
   end Pop;

   ----------
   -- Push --
   ----------

   function Push
     (Self  : in out JSON_Parser_Base'Class;
      Parse : not null Parse_Subprogram;
      State : Interfaces.Unsigned_32) return Boolean
   is
      use type VSS.JSON.Streams.JSON_Stream_Element_Kind;
      use type VSS.JSON.Pull_Readers.JSON_Reader_Error;

   begin
      if Self.Event /= VSS.JSON.Streams.Invalid
        or else Self.Error /= VSS.JSON.Pull_Readers.Not_Valid
      then
         Self.Stack.Push (Parse, State);
      end if;

      return False;
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push
     (Self  : in out Parse_Stack'Class;
      Parse : not null Parse_Subprogram;
      State : Interfaces.Unsigned_32) is
   begin
      Self.Head := Self.Head + 1;
      Self.Stack (Self.Head) := (Parse, State);
   end Push;

   ----------
   -- Read --
   ----------

   function Read
     (Self  : in out JSON_Parser_Base'Class;
      Parse : not null Parse_Subprogram;
      State : Interfaces.Unsigned_32) return Boolean
   is
      Success   : Boolean := True;
      Character : VSS.Characters.Virtual_Character;

   begin
      Self.Stream.Get (Character, Success);

      if not Success then
         if Self.Stream.Is_End_Of_Stream then
            Self.C := End_Of_Stream;
         end if;

         if Self.Stream.Has_Error then
            --  In case of IO error save error message and mark document as
            --  invalid.

            Self.Message := Self.Stream.Error_Message;
            Self.Event   := VSS.JSON.Streams.Invalid;
            Self.Error   := VSS.JSON.Pull_Readers.Not_Valid;

            return False;

         else
            Self.Event := VSS.JSON.Streams.Invalid;
            Self.Error := VSS.JSON.Pull_Readers.Premature_End_Of_Document;
         end if;

         if not Self.Stream.Is_End_Of_Stream then
            Success := Self.Push (Parse, State);
         end if;

         return False;

      else
         Self.C := Wide_Wide_Character (Character);
      end if;

      return True;
   end Read;

   ------------------
   -- Report_Error --
   ------------------

   function Report_Error
     (Self    : in out JSON_Parser_Base'Class;
      Message : Wide_Wide_String) return Boolean is
   begin
      Self.Event := VSS.JSON.Streams.Invalid;
      Self.Error := VSS.JSON.Pull_Readers.Not_Valid;
      Self.Message := VSS.Strings.To_Virtual_String (Message);

      return False;
   end Report_Error;

   ----------------
   -- Set_Stream --
   ----------------

   procedure Set_Stream
     (Self   : in out JSON_Parser_Base'Class;
      Stream : not null VSS.Text_Streams.Input_Text_Stream_Access) is
   begin
      Self.Stream := Stream;
   end Set_Stream;

   ---------------------
   -- Store_Character --
   ---------------------

   procedure Store_Character (Self : in out JSON_Parser_Base'Class) is
   begin
      Self.Buffer.Append (VSS.Characters.Virtual_Character (Self.C));
   end Store_Character;

   ---------------------
   -- Store_Character --
   ---------------------

   procedure Store_Character
     (Self      : in out JSON_Parser_Base'Class;
      Character : Wide_Wide_Character) is
   begin
      Self.Buffer.Append (VSS.Characters.Virtual_Character (Character));
   end Store_Character;

   ---------------------
   -- Store_Character --
   ---------------------

   procedure Store_Character
     (Self : in out JSON_Parser_Base'Class;
      Code : VSS.Unicode.UTF16_Code_Unit) is
   begin
      Self.Buffer.Append (VSS.Characters.Virtual_Character'Val (Code));
   end Store_Character;

   ---------------------
   -- Store_Character --
   ---------------------

   procedure Store_Character
     (Self : in out JSON_Parser_Base'Class;
      High : VSS.Unicode.UTF16_Code_Unit;
      Low  : VSS.Unicode.UTF16_Code_Unit)
   is
      use type VSS.Unicode.Code_Point;
      use type VSS.Unicode.UTF16_Code_Unit;

      Code : VSS.Unicode.Code_Point := 16#01_0000#;

   begin
      Code :=
        Code
          + VSS.Unicode.Code_Point (High and 16#03FF#) * 16#0400#
          + VSS.Unicode.Code_Point (Low and 16#03FF#);
      Self.Buffer.Append (VSS.Characters.Virtual_Character'Val (Code));
   end Store_Character;

   ------------------
   -- String_Value --
   ------------------

   function String_Value
     (Self : JSON_Parser_Base'Class) return VSS.Strings.Virtual_String is
   begin
      return Self.Buffer;
   end String_Value;

   ---------
   -- Top --
   ---------

   function Top (Self : Parse_Stack'Class) return Parse_State is
   begin
      return Self.Stack (Self.Head);
   end Top;

end VSS.JSON.Implementation.Parsers;
