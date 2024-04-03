--
--  Copyright (C) 2023-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with VSS.Implementation.Character_Codes;
with VSS.Implementation.Text_Handlers;
with VSS.Unicode;

package body VSS.Strings.Templates is

   type Formatter_Access is
     access constant VSS.Strings.Formatters.Abstract_Formatter'Class;

   type Formatter_Array is array (Positive range 1 .. 9) of Formatter_Access;

   function Format
     (Self       : Virtual_String_Template'Class;
      Parameters : Formatter_Array) return VSS.Strings.Virtual_String;

   ------------
   -- Format --
   ------------

   function Format
     (Self       : Virtual_String_Template'Class;
      Parameters : Formatter_Array) return VSS.Strings.Virtual_String
   is
      use VSS.Implementation.Character_Codes;
      use type VSS.Unicode.Code_Point;

      type States is (Initial, Open_Bracket, Name, Format);

      procedure Append_Parameter;

      Handler          : constant
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (Self.Template.Data);
      Position         : aliased VSS.Implementation.Strings.Cursor;
      Code             : VSS.Unicode.Code_Point'Base;
      Parameter        : Positive := 1;
      State            : States   := Initial;
      Result           : VSS.Strings.Virtual_String;
      Parameter_Format : VSS.Strings.Formatters.Format_Information;
      --  Parameter_Format : VSS.Strings.Virtual_String;

      ----------------------
      -- Append_Parameter --
      ----------------------

      procedure Append_Parameter is
      begin
         if Parameters (Parameter) /= null then
            Result.Append (Parameters (Parameter).Format (Parameter_Format));
            Parameter := @ + 1;
            Parameter_Format :=
              (Width     => 0,
               Alignment => VSS.Strings.Formatters.Default,
               Format    => VSS.Strings.Empty_Virtual_String);
         end if;
      end Append_Parameter;

   begin
      Handler.Before_First_Character (Self.Template.Data, Position);

      while Handler.Forward_Element (Self.Template.Data, Position, Code) loop
         case State is
            when Initial =>
               case Code is
                  when Left_Curly_Bracket =>
                     State := Open_Bracket;

                  when others =>
                     Result.Append
                       (VSS.Characters.Virtual_Character'Val (Code));
               end case;

            when Open_Bracket =>
               case Code is
                  when Left_Curly_Bracket =>
                     Result.Append ('{');
                     State := Initial;

                  when Right_Curly_Bracket =>
                     Append_Parameter;
                     State := Initial;

                  when Colon =>
                     State := Format;

                  when others =>
                     State := Name;
               end case;

            when Name =>
               case Code is
                  when Right_Curly_Bracket =>
                     Append_Parameter;
                     State := Initial;

                  when Colon =>
                     State := Format;

                  when others =>
                     null;
               end case;

            when Format =>
               case Code is
                  when Right_Curly_Bracket =>
                     Append_Parameter;
                     State := Initial;

                  when others =>
                     Parameter_Format.Format.Append
                       (VSS.Characters.Virtual_Character'Val (Code));
               end case;
         end case;
      end loop;

      return Result;
   end Format;

   ------------
   -- Format --
   ------------

   function Format
     (Self      : Virtual_String_Template;
      Parameter : VSS.Strings.Formatters.Abstract_Formatter'Class)
      return VSS.Strings.Virtual_String is
   begin
      return
        Self.Format
          (Formatter_Array'(1 => Parameter'Unchecked_Access, others => null));
   end Format;

   ------------
   -- Format --
   ------------

   function Format
     (Self        : Virtual_String_Template;
      Parameter_1 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_2 : VSS.Strings.Formatters.Abstract_Formatter'Class)
      return VSS.Strings.Virtual_String is
   begin
      return
        Self.Format
          (Formatter_Array'
             (1      => Parameter_1'Unchecked_Access,
              2      => Parameter_2'Unchecked_Access,
              others => null));
   end Format;

   ------------
   -- Format --
   ------------

   function Format
     (Self        : Virtual_String_Template;
      Parameter_1 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_2 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_3 : VSS.Strings.Formatters.Abstract_Formatter'Class)
      return VSS.Strings.Virtual_String is
   begin
      return
        Self.Format
          (Formatter_Array'
             (1      => Parameter_1'Unchecked_Access,
              2      => Parameter_2'Unchecked_Access,
              3      => Parameter_3'Unchecked_Access,
              others => null));
   end Format;

   ------------
   -- Format --
   ------------

   function Format
     (Self        : Virtual_String_Template;
      Parameter_1 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_2 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_3 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_4 : VSS.Strings.Formatters.Abstract_Formatter'Class)
      return VSS.Strings.Virtual_String is
   begin
      return
        Self.Format
          (Formatter_Array'
             (1      => Parameter_1'Unchecked_Access,
              2      => Parameter_2'Unchecked_Access,
              3      => Parameter_3'Unchecked_Access,
              4      => Parameter_4'Unchecked_Access,
              others => null));
   end Format;

   ------------
   -- Format --
   ------------

   function Format
     (Self        : Virtual_String_Template;
      Parameter_1 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_2 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_3 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_4 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_5 : VSS.Strings.Formatters.Abstract_Formatter'Class)
      return VSS.Strings.Virtual_String is
   begin
      return
        Self.Format
          (Formatter_Array'
             (1      => Parameter_1'Unchecked_Access,
              2      => Parameter_2'Unchecked_Access,
              3      => Parameter_3'Unchecked_Access,
              4      => Parameter_4'Unchecked_Access,
              5      => Parameter_5'Unchecked_Access,
              others => null));
   end Format;

   ------------
   -- Format --
   ------------

   function Format
     (Self        : Virtual_String_Template;
      Parameter_1 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_2 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_3 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_4 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_5 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_6 : VSS.Strings.Formatters.Abstract_Formatter'Class)
      return VSS.Strings.Virtual_String is
   begin
      return
        Self.Format
          (Formatter_Array'
             (1      => Parameter_1'Unchecked_Access,
              2      => Parameter_2'Unchecked_Access,
              3      => Parameter_3'Unchecked_Access,
              4      => Parameter_4'Unchecked_Access,
              5      => Parameter_5'Unchecked_Access,
              6      => Parameter_6'Unchecked_Access,
              others => null));
   end Format;

   ------------
   -- Format --
   ------------

   function Format
     (Self        : Virtual_String_Template;
      Parameter_1 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_2 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_3 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_4 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_5 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_6 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_7 : VSS.Strings.Formatters.Abstract_Formatter'Class)
      return VSS.Strings.Virtual_String is
   begin
      return
        Self.Format
          (Formatter_Array'
             (1      => Parameter_1'Unchecked_Access,
              2      => Parameter_2'Unchecked_Access,
              3      => Parameter_3'Unchecked_Access,
              4      => Parameter_4'Unchecked_Access,
              5      => Parameter_5'Unchecked_Access,
              6      => Parameter_6'Unchecked_Access,
              7      => Parameter_7'Unchecked_Access,
              others => null));
   end Format;

   ------------
   -- Format --
   ------------

   function Format
     (Self        : Virtual_String_Template;
      Parameter_1 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_2 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_3 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_4 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_5 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_6 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_7 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_8 : VSS.Strings.Formatters.Abstract_Formatter'Class)
      return VSS.Strings.Virtual_String is
   begin
      return
        Self.Format
          (Formatter_Array'
             (1      => Parameter_1'Unchecked_Access,
              2      => Parameter_2'Unchecked_Access,
              3      => Parameter_3'Unchecked_Access,
              4      => Parameter_4'Unchecked_Access,
              5      => Parameter_5'Unchecked_Access,
              6      => Parameter_6'Unchecked_Access,
              7      => Parameter_7'Unchecked_Access,
              8      => Parameter_8'Unchecked_Access,
              others => null));
   end Format;

   ------------
   -- Format --
   ------------

   function Format
     (Self        : Virtual_String_Template;
      Parameter_1 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_2 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_3 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_4 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_5 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_6 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_7 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_8 : VSS.Strings.Formatters.Abstract_Formatter'Class;
      Parameter_9 : VSS.Strings.Formatters.Abstract_Formatter'Class)
      return VSS.Strings.Virtual_String is
   begin
      return
        Self.Format
          (Formatter_Array'
             (1      => Parameter_1'Unchecked_Access,
              2      => Parameter_2'Unchecked_Access,
              3      => Parameter_3'Unchecked_Access,
              4      => Parameter_4'Unchecked_Access,
              5      => Parameter_5'Unchecked_Access,
              6      => Parameter_6'Unchecked_Access,
              7      => Parameter_7'Unchecked_Access,
              8      => Parameter_8'Unchecked_Access,
              9      => Parameter_9'Unchecked_Access));
   end Format;

   --------------------------------
   -- To_Virtual_String_Template --
   --------------------------------

   function To_Virtual_String_Template
     (Item : Wide_Wide_String) return Virtual_String_Template is
   begin
      return (Template => VSS.Strings.To_Virtual_String (Item));
   end To_Virtual_String_Template;

end VSS.Strings.Templates;
