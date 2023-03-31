--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

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
      pragma Unreferenced (Self);
      pragma Unreferenced (Parameters);

   begin
      return Empty_Virtual_String;
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
