--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Wide_Wide_Text_IO;
with Ada.Strings.Wide_Wide_Fixed;

with VSS.Strings.Conversions;
with VSS.Strings.Texts;

package body VSS.XML.Implementation.Error_Handlers is

   procedure Put
     (Kind  : VSS.Strings.Virtual_String;
      Error : VSS.XML.Parse_Errors.SAX_Parse_Error'Class);

   -----------
   -- Error --
   -----------

   overriding procedure Error
     (Self    : in out Default_Error_Handler;
      Error   : VSS.XML.Parse_Errors.SAX_Parse_Error'Class;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);

   begin
      Put ("error", Error);
   end Error;

   -----------------
   -- Fatal_Error --
   -----------------

   overriding procedure Fatal_Error
     (Self    : in out Default_Error_Handler;
      Error   : VSS.XML.Parse_Errors.SAX_Parse_Error'Class;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);

   begin
      Put ("fatal error", Error);
   end Fatal_Error;

   ---------
   -- Put --
   ---------

   procedure Put
     (Kind  : VSS.Strings.Virtual_String;
      Error : VSS.XML.Parse_Errors.SAX_Parse_Error'Class)
   is
      Line_Image   : constant Wide_Wide_String :=
        Ada.Strings.Wide_Wide_Fixed.Trim
          (VSS.Strings.Texts.Line_Index'Base'Wide_Wide_Image
             (Error.Get_Line_Number),
           Ada.Strings.Both);
      Column_Image : constant Wide_Wide_String :=
        Ada.Strings.Wide_Wide_Fixed.Trim
          (VSS.Strings.Character_Index'Base'Wide_Wide_Image
             (Error.Get_Column_Number),
           Ada.Strings.Both);

   begin
      Ada.Wide_Wide_Text_IO.Put_Line
        (Ada.Wide_Wide_Text_IO.Standard_Error,
         VSS.Strings.Conversions.To_Wide_Wide_String (Error.Get_System_Id)
         & ':'
         & Line_Image
         & ':'
         & Column_Image
         & ": <"
         & VSS.Strings.Conversions.To_Wide_Wide_String (Kind)
         & "> "
         & VSS.Strings.Conversions.To_Wide_Wide_String (Error.Get_Message));
   end Put;

   -------------
   -- Warning --
   -------------

   overriding procedure Warning
     (Self    : in out Default_Error_Handler;
      Error   : VSS.XML.Parse_Errors.SAX_Parse_Error'Class;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);

   begin
      Put ("warning", Error);
   end Warning;

end VSS.XML.Implementation.Error_Handlers;
