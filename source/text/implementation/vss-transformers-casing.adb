--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Implementation.Strings;
with VSS.Implementation.UTF8_Casing;
with VSS.Strings.Internals;

package body VSS.Transformers.Casing is

   ---------------
   -- Transform --
   ---------------

   overriding function Transform
     (Self : Lowercase_Transformer;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String
   is
      Aux : VSS.Implementation.Strings.String_Data;

   begin
      VSS.Implementation.UTF8_Casing.Convert_Case
        (VSS.Strings.Internals.Data_Access_Constant (Item).all,
         VSS.Implementation.UTF8_Casing.Lowercase,
         Aux);

      return Result : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Internals.To_Virtual_String (Aux)
      do
         VSS.Implementation.Strings.Unreference (Aux);
      end return;
   end Transform;

   ---------------
   -- Transform --
   ---------------

   overriding procedure Transform
     (Self : Lowercase_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class)
   is
      Aux : VSS.Implementation.Strings.String_Data;

   begin
      VSS.Implementation.UTF8_Casing.Convert_Case
        (VSS.Strings.Internals.Data_Access_Constant (Item).all,
         VSS.Implementation.UTF8_Casing.Lowercase,
         Aux);

      VSS.Strings.Internals.Set_By_Move (Item, Aux);
   end Transform;

   ---------------
   -- Transform --
   ---------------

   overriding function Transform
     (Self : Simple_Lowercase_Transformer;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String
   is
      Aux : VSS.Implementation.Strings.String_Data;

   begin
      VSS.Implementation.UTF8_Casing.Convert_Case
        (VSS.Strings.Internals.Data_Access_Constant (Item).all,
         VSS.Implementation.UTF8_Casing.Simple_Lowercase,
         Aux);

      return Result : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Internals.To_Virtual_String (Aux)
      do
         VSS.Implementation.Strings.Unreference (Aux);
      end return;
   end Transform;

   ---------------
   -- Transform --
   ---------------

   overriding procedure Transform
     (Self : Simple_Lowercase_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class)
   is
      Aux : VSS.Implementation.Strings.String_Data;

   begin
      VSS.Implementation.UTF8_Casing.Convert_Case
        (VSS.Strings.Internals.Data_Access_Constant (Item).all,
         VSS.Implementation.UTF8_Casing.Simple_Lowercase,
         Aux);

      VSS.Strings.Internals.Set_By_Move (Item, Aux);
   end Transform;

   ---------------
   -- Transform --
   ---------------

   overriding function Transform
     (Self : Simple_Uppercase_Transformer;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String
   is
      Aux : VSS.Implementation.Strings.String_Data;

   begin
      VSS.Implementation.UTF8_Casing.Convert_Case
        (VSS.Strings.Internals.Data_Access_Constant (Item).all,
         VSS.Implementation.UTF8_Casing.Simple_Uppercase,
         Aux);

      return Result : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Internals.To_Virtual_String (Aux)
      do
         VSS.Implementation.Strings.Unreference (Aux);
      end return;
   end Transform;

   ---------------
   -- Transform --
   ---------------

   overriding procedure Transform
     (Self : Simple_Uppercase_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class)
   is
      Aux : VSS.Implementation.Strings.String_Data;

   begin
      VSS.Implementation.UTF8_Casing.Convert_Case
        (VSS.Strings.Internals.Data_Access_Constant (Item).all,
         VSS.Implementation.UTF8_Casing.Simple_Uppercase,
         Aux);

      VSS.Strings.Internals.Set_By_Move (Item, Aux);
   end Transform;

   ---------------
   -- Transform --
   ---------------

   overriding function Transform
     (Self : Uppercase_Transformer;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String
   is
      Aux : VSS.Implementation.Strings.String_Data;

   begin
      VSS.Implementation.UTF8_Casing.Convert_Case
        (VSS.Strings.Internals.Data_Access_Constant (Item).all,
         VSS.Implementation.UTF8_Casing.Uppercase,
         Aux);

      return Result : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Internals.To_Virtual_String (Aux)
      do
         VSS.Implementation.Strings.Unreference (Aux);
      end return;
   end Transform;

   ---------------
   -- Transform --
   ---------------

   overriding procedure Transform
     (Self : Uppercase_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class)
   is
      Aux : VSS.Implementation.Strings.String_Data;

   begin
      VSS.Implementation.UTF8_Casing.Convert_Case
        (VSS.Strings.Internals.Data_Access_Constant (Item).all,
         VSS.Implementation.UTF8_Casing.Uppercase,
         Aux);

      VSS.Strings.Internals.Set_By_Move (Item, Aux);
   end Transform;

end VSS.Transformers.Casing;
