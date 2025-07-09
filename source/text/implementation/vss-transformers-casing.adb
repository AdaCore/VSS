--
--  Copyright (C) 2023-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Implementation.Strings;
with VSS.Implementation.Text_Handlers.UTF8;
with VSS.Implementation.UTF8_Casing;
with VSS.Implementation.UTF8_Strings;
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
      Aux : VSS.Implementation.UTF8_Strings.UTF8_String_Data;

   begin
      if Item.Is_Empty then
         return VSS.Strings.Empty_Virtual_String;
      end if;

      VSS.Implementation.UTF8_Casing.Convert_Case
        (VSS.Implementation.Text_Handlers.UTF8.UTF8_Text'Class
           (VSS.Implementation.Strings.Constant_Handler
                (VSS.Strings.Internals.Data_Access_Constant
                     (Item).all).all).Data,
         VSS.Implementation.UTF8_Casing.Lowercase,
         Aux);

      return Result : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Internals.To_Virtual_String (Aux)
      do
         VSS.Implementation.UTF8_Strings.Unreference (Aux);
      end return;
   end Transform;

   ---------------
   -- Transform --
   ---------------

   overriding procedure Transform
     (Self : Lowercase_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class)
   is
      Aux : VSS.Implementation.UTF8_Strings.UTF8_String_Data;

   begin
      if Item.Is_Empty then
         return;
      end if;

      VSS.Implementation.UTF8_Casing.Convert_Case
        (VSS.Implementation.Text_Handlers.UTF8.UTF8_Text'Class
           (VSS.Implementation.Strings.Constant_Handler
                (VSS.Strings.Internals.Data_Access_Constant
                     (Item).all).all).Data,
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
      Aux : VSS.Implementation.UTF8_Strings.UTF8_String_Data;

   begin
      if Item.Is_Empty then
         return VSS.Strings.Empty_Virtual_String;
      end if;

      VSS.Implementation.UTF8_Casing.Convert_Case
        (VSS.Implementation.Text_Handlers.UTF8.UTF8_Text'Class
           (VSS.Implementation.Strings.Constant_Handler
                (VSS.Strings.Internals.Data_Access_Constant
                     (Item).all).all).Data,
         VSS.Implementation.UTF8_Casing.Simple_Lowercase,
         Aux);

      return Result : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Internals.To_Virtual_String (Aux)
      do
         VSS.Implementation.UTF8_Strings.Unreference (Aux);
      end return;
   end Transform;

   ---------------
   -- Transform --
   ---------------

   overriding procedure Transform
     (Self : Simple_Lowercase_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class)
   is
      Aux : VSS.Implementation.UTF8_Strings.UTF8_String_Data;

   begin
      if Item.Is_Empty then
         return;
      end if;

      VSS.Implementation.UTF8_Casing.Convert_Case
        (VSS.Implementation.Text_Handlers.UTF8.UTF8_Text'Class
           (VSS.Implementation.Strings.Constant_Handler
                (VSS.Strings.Internals.Data_Access_Constant
                     (Item).all).all).Data,
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
      Aux : VSS.Implementation.UTF8_Strings.UTF8_String_Data;

   begin
      if Item.Is_Empty then
         return VSS.Strings.Empty_Virtual_String;
      end if;

      VSS.Implementation.UTF8_Casing.Convert_Case
        (VSS.Implementation.Text_Handlers.UTF8.UTF8_Text'Class
           (VSS.Implementation.Strings.Constant_Handler
                (VSS.Strings.Internals.Data_Access_Constant
                     (Item).all).all).Data,
         VSS.Implementation.UTF8_Casing.Simple_Uppercase,
         Aux);

      return Result : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Internals.To_Virtual_String (Aux)
      do
         VSS.Implementation.UTF8_Strings.Unreference (Aux);
      end return;
   end Transform;

   ---------------
   -- Transform --
   ---------------

   overriding procedure Transform
     (Self : Simple_Uppercase_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class)
   is
      Aux : VSS.Implementation.UTF8_Strings.UTF8_String_Data;

   begin
      if Item.Is_Empty then
         return;
      end if;

      VSS.Implementation.UTF8_Casing.Convert_Case
        (VSS.Implementation.Text_Handlers.UTF8.UTF8_Text'Class
           (VSS.Implementation.Strings.Constant_Handler
                (VSS.Strings.Internals.Data_Access_Constant
                     (Item).all).all).Data,
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
      Aux : VSS.Implementation.UTF8_Strings.UTF8_String_Data;

   begin
      if Item.Is_Empty then
         return VSS.Strings.Empty_Virtual_String;
      end if;

      VSS.Implementation.UTF8_Casing.Convert_Case
        (VSS.Implementation.Text_Handlers.UTF8.UTF8_Text'Class
           (VSS.Implementation.Strings.Constant_Handler
                (VSS.Strings.Internals.Data_Access_Constant
                     (Item).all).all).Data,
         VSS.Implementation.UTF8_Casing.Uppercase,
         Aux);

      return Result : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Internals.To_Virtual_String (Aux)
      do
         VSS.Implementation.UTF8_Strings.Unreference (Aux);
      end return;
   end Transform;

   ---------------
   -- Transform --
   ---------------

   overriding procedure Transform
     (Self : Uppercase_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class)
   is
      Aux : VSS.Implementation.UTF8_Strings.UTF8_String_Data;

   begin
      if Item.Is_Empty then
         return;
      end if;

      VSS.Implementation.UTF8_Casing.Convert_Case
        (VSS.Implementation.Text_Handlers.UTF8.UTF8_Text'Class
           (VSS.Implementation.Strings.Constant_Handler
                (VSS.Strings.Internals.Data_Access_Constant
                     (Item).all).all).Data,
         VSS.Implementation.UTF8_Casing.Uppercase,
         Aux);

      VSS.Strings.Internals.Set_By_Move (Item, Aux);
   end Transform;

end VSS.Transformers.Casing;
