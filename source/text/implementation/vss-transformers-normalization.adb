--
--  Copyright (C) 2023-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Implementation.UTF8_Normalization;
with VSS.Implementation.UTF8_Strings;
with VSS.Strings.Internals;

package body VSS.Transformers.Normalization is

   ---------------
   -- Transform --
   ---------------

   overriding function Transform
     (Self : Normalization_Form_C_Transformer;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String
   is
      Aux : VSS.Implementation.UTF8_Strings.UTF8_String_Data;

   begin
      VSS.Implementation.UTF8_Normalization.Normalize
        (VSS.Strings.Internals.Data_Access_Constant (Item).all,
         VSS.Implementation.Normalization_Form_C,
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
     (Self : Normalization_Form_C_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class)
   is
      Aux : VSS.Implementation.UTF8_Strings.UTF8_String_Data;

   begin
      VSS.Implementation.UTF8_Normalization.Normalize
        (VSS.Strings.Internals.Data_Access_Constant (Item).all,
         VSS.Implementation.Normalization_Form_C,
         Aux);

      VSS.Strings.Internals.Set_By_Move (Item, Aux);
   end Transform;

   ---------------
   -- Transform --
   ---------------

   overriding function Transform
     (Self : Normalization_Form_D_Transformer;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String
   is
      Aux : VSS.Implementation.UTF8_Strings.UTF8_String_Data;

   begin
      VSS.Implementation.UTF8_Normalization.Normalize
        (VSS.Strings.Internals.Data_Access_Constant (Item).all,
         VSS.Implementation.Normalization_Form_D,
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
     (Self : Normalization_Form_D_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class)
   is
      Aux : VSS.Implementation.UTF8_Strings.UTF8_String_Data;

   begin
      VSS.Implementation.UTF8_Normalization.Normalize
        (VSS.Strings.Internals.Data_Access_Constant (Item).all,
         VSS.Implementation.Normalization_Form_D,
         Aux);

      VSS.Strings.Internals.Set_By_Move (Item, Aux);
   end Transform;

   ---------------
   -- Transform --
   ---------------

   overriding function Transform
     (Self : Normalization_Form_KC_Transformer;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String
   is
      Aux : VSS.Implementation.UTF8_Strings.UTF8_String_Data;

   begin
      VSS.Implementation.UTF8_Normalization.Normalize
        (VSS.Strings.Internals.Data_Access_Constant (Item).all,
         VSS.Implementation.Normalization_Form_KC,
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
     (Self : Normalization_Form_KC_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class)
   is
      Aux : VSS.Implementation.UTF8_Strings.UTF8_String_Data;

   begin
      VSS.Implementation.UTF8_Normalization.Normalize
        (VSS.Strings.Internals.Data_Access_Constant (Item).all,
         VSS.Implementation.Normalization_Form_KC,
         Aux);

      VSS.Strings.Internals.Set_By_Move (Item, Aux);
   end Transform;

   ---------------
   -- Transform --
   ---------------

   overriding function Transform
     (Self : Normalization_Form_KD_Transformer;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String
   is
      Aux : VSS.Implementation.UTF8_Strings.UTF8_String_Data;

   begin
      VSS.Implementation.UTF8_Normalization.Normalize
        (VSS.Strings.Internals.Data_Access_Constant (Item).all,
         VSS.Implementation.Normalization_Form_KD,
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
     (Self : Normalization_Form_KD_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class)
   is
      Aux : VSS.Implementation.UTF8_Strings.UTF8_String_Data;

   begin
      VSS.Implementation.UTF8_Normalization.Normalize
        (VSS.Strings.Internals.Data_Access_Constant (Item).all,
         VSS.Implementation.Normalization_Form_KD,
         Aux);

      VSS.Strings.Internals.Set_By_Move (Item, Aux);
   end Transform;

end VSS.Transformers.Normalization;
