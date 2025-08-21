--
--  Copyright (C) 2024-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Implementation.UTF8_Casing;
with VSS.Implementation.UTF8_Normalization;
with VSS.Implementation.UTF8_Strings;
with VSS.Strings.Internals;

package body VSS.Transformers.Caseless is

   ---------------
   -- Transform --
   ---------------

   overriding function Transform
     (Self : Identifier_Caseless_Transformer;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String
   is
      Aux_NFD       : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Aux_CF_Mapped : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Aux_CF_NFC    : VSS.Implementation.UTF8_Strings.UTF8_String_Data;

   begin
      if Item.Is_Empty then
         return VSS.Strings.Empty_Virtual_String;
      end if;

      VSS.Implementation.UTF8_Normalization.Normalize
        (VSS.Strings.Internals.Data_Access_Constant (Item).all,
         VSS.Implementation.Normalization_Form_D,
         Aux_NFD);
      VSS.Implementation.UTF8_Casing.Convert_Case
        (Aux_NFD,
         VSS.Implementation.UTF8_Casing.NFKC_Casefold,
         Aux_CF_Mapped);
      VSS.Implementation.UTF8_Normalization.Normalize
        (Aux_CF_Mapped,
         VSS.Implementation.Normalization_Form_C,
         Aux_CF_NFC);

      return Result : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Internals.To_Virtual_String (Aux_CF_NFC)
      do
         VSS.Implementation.UTF8_Strings.Unreference (Aux_NFD);
         VSS.Implementation.UTF8_Strings.Unreference (Aux_CF_Mapped);
         VSS.Implementation.UTF8_Strings.Unreference (Aux_CF_NFC);
      end return;
   end Transform;

   ---------------
   -- Transform --
   ---------------

   overriding procedure Transform
     (Self : Identifier_Caseless_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class)
   is
      Aux_NFD       : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Aux_CF_Mapped : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Aux_CF_NFC    : VSS.Implementation.UTF8_Strings.UTF8_String_Data;

   begin
      VSS.Implementation.UTF8_Normalization.Normalize
        (VSS.Strings.Internals.Data_Access_Constant (Item).all,
         VSS.Implementation.Normalization_Form_D,
         Aux_NFD);
      VSS.Implementation.UTF8_Casing.Convert_Case
        (Aux_NFD,
         VSS.Implementation.UTF8_Casing.NFKC_Casefold,
         Aux_CF_Mapped);
      VSS.Implementation.UTF8_Normalization.Normalize
        (Aux_CF_Mapped,
         VSS.Implementation.Normalization_Form_C,
         Aux_CF_NFC);

      VSS.Implementation.UTF8_Strings.Unreference (Aux_NFD);
      VSS.Implementation.UTF8_Strings.Unreference (Aux_CF_Mapped);
      VSS.Strings.Internals.Set_By_Move (Item, Aux_CF_NFC);
   end Transform;

end VSS.Transformers.Caseless;
