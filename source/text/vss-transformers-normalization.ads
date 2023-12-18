--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Generic API to transform text data.

with VSS.Strings;

package VSS.Transformers.Normalization is

   pragma Preelaborate;

   To_Normalization_Form_D  : constant Abstract_Transformer'Class;
   --  Transform text to Normalization Form D.

   To_Normalization_Form_C  : constant Abstract_Transformer'Class;
   --  Transform text to Normalization Form C.

   To_Normalization_Form_KD : constant Abstract_Transformer'Class;
   --  Transform text to Normalization Form KD.

   To_Normalization_Form_KC : constant Abstract_Transformer'Class;
   --  Transform text to Normalization Form KC.

private

   type Normalization_Form_D_Transformer is
     limited new Abstract_Transformer with null record;

   overriding function Transform
     (Self : Normalization_Form_D_Transformer;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String;

   overriding procedure Transform
     (Self : Normalization_Form_D_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class);

   type Normalization_Form_C_Transformer is
     limited new Abstract_Transformer with null record;

   overriding function Transform
     (Self : Normalization_Form_C_Transformer;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String;

   overriding procedure Transform
     (Self : Normalization_Form_C_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class);

   type Normalization_Form_KD_Transformer is
     limited new Abstract_Transformer with null record;

   overriding function Transform
     (Self : Normalization_Form_KD_Transformer;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String;

   overriding procedure Transform
     (Self : Normalization_Form_KD_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class);

   type Normalization_Form_KC_Transformer is
     limited new Abstract_Transformer with null record;

   overriding function Transform
     (Self : Normalization_Form_KC_Transformer;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String;

   overriding procedure Transform
     (Self : Normalization_Form_KC_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class);

   To_Normalization_Form_D  : constant Abstract_Transformer'Class :=
     Normalization_Form_D_Transformer'(null record);
   To_Normalization_Form_C  : constant Abstract_Transformer'Class :=
     Normalization_Form_C_Transformer'(null record);
   To_Normalization_Form_KD : constant Abstract_Transformer'Class :=
     Normalization_Form_KD_Transformer'(null record);
   To_Normalization_Form_KC : constant Abstract_Transformer'Class :=
     Normalization_Form_KC_Transformer'(null record);

end VSS.Transformers.Normalization;
