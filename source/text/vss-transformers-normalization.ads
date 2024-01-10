--
--  Copyright (C) 2023-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Unicode Normalization transformations.

with VSS.Strings;

package VSS.Transformers.Normalization is

   pragma Preelaborate;

   --  XXX GNAT 20231112 All types need to be make publicaly declared and
   --  constants are declared as constants of that type (not classwide type
   --  of base type)
   --
   --   Normalization_Form_D  : constant Abstract_Transformer'Class;
   --   --  Transform text to Normalization Form D.
   --
   --   Normalization_Form_C  : constant Abstract_Transformer'Class;
   --   --  Transform text to Normalization Form C.
   --
   --   Normalization_Form_KD : constant Abstract_Transformer'Class;
   --   --  Transform text to Normalization Form KD.
   --
   --   Normalization_Form_KC : constant Abstract_Transformer'Class;
   --   --  Transform text to Normalization Form KC.

   type Normalization_Form_D_Transformer is
     limited new Abstract_Transformer with null record;
   --  @private

   overriding function Transform
     (Self : Normalization_Form_D_Transformer;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String;
   --  @private

   overriding procedure Transform
     (Self : Normalization_Form_D_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class);
   --  @private

   type Normalization_Form_C_Transformer is
     limited new Abstract_Transformer with null record;
   --  @private

   overriding function Transform
     (Self : Normalization_Form_C_Transformer;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String;
   --  @private

   overriding procedure Transform
     (Self : Normalization_Form_C_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class);
   --  @private

   type Normalization_Form_KD_Transformer is
     limited new Abstract_Transformer with null record;
   --  @private

   overriding function Transform
     (Self : Normalization_Form_KD_Transformer;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String;
   --  @private

   overriding procedure Transform
     (Self : Normalization_Form_KD_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class);
   --  @private

   type Normalization_Form_KC_Transformer is
     limited new Abstract_Transformer with null record;
   --  @private

   overriding function Transform
     (Self : Normalization_Form_KC_Transformer;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String;
   --  @private

   overriding procedure Transform
     (Self : Normalization_Form_KC_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class);
   --  @private

   Normalization_Form_D  : constant Normalization_Form_D_Transformer :=
     Normalization_Form_D_Transformer'(null record);
   Normalization_Form_C  : constant Normalization_Form_C_Transformer :=
     Normalization_Form_C_Transformer'(null record);
   Normalization_Form_KD : constant Normalization_Form_KD_Transformer :=
     Normalization_Form_KD_Transformer'(null record);
   Normalization_Form_KC : constant Normalization_Form_KC_Transformer :=
     Normalization_Form_KC_Transformer'(null record);

  --   Normalization_Form_D  : constant Abstract_Transformer'Class :=
  --     Normalization_Form_D_Transformer'(null record);
  --   Normalization_Form_C  : constant Abstract_Transformer'Class :=
  --     Normalization_Form_C_Transformer'(null record);
  --   Normalization_Form_KD : constant Abstract_Transformer'Class :=
  --     Normalization_Form_KD_Transformer'(null record);
  --   Normalization_Form_KC : constant Abstract_Transformer'Class :=
  --     Normalization_Form_KC_Transformer'(null record);

end VSS.Transformers.Normalization;
