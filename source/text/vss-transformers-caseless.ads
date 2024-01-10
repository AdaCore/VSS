--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Unicode default caseless conversion.
--
--  For locale tailored caseless conversion see VSS.Locales.

with VSS.Strings;

package VSS.Transformers.Caseless is

   pragma Preelaborate;

   --  XXX GNAT 20231112 All types need to be make publicaly declared and
   --  constants are declared as constants of that type (not classwide type
   --  of base type)
   --
   --   To_Default_Caseless       : constant Abstract_Transformer'Class;
   --   --  Convert text to lowercase using default full case conversion.
   --
   --   To_Canonical_Caseless     : constant Abstract_Transformer'Class;
   --   --  Convert text to uppercase using default full case conversion.
   --
   --   To_Compatibility_Caseless : constant Abstract_Transformer'Class;
   --   --  Convert text to lowercase using default simple case conversion.
   --
   --   To_Identifier_Caseless    : constant Abstract_Transformer'Class;
   --   --  Convert text to uppercase using default simple case conversion.

   type Identifier_Caseless_Transformer is
     limited new Abstract_Transformer with null record;
   --  @private

   overriding function Transform
     (Self : Identifier_Caseless_Transformer;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String;
   --  @private

   overriding procedure Transform
     (Self : Identifier_Caseless_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class);
   --  @private

   To_Identifier_Caseless : constant Identifier_Caseless_Transformer :=
     Identifier_Caseless_Transformer'(null record);

end VSS.Transformers.Caseless;
