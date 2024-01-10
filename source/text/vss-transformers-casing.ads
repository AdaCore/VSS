--
--  Copyright (C) 2023-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Unicode default case conversion.
--
--  For locale tailored case conversion see VSS.Locales.

with VSS.Strings;

package VSS.Transformers.Casing is

   pragma Preelaborate;

   --  XXX GNAT 20231112 All types need to be make publicaly declared and
   --  constants are declared as constants of that type (not classwide type
   --  of base type)
   --
   --   Lowercase        : constant Abstract_Transformer'Class;
   --   --  Convert text to lowercase using default full case conversion.
   --
   --   Uppercase        : constant Abstract_Transformer'Class;
   --   --  Convert text to uppercase using default full case conversion.
   --
   --   Simple_Lowercase : constant Abstract_Transformer'Class;
   --   --  Convert text to lowercase using default simple case conversion.
   --
   --   Simple_Uppercase : constant Abstract_Transformer'Class;
   --   --  Convert text to uppercase using default simple case conversion.

   type Simple_Lowercase_Transformer is
     limited new Abstract_Transformer with null record;

   overriding function Transform
     (Self : Simple_Lowercase_Transformer;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String;

   overriding procedure Transform
     (Self : Simple_Lowercase_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class);

   type Simple_Uppercase_Transformer is
     limited new Abstract_Transformer with null record;

   overriding function Transform
     (Self : Simple_Uppercase_Transformer;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String;

   overriding procedure Transform
     (Self : Simple_Uppercase_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class);

   type Lowercase_Transformer is
     limited new Abstract_Transformer with null record;

   overriding function Transform
     (Self : Lowercase_Transformer;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String;

   overriding procedure Transform
     (Self : Lowercase_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class);

   type Uppercase_Transformer is
     limited new Abstract_Transformer with null record;

   overriding function Transform
     (Self : Uppercase_Transformer;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String;

   overriding procedure Transform
     (Self : Uppercase_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class);

   Lowercase        : constant Lowercase_Transformer :=
     Lowercase_Transformer'(null record);
   Simple_Lowercase : constant Simple_Lowercase_Transformer :=
     Simple_Lowercase_Transformer'(null record);
   Simple_Uppercase : constant Simple_Uppercase_Transformer :=
     Simple_Uppercase_Transformer'(null record);
   Uppercase        : constant Uppercase_Transformer :=
     Uppercase_Transformer'(null record);

   --   Lowercase        : constant Abstract_Transformer'Class :=
   --     Lowercase_Transformer'(null record);
   --   Simple_Lowercase : constant Abstract_Transformer'Class :=
   --     Simple_Lowercase_Transformer'(null record);
   --   Simple_Uppercase : constant Abstract_Transformer'Class :=
   --     Simple_Uppercase_Transformer'(null record);
   --   Uppercase        : constant Abstract_Transformer'Class :=
   --     Uppercase_Transformer'(null record);

end VSS.Transformers.Casing;
