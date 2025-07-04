--
--  Copyright (C) 2021-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Command_Line;
with Ada.Containers;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with UCD.Case_Folding_Loader;
with UCD.Characters;
with UCD.Derived_Core_Properties_Loader;
with UCD.Derived_General_Category_Loader;
with UCD.Properties;
with UCD.Property_Aliases_Loader;
with UCD.Property_Value_Aliases_Loader;

with VSS.Characters;
with VSS.Strings;

with Test_Support;

procedure Test_Characters is
   use type VSS.Characters.General_Category;

   procedure Initialize_UCD;
   --  Initialize UCD and loads necessary information for testing.

   procedure Test_Properties;
   --  Test properties of all characters.

   procedure Test_Well_Known;
   --  Test properties of few well known character to be sure that properties
   --  for them have expected values. Full coverage of all characters is done
   --  in other tests for some groups of properties.

   procedure Test_To_Virtual_String;
   --  Test conversion of the `Virtual_Character` into `Virtual_String`

   UCD_Loaded : Boolean := False;

   --------------------
   -- Initialize_UCD --
   --------------------

   procedure Initialize_UCD is
   begin
      if Ada.Command_Line.Argument_Count /= 1 then
         return;
      end if;

      declare
         UCD_Root : constant Wide_Wide_String :=
           Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode
             (Ada.Command_Line.Argument (1));

      begin
         UCD.Property_Aliases_Loader.Load (UCD_Root);
         UCD.Property_Value_Aliases_Loader.Load_Aliases (UCD_Root);

         UCD.Characters.Initialize_Character_Database;

         UCD.Property_Value_Aliases_Loader.Load_Missing (UCD_Root);
         UCD.Derived_General_Category_Loader.Load (UCD_Root);
         UCD.Derived_Core_Properties_Loader.Load (UCD_Root);

         UCD.Case_Folding_Loader.Load (UCD_Root);

         UCD_Loaded := True;
      end;
   end Initialize_UCD;

   ---------------------
   -- Test_Properties --
   ---------------------

   procedure Test_Properties is
   begin
      Initialize_UCD;

      if not UCD_Loaded then
         Test_Support.Fail ("UCD is not available");
      end if;

      declare
         use type UCD.Properties.Property_Value_Access;

         GC_Property : constant UCD.Properties.Property_Access :=
           UCD.Properties.Resolve ("gc");
         GC_Mapping  : constant
           array (VSS.Characters.General_Category)
           of UCD.Properties.Property_Value_Access :=
             [VSS.Characters.Uppercase_Letter =>
                UCD.Properties.Resolve (GC_Property, "Lu"),
              VSS.Characters.Lowercase_Letter =>
                UCD.Properties.Resolve (GC_Property, "Ll"),
              VSS.Characters.Titlecase_Letter =>
                UCD.Properties.Resolve (GC_Property, "Lt"),
              VSS.Characters.Modifier_Letter =>
                UCD.Properties.Resolve (GC_Property, "Lm"),
              VSS.Characters.Other_Letter =>
                UCD.Properties.Resolve (GC_Property, "Lo"),

              VSS.Characters.Nonspacing_Mark =>
                UCD.Properties.Resolve (GC_Property, "Mn"),
              VSS.Characters.Spacing_Mark =>
                UCD.Properties.Resolve (GC_Property, "Mc"),
              VSS.Characters.Enclosing_Mark =>
                UCD.Properties.Resolve (GC_Property, "Me"),

              VSS.Characters.Decimal_Number =>
                UCD.Properties.Resolve (GC_Property, "Nd"),
              VSS.Characters.Letter_Number =>
                UCD.Properties.Resolve (GC_Property, "Nl"),
              VSS.Characters.Other_Number =>
                UCD.Properties.Resolve (GC_Property, "No"),

              VSS.Characters.Connector_Punctuation =>
                UCD.Properties.Resolve (GC_Property, "Pc"),
              VSS.Characters.Dash_Punctuation =>
                UCD.Properties.Resolve (GC_Property, "Pd"),
              VSS.Characters.Open_Punctuation =>
                UCD.Properties.Resolve (GC_Property, "Ps"),
              VSS.Characters.Close_Punctuation =>
                UCD.Properties.Resolve (GC_Property, "Pe"),
              VSS.Characters.Initial_Punctuation =>
                UCD.Properties.Resolve (GC_Property, "Pi"),
              VSS.Characters.Final_Punctuation =>
                UCD.Properties.Resolve (GC_Property, "Pf"),
              VSS.Characters.Other_Punctuation =>
                UCD.Properties.Resolve (GC_Property, "Po"),

              VSS.Characters.Math_Symbol =>
                UCD.Properties.Resolve (GC_Property, "Sm"),
              VSS.Characters.Currency_Symbol =>
                UCD.Properties.Resolve (GC_Property, "Sc"),
              VSS.Characters.Modifier_Symbol =>
                UCD.Properties.Resolve (GC_Property, "Sk"),
              VSS.Characters.Other_Symbol =>
                UCD.Properties.Resolve (GC_Property, "So"),

              VSS.Characters.Space_Separator =>
                UCD.Properties.Resolve (GC_Property, "Zs"),
              VSS.Characters.Line_Separator =>
                UCD.Properties.Resolve (GC_Property, "Zl"),
              VSS.Characters.Paragraph_Separator =>
                UCD.Properties.Resolve (GC_Property, "Zp"),

              VSS.Characters.Control =>
                UCD.Properties.Resolve (GC_Property, "Cc"),
              VSS.Characters.Format =>
                UCD.Properties.Resolve (GC_Property, "Cf"),
              VSS.Characters.Surrogate =>
                UCD.Properties.Resolve (GC_Property, "Cs"),
              VSS.Characters.Private_Use =>
                UCD.Properties.Resolve (GC_Property, "Co"),
              VSS.Characters.Unassigned =>
                UCD.Properties.Resolve (GC_Property, "Cn")];

         Lowercase_Property : constant UCD.Properties.Property_Access :=
           UCD.Properties.Resolve ("Lowercase");
         Lowercase_Y        : constant UCD.Properties.Property_Value_Access :=
           UCD.Properties.Resolve (Lowercase_Property, "Y");
         Lowercase_N        : constant UCD.Properties.Property_Value_Access :=
           UCD.Properties.Resolve (Lowercase_Property, "N");
         Uppercase_Property : constant UCD.Properties.Property_Access :=
           UCD.Properties.Resolve ("Uppercase");
         Uppercase_Y        : constant UCD.Properties.Property_Value_Access :=
           UCD.Properties.Resolve (Uppercase_Property, "Y");
         Uppercase_N        : constant UCD.Properties.Property_Value_Access :=
           UCD.Properties.Resolve (Uppercase_Property, "N");
         Cased_Property     : constant UCD.Properties.Property_Access :=
           UCD.Properties.Resolve ("Cased");
         Cased_Y            : constant UCD.Properties.Property_Value_Access :=
           UCD.Properties.Resolve (Cased_Property, "Y");
         Cased_N            : constant UCD.Properties.Property_Value_Access :=
           UCD.Properties.Resolve (Cased_Property, "N");

         Simple_Case_Folding_Property : constant
           UCD.Properties.Property_Access :=
             UCD.Properties.Resolve ("Simple_Case_Folding");

      begin
         for Character in VSS.Characters.Virtual_Character'First_Valid
                            .. VSS.Characters.Virtual_Character'Last_Valid
         loop
            --  General Category

            Test_Support.Assert
              (UCD.Characters.Get
                 (VSS.Characters.Virtual_Character'Pos (Character),
                  GC_Property)
               = GC_Mapping (VSS.Characters.Get_General_Category (Character)));

            --  Lowercase

            if VSS.Characters.Is_Valid_Virtual_Character (Character)
              and then VSS.Characters.Get_Lowercase (Character)
            then
               Test_Support.Assert
                 (UCD.Characters.Get
                    (VSS.Characters.Virtual_Character'Pos (Character),
                     Lowercase_Property) = Lowercase_Y);

            else
               Test_Support.Assert
                 (UCD.Characters.Get
                    (VSS.Characters.Virtual_Character'Pos (Character),
                     Lowercase_Property) = Lowercase_N);
            end if;

            --  Uppercase

            if VSS.Characters.Is_Valid_Virtual_Character (Character)
              and then VSS.Characters.Get_Uppercase (Character)
            then
               Test_Support.Assert
                 (UCD.Characters.Get
                    (VSS.Characters.Virtual_Character'Pos (Character),
                     Uppercase_Property) = Uppercase_Y);

            else
               Test_Support.Assert
                 (UCD.Characters.Get
                    (VSS.Characters.Virtual_Character'Pos (Character),
                     Uppercase_Property) = Uppercase_N);
            end if;

            --  Cased

            if VSS.Characters.Is_Valid_Virtual_Character (Character)
              and then VSS.Characters.Get_Cased (Character)
            then
               Test_Support.Assert
                 (UCD.Characters.Get
                    (VSS.Characters.Virtual_Character'Pos (Character),
                     Cased_Property) = Cased_Y);

            else
               Test_Support.Assert
                 (UCD.Characters.Get
                    (VSS.Characters.Virtual_Character'Pos (Character),
                     Cased_Property) = Cased_N);
            end if;

            --  Simple case folding

            if VSS.Characters.Is_Valid_Virtual_Character (Character) then
               declare
                  use type Ada.Containers.Count_Type;
                  use type VSS.Characters.Virtual_Character;

                  V : constant UCD.Properties.Property_Value_Access :=
                    UCD.Characters.Get
                      (VSS.Characters.Virtual_Character'Pos (Character),
                       Simple_Case_Folding_Property);
                  M : constant VSS.Characters.Virtual_Character :=
                    VSS.Characters.Get_Simple_Case_Folding (Character);

               begin
                  if V = null then
                     --  No specific mapping defined, character maps to itself

                     Test_Support.Assert (Character = M);

                  else
                     Test_Support.Assert (V.String.Length = 1);
                     Test_Support.Assert
                       (VSS.Characters.Virtual_Character'Val (V.String (1))
                          = M);
                  end if;
               end;
            end if;
         end loop;
      end;
   end Test_Properties;

   ----------------------------
   -- Test_To_Virtual_String --
   ----------------------------

   procedure Test_To_Virtual_String is
      use type VSS.Strings.Character_Count;
      use type VSS.Strings.Virtual_String;

      C : constant VSS.Characters.Virtual_Character := 'a';
      S : VSS.Strings.Virtual_String;

   begin
      S := VSS.Characters.To_Virtual_String (C);
      Test_Support.Assert (not S.Is_Empty);
      Test_Support.Assert (S.Character_Length = 1);
      Test_Support.Assert (S = "a");
   end Test_To_Virtual_String;

   ---------------------
   -- Test_Well_Known --
   ---------------------

   procedure Test_Well_Known is
      use type VSS.Characters.Virtual_Character;
      use type VSS.Strings.Virtual_String;

   begin
      --  General_Category

      Test_Support.Assert
        (VSS.Characters.Get_General_Category
           (VSS.Characters.Virtual_Character'Val (16#00#))
         = VSS.Characters.Control);
      Test_Support.Assert
        (VSS.Characters.Get_General_Category
           (VSS.Characters.Virtual_Character'Val (16#20#))
         = VSS.Characters.Space_Separator);
      Test_Support.Assert
        (VSS.Characters.Get_General_Category
           (VSS.Characters.Virtual_Character'Val (16#31#))
         = VSS.Characters.Decimal_Number);
      Test_Support.Assert
        (VSS.Characters.Get_General_Category
           (VSS.Characters.Virtual_Character'Val (16#41#))
         = VSS.Characters.Uppercase_Letter);
      Test_Support.Assert
        (VSS.Characters.Get_General_Category
           (VSS.Characters.Virtual_Character'Val (16#430#))
         = VSS.Characters.Lowercase_Letter);
      Test_Support.Assert
        (VSS.Characters.Get_General_Category
           (VSS.Characters.Virtual_Character'Val (16#D800#))
         = VSS.Characters.Surrogate);

      --  Case mappings

      Test_Support.Assert
        (VSS.Characters.Get_Simple_Lowercase_Mapping
           (VSS.Characters.Virtual_Character'Val (16#41#))
         = VSS.Characters.Virtual_Character'Val (16#61#));
      Test_Support.Assert
        (VSS.Characters.Get_Simple_Titlecase_Mapping
           (VSS.Characters.Virtual_Character'Val (16#61#))
         = VSS.Characters.Virtual_Character'Val (16#41#));
      Test_Support.Assert
        (VSS.Characters.Get_Simple_Uppercase_Mapping
           (VSS.Characters.Virtual_Character'Val (16#61#))
         = VSS.Characters.Virtual_Character'Val (16#41#));
      Test_Support.Assert
        (VSS.Characters.Get_Simple_Case_Folding
           (VSS.Characters.Virtual_Character'Val (16#61#))
         = VSS.Characters.Virtual_Character'Val (16#61#));
      Test_Support.Assert
        (VSS.Characters.Get_Simple_Case_Folding
           (VSS.Characters.Virtual_Character'Val (16#41#))
         = VSS.Characters.Virtual_Character'Val (16#61#));

      Test_Support.Assert
        (VSS.Characters.Get_Lowercase_Mapping ('А') = "а");
      Test_Support.Assert
        (VSS.Characters.Get_Titlecase_Mapping ('а') = "А");
      Test_Support.Assert
        (VSS.Characters.Get_Uppercase_Mapping ('а') = "А");

      Test_Support.Assert
        (VSS.Characters.Get_Lowercase_Mapping ('1') = "1");
      Test_Support.Assert
        (VSS.Characters.Get_Titlecase_Mapping ('1') = "1");
      Test_Support.Assert
        (VSS.Characters.Get_Uppercase_Mapping ('1') = "1");
   end Test_Well_Known;

begin
   Test_Support.Run_Testcase
     (Test_Well_Known'Access, "well-known-character-properties");

   Test_Support.Run_Testcase
     (Test_Properties'Access,
      "all-character-properties",
      "Test character's properties of all characters");
   Test_Support.Run_Testcase
     (Test_To_Virtual_String'Access, "To_Virtual_String");
end Test_Characters;
