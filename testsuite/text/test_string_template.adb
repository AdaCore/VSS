--
--  Copyright (C) 2023-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Interfaces;

with VSS.Strings.Formatters.Generic_Integers;
with VSS.Strings.Formatters.Strings;
with VSS.Strings.Templates;

with Test_Support;

procedure Test_String_Template is

   procedure Test_Single_Placeholer;

   procedure Test_Multiple_Placeholer;

   procedure Test_Integer_Formatter;

   procedure Test_Formatters;
   --  Run testsuite of different formatter

   procedure Test_Template;
   --  Run `Virtual_String_Template` testsuite

   ---------------------
   -- Test_Formatters --
   ---------------------

   procedure Test_Formatters is
   begin
      Test_Support.Run_Testcase
        (Test_Integer_Formatter'Access, "Generic_Integer_Formatter");
   end Test_Formatters;

   ----------------------------
   -- Test_Integer_Formatter --
   ----------------------------

   procedure Test_Integer_Formatter is

      use type VSS.Strings.Virtual_String;

      package Integer_Formatters is
        new VSS.Strings.Formatters.Generic_Integers (Interfaces.Integer_128);

   begin
      --  Smallest negative value.

      declare
         Value    : constant Interfaces.Integer_128 :=
           Interfaces.Integer_128'First;
         Template : constant VSS.Strings.Templates.Virtual_String_Template :=
           "{}";
         Text     : constant VSS.Strings.Virtual_String :=
           Template.Format (Integer_Formatters.Image (Value));
         Image    : constant VSS.Strings.Virtual_String :=
           VSS.Strings.To_Virtual_String
             (Interfaces.Integer_128'Wide_Wide_Image (Value));

      begin
         Test_Support.Assert (Text = Image);
      end;

      --  Largest positive value.

      declare
         Value    : constant Interfaces.Integer_128 :=
           Interfaces.Integer_128'Last;
         Template : constant VSS.Strings.Templates.Virtual_String_Template :=
           " {}";
         Text     : constant VSS.Strings.Virtual_String :=
           Template.Format (Integer_Formatters.Image (Value));
         Image    : constant VSS.Strings.Virtual_String :=
           VSS.Strings.To_Virtual_String
             (Interfaces.Integer_128'Wide_Wide_Image (Value));

      begin
         Test_Support.Assert (Text = Image);
      end;

      --  Zero value.

      declare
         Value    : constant Interfaces.Integer_128 := 0;
         Template : constant VSS.Strings.Templates.Virtual_String_Template :=
           " {}";
         Text     : constant VSS.Strings.Virtual_String :=
           Template.Format (Integer_Formatters.Image (Value));
         Image    : constant VSS.Strings.Virtual_String :=
           VSS.Strings.To_Virtual_String
             (Interfaces.Integer_128'Wide_Wide_Image (Value));

      begin
         Test_Support.Assert (Text = Image);
      end;

      --  Fixed width, without zero padding, positive value

      declare
         Value    : constant Interfaces.Integer_128 := 12345;
         Template : constant VSS.Strings.Templates.Virtual_String_Template :=
           "{:+10}";
         Text     : constant VSS.Strings.Virtual_String :=
           Template.Format (Integer_Formatters.Image (Value));

      begin
         Test_Support.Assert (Text = "     +12345");
      end;

      --  Fixed width, with zero padding, positive value

      declare
         Value    : constant Interfaces.Integer_128 := 12345;
         Template : constant VSS.Strings.Templates.Virtual_String_Template :=
           "{:+010}";
         Text     : constant VSS.Strings.Virtual_String :=
           Template.Format (Integer_Formatters.Image (Value));

      begin
         Test_Support.Assert (Text = "+0000012345");
      end;

      --  Fixed width, width overflow, positive value, sign padding

      declare
         --  use type Interfaces.Integer_128;

         Value    : constant Interfaces.Integer_128 := 1234567890;
         Template : constant VSS.Strings.Templates.Virtual_String_Template :=
           "{:-8}";
         Text     : constant VSS.Strings.Virtual_String :=
           Template.Format (Integer_Formatters.Image (Value));

      begin
         Test_Support.Assert (Text = " 1234567890");
      end;

      --  Base, fixed width, grouping

      declare
         Value    : constant Interfaces.Integer_128 := 16#ABCD_1234#;
         Template : constant VSS.Strings.Templates.Virtual_String_Template :=
           "{:8#16_4}";
         Text     : constant VSS.Strings.Virtual_String :=
           Template.Format (Integer_Formatters.Image (Value));

      begin
         Test_Support.Assert (Text = "ABCD_1234");
      end;

      --  Base, fixed width, padding, grouping

      declare
         Value    : constant Interfaces.Integer_128 := 16#EF#;
         Template : constant VSS.Strings.Templates.Virtual_String_Template :=
           "{:08#16_4}";
         Text     : constant VSS.Strings.Virtual_String :=
           Template.Format (Integer_Formatters.Image (Value));

      begin
         Test_Support.Assert (Text = "0000_00EF");
      end;
   end Test_Integer_Formatter;

   ------------------------------
   -- Test_Multiple_Placeholer --
   ------------------------------

   procedure Test_Multiple_Placeholer is

      use type VSS.Strings.Virtual_String;

      Template : constant VSS.Strings.Templates.Virtual_String_Template :=
        "{}:{}:{}";
      Text     : constant VSS.Strings.Virtual_String :=
        Template.Format
          (VSS.Strings.Formatters.Strings.Image ("a"),
           VSS.Strings.Formatters.Strings.Image ("b"),
           VSS.Strings.Formatters.Strings.Image ("c"));

   begin
      Test_Support.Assert (Text = "a:b:c");
   end Test_Multiple_Placeholer;

   ----------------------------
   -- Test_Single_Placeholer --
   ----------------------------

   procedure Test_Single_Placeholer is

      use type VSS.Strings.Virtual_String;

   begin
      --  Placeholder only

      declare
         Template : constant VSS.Strings.Templates.Virtual_String_Template :=
           "{}";
         Text     : constant VSS.Strings.Virtual_String :=
           Template.Format (VSS.Strings.Formatters.Strings.Image ("world"));

      begin
         Test_Support.Assert (Text = "world");
      end;

      --  Placeholder inside the text

      declare
         Template : constant VSS.Strings.Templates.Virtual_String_Template :=
           "Hello, {}!";
         Text     : constant VSS.Strings.Virtual_String :=
           Template.Format (VSS.Strings.Formatters.Strings.Image ("world"));

      begin
         Test_Support.Assert (Text = "Hello, world!");
      end;
   end Test_Single_Placeholer;

   -------------------
   -- Test_Template --
   -------------------

   procedure Test_Template is
   begin
      Test_Support.Run_Testcase
        (Test_Single_Placeholer'Access, "single placeholder");
      Test_Support.Run_Testcase
        (Test_Multiple_Placeholer'Access, "multiple placeholders");
   end Test_Template;

begin
   Test_Support.Run_Testsuite
     (Test_Template'Access, "Virtual_String_Template");
   Test_Support.Run_Testsuite (Test_Formatters'Access, "Formatters");
end Test_String_Template;
