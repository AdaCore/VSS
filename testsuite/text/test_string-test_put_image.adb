--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

separate (Test_String)
procedure Test_Put_Image is
begin
   --  "Null" string

   declare
      S : VSS.Strings.Virtual_String;

   begin
      Test_Support.Assert
        (VSS.Strings.Virtual_String'Wide_Wide_Image (S) = """""");
   end;

   --  Empty string

   declare
      S : constant VSS.Strings.Virtual_String := "";

   begin
      Test_Support.Assert
        (VSS.Strings.Virtual_String'Wide_Wide_Image (S) = """""");
   end;

   --  Ordinary string

   declare
      S : constant VSS.Strings.Virtual_String := "ABCD";

   begin
      Test_Support.Assert
        (VSS.Strings.Virtual_String'Wide_Wide_Image (S) = """ABCD""");
   end;

   --  String with quotation mark character

   declare
      S : constant VSS.Strings.Virtual_String := "A""BCD";

   begin
      Test_Support.Assert
        (VSS.Strings.Virtual_String'Wide_Wide_Image (S) = """A""""BCD""");
   end;

   --  String with characters outside of ASCII

   declare
      S : constant VSS.Strings.Virtual_String := "AБა😀";

   begin
      Test_Support.Assert
        (VSS.Strings.Virtual_String'Wide_Wide_Image (S) = """AБა😀""");
   end;
end Test_Put_Image;
