--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.Wide_Wide_Unbounded;

with VSS.Strings.Conversions;

with Hello_World_Data;
with Test_Support;

procedure Test_String_Conversions is

   procedure Do_Test
     (UTF_32_Encoded : Wide_Wide_String;
      UTF_8_Encoded  : Ada.Strings.UTF_Encoding.UTF_8_String);
   --  Do test for given string

   -------------
   -- Do_Test --
   -------------

   procedure Do_Test
     (UTF_32_Encoded : Wide_Wide_String;
      UTF_8_Encoded  : Ada.Strings.UTF_Encoding.UTF_8_String)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
      use type Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;

   begin
      --  Virtual_String created from UTF-32 encoded Wide_Wide_String

      declare
         S : constant VSS.Strings.Virtual_String :=
           VSS.Strings.To_Virtual_String (UTF_32_Encoded);

      begin
         Test_Support.Assert
           (VSS.Strings.Conversions.To_UTF_8_String (S) = UTF_8_Encoded);
         Test_Support.Assert
           (VSS.Strings.Conversions.To_Unbounded_UTF_8_String (S)
              = Ada.Strings.Unbounded.To_Unbounded_String (UTF_8_Encoded));
         Test_Support.Assert
           (VSS.Strings.Conversions.To_Wide_Wide_String (S) = UTF_32_Encoded);
         Test_Support.Assert
           (VSS.Strings.Conversions.To_Unbounded_Wide_Wide_String (S)
              = Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String
                  (UTF_32_Encoded));
      end;

      --  Virtual_String created from UTF-32 encoded Unbounded_Wide_Wide_String

      declare
         S : constant VSS.Strings.Virtual_String :=
           VSS.Strings.Conversions.To_Virtual_String
             (Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String
                (UTF_32_Encoded));

      begin
         Test_Support.Assert
           (VSS.Strings.Conversions.To_UTF_8_String (S) = UTF_8_Encoded);
         Test_Support.Assert
           (VSS.Strings.Conversions.To_Unbounded_UTF_8_String (S)
              = Ada.Strings.Unbounded.To_Unbounded_String (UTF_8_Encoded));
         Test_Support.Assert
           (VSS.Strings.Conversions.To_Wide_Wide_String (S) = UTF_32_Encoded);
         Test_Support.Assert
           (VSS.Strings.Conversions.To_Unbounded_Wide_Wide_String (S)
              = Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String
                  (UTF_32_Encoded));
      end;

      --  Virtual_String created from UTF-8 encoded String

      declare
         S : constant VSS.Strings.Virtual_String :=
           VSS.Strings.Conversions.To_Virtual_String (UTF_8_Encoded);

      begin
         Test_Support.Assert
           (VSS.Strings.Conversions.To_UTF_8_String (S) = UTF_8_Encoded);
         Test_Support.Assert
           (VSS.Strings.Conversions.To_Unbounded_UTF_8_String (S)
              = Ada.Strings.Unbounded.To_Unbounded_String (UTF_8_Encoded));
         Test_Support.Assert
           (VSS.Strings.Conversions.To_Wide_Wide_String (S) = UTF_32_Encoded);
         Test_Support.Assert
           (VSS.Strings.Conversions.To_Unbounded_Wide_Wide_String (S)
              = Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String
                  (UTF_32_Encoded));
      end;

      --  Virtual_String created from UTF-8 encoded Unbounded_String

      declare
         S : constant VSS.Strings.Virtual_String :=
           VSS.Strings.Conversions.To_Virtual_String
             (Ada.Strings.Unbounded.To_Unbounded_String (UTF_8_Encoded));

      begin
         Test_Support.Assert
           (VSS.Strings.Conversions.To_UTF_8_String (S) = UTF_8_Encoded);
         Test_Support.Assert
           (VSS.Strings.Conversions.To_Unbounded_UTF_8_String (S)
              = Ada.Strings.Unbounded.To_Unbounded_String (UTF_8_Encoded));
         Test_Support.Assert
           (VSS.Strings.Conversions.To_Wide_Wide_String (S) = UTF_32_Encoded);
         Test_Support.Assert
           (VSS.Strings.Conversions.To_Unbounded_Wide_Wide_String (S)
              = Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String
                  (UTF_32_Encoded));
      end;
   end Do_Test;

begin
   --  Check conversion of "Hello, world!" in different languages. It is known
   --  all strings are well-formed, there should be no exceptions. Note, it is
   --  sanity check, it doesn't cover all cases of UTF-8 validation.

   for Language in Hello_World_Data.Language'Range loop
      declare
         String : VSS.Strings.Virtual_String;

      begin
         String :=
           VSS.Strings.Conversions.To_Virtual_String
             (Hello_World_Data.Hello (Language));

         if VSS.Strings.Conversions.To_UTF_8_String (String)
           /= Hello_World_Data.Hello (Language)
         then
            raise Program_Error;
         end if;

         --  Check conversion to Wide_Wide_String, it may be improved by
         --  providing Wide_Wide_String version in Hello_World_Data.

         if VSS.Strings.Conversions.To_Wide_Wide_String (String)
           /= VSS.Strings.Conversions.To_Wide_Wide_String
                (VSS.Strings.To_Virtual_String
                   (VSS.Strings.Conversions.To_Wide_Wide_String (String)))
         then
            raise Program_Error;
         end if;

         --  Check conversion to Wide_Wide_String, it may be improved by
         --  providing Wide_Wide_String version in Hello_World_Data.

         if VSS.Strings.Conversions.To_Wide_Wide_String (String)
           /= VSS.Strings.Conversions.To_Wide_Wide_String
                (VSS.Strings.To_Virtual_String
                   (Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String
                      (VSS.Strings.Conversions.To_Unbounded_Wide_Wide_String
                         (String))))
         then
            raise Program_Error;
         end if;
      end;
   end loop;

   --  Check conversion of one character of each representation length in
   --  UTF-8. First string is quite short and may fit to be stored in-place
   --  while second one is large enought to be stored by handler with
   --  allocation.
   --
   --  More cases may need to be added to cover other configurations of string
   --  handlers.

   declare
      S1 : constant Wide_Wide_String := "AБक𐌈";
      S2 : constant Wide_Wide_String := "AБक𐌈𐌈कБA";
      E1 : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
        (1  => Character'Val (16#41#),
         2  => Character'Val (16#D0#),
         3  => Character'Val (16#91#),
         4  => Character'Val (16#E0#),
         5  => Character'Val (16#A4#),
         6  => Character'Val (16#95#),
         7  => Character'Val (16#F0#),
         8  => Character'Val (16#90#),
         9  => Character'Val (16#8C#),
         10 => Character'Val (16#88#));
      E2 : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
        (1  => Character'Val (16#41#),
         2  => Character'Val (16#D0#),
         3  => Character'Val (16#91#),
         4  => Character'Val (16#E0#),
         5  => Character'Val (16#A4#),
         6  => Character'Val (16#95#),
         7  => Character'Val (16#F0#),
         8  => Character'Val (16#90#),
         9  => Character'Val (16#8C#),
         10 => Character'Val (16#88#),
         11 => Character'Val (16#F0#),
         12 => Character'Val (16#90#),
         13 => Character'Val (16#8C#),
         14 => Character'Val (16#88#),
         15 => Character'Val (16#E0#),
         16 => Character'Val (16#A4#),
         17 => Character'Val (16#95#),
         18 => Character'Val (16#D0#),
         19 => Character'Val (16#91#),
         20 => Character'Val (16#41#));

   begin
      Do_Test (S1, E1);
      Do_Test (S2, E2);
   end;

   --  Check that null string is handled properly.

   declare
      N : VSS.Strings.Virtual_String;
      pragma Warnings (Off, N);

   begin
      if VSS.Strings.Conversions.To_UTF_8_String (N) /= "" then
         raise Program_Error;
      end if;
   end;

   --  T717-008 Check that conversion of quite large Wide_Wide_String is
   --  successful.

   declare
      S : constant Wide_Wide_String :=
        "This is large string literal to test conversion from "
        & " Wide_Wide_String to Virtual_String. At the time of initial "
        & "development this string literal requires to have at least "
        & "256 (two hundreds fithty six) characters to overflow unsigned "
        & "8 (eight) bit wide Length member of the UTF-8 ib place string "
        & "handler.";
      E : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
        "This is large string literal to test conversion from "
        & " Wide_Wide_String to Virtual_String. At the time of initial "
        & "development this string literal requires to have at least "
        & "256 (two hundreds fithty six) characters to overflow unsigned "
        & "8 (eight) bit wide Length member of the UTF-8 ib place string "
        & "handler.";

   begin
      Do_Test (S, E);
   end;

   --  Check conversion of the null and empty strings to
   --  Unbounded_String/Unbounded_Wide_Wide_String.

   declare
      use type Ada.Strings.Unbounded.Unbounded_String;
      use type Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;

      Null_Virtual_String  : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String;
      --  VSS.Strings.Empty_Virtual_String is null string by convention.
      Empty_Virtual_String : constant VSS.Strings.Virtual_String := "";

   begin
      Test_Support.Assert
        (VSS.Strings.Conversions.To_Unbounded_UTF_8_String
           (Null_Virtual_String)
         = Ada.Strings.Unbounded.Null_Unbounded_String);
      Test_Support.Assert
        (VSS.Strings.Conversions.To_Unbounded_UTF_8_String
           (Empty_Virtual_String)
         = Ada.Strings.Unbounded.Null_Unbounded_String);

      Test_Support.Assert
        (VSS.Strings.Conversions.To_Unbounded_Wide_Wide_String
           (Null_Virtual_String)
         = Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String);
      Test_Support.Assert
        (VSS.Strings.Conversions.To_Unbounded_Wide_Wide_String
           (Empty_Virtual_String)
         = Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String);
   end;
end Test_String_Conversions;
