--
--  Copyright (C) 2020, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Data package for testing.

with Ada.Strings.UTF_Encoding;

with VSS.Strings.Conversions;

package body Hello_World_Data is

   use Ada.Strings.UTF_Encoding;

   Hello_Arabic  : aliased constant UTF_8_String := "ﻡﺮﺤﺑﺍ ﺎﻠﻋﺎﻠﻣ";
   Hello_Chinese : aliased constant UTF_8_String := "你好世界";
   Hello_English : aliased constant UTF_8_String := "Hello, world!";
   Hello_French  : aliased constant UTF_8_String := "Bonjour tout le monde";
   Hello_German  : aliased constant UTF_8_String := "Hallo Welt";
   Hello_Greek   : aliased constant UTF_8_String := "Γεια σας κόσμο";
   Hello_Hebrew  : aliased constant UTF_8_String := "שלום העולם";
   Hello_Japanse : aliased constant UTF_8_String := "こんにちは世界";
   Hello_Korean  : aliased constant UTF_8_String := "안녕하세요";
   Hello_Russian : aliased constant UTF_8_String := "Здравствуй, мир!";
   Hello_Thai    : aliased constant UTF_8_String := "สวัสดีชาวโลก";
   Hello_Turkish : aliased constant UTF_8_String := "merhaba dünya";

   Language_Arabic  : aliased constant UTF_8_String := "Arabic";
   Language_Chinese : aliased constant UTF_8_String := "Chinese";
   Language_English : aliased constant UTF_8_String := "English";
   Language_French  : aliased constant UTF_8_String := "French";
   Language_German  : aliased constant UTF_8_String := "German";
   Language_Greek   : aliased constant UTF_8_String := "Greek";
   Language_Hebrew  : aliased constant UTF_8_String := "Hebrew";
   Language_Japanse : aliased constant UTF_8_String := "Japanse";
   Language_Korean  : aliased constant UTF_8_String := "Korean";
   Language_Russian : aliased constant UTF_8_String := "Russian";
   Language_Thai    : aliased constant UTF_8_String := "Thai";
   Language_Turkish : aliased constant UTF_8_String := "Turkish";

   Languages : constant array (Language) of access constant UTF_8_String :=
     (Language_Arabic'Access,
      Language_Chinese'Access,
      Language_English'Access,
      Language_French'Access,
      Language_German'Access,
      Language_Greek'Access,
      Language_Hebrew'Access,
      Language_Japanse'Access,
      Language_Korean'Access,
      Language_Russian'Access,
      Language_Thai'Access,
      Language_Turkish'Access);
   Hellos : constant array (Language) of access constant UTF_8_String :=
     (Hello_Arabic'Access,
      Hello_Chinese'Access,
      Hello_English'Access,
      Hello_French'Access,
      Hello_German'Access,
      Hello_Greek'Access,
      Hello_Hebrew'Access,
      Hello_Japanse'Access,
      Hello_Korean'Access,
      Hello_Russian'Access,
      Hello_Thai'Access,
      Hello_Turkish'Access);

   -----------
   -- Hello --
   -----------

   function Hello (Self : Language) return String is
   begin
      return Hellos (Self).all;
   end Hello;

   -----------
   -- Hello --
   -----------

   function Hello (Self : Language) return VSS.Strings.Virtual_String is
   begin
      return VSS.Strings.Conversions.To_Virtual_String (Hellos (Self).all);
   end Hello;

   ----------
   -- Name --
   ----------

   function Name (Self : Language) return String is
   begin
      return Languages (Self).all;
   end Name;

   ----------
   -- Name --
   ----------

   function Name (Self : Language) return VSS.Strings.Virtual_String is
   begin
      return VSS.Strings.Conversions.To_Virtual_String (Languages (Self).all);
   end Name;

end Hello_World_Data;
