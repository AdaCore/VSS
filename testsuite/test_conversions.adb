with Ada.Strings.UTF_Encoding;

with Magic_Strings.Conversions;

procedure Test_Conversions is

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

   Hellos : constant array (Positive range <>) of access constant UTF_8_String :=
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

begin
   --  Check conversion of "Hello, world!" in different languages. It is known
   --  all strings are well-formed, there should be no exceptions. Note, it is
   --  sanity check, it doesn't cover all cases of UTF-8 validation.

   for Source of Hellos loop
      declare
         String : Magic_Strings.Magic_String;

      begin
         String := Magic_Strings.Conversions.To_Magic_String (Source.all);

         if Magic_Strings.Conversions.To_UTF_8_String (String) /= Source.all then
            raise Program_Error;
         end if;
      end;
   end loop;
end Test_Conversions;
