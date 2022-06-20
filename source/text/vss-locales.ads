--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This is placeholder for locale information, not implemented but necessary
--  to stabilize strings API.

--  with VSS.Strings;

package VSS.Locales is

   pragma Preelaborate;

   type Locale is tagged private;

   function Default_Unicode_Locale return Locale;
   --  Return locale with default values for all character properties as
   --  defined by Unicode standard. It should be used when application wants
   --  to ignore any tailoring of the character properties, thus have stable
   --  results on any locale.

   function Current_Locale return Locale;
   --  Return locale associated with the current thread, or system's locale
   --  when thread locale is not set.

--   function To_Lowercase
--     (Self : Locale'Class;
--      Item : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String;
--   --  Convert string to lowercase form using full case conversion and
--   --  tailoring provided by given locale.
--
--   function To_Titlecase
--     (Self : Locale'Class;
--      Item : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String;
--   --  Convert string to titlecase form using full case conversion and
--   --  tailoring provided by given locale.
--
--   function To_Uppercase
--     (Self : VSS.Locales.Locale'Class;
--      Item : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String;
--   --  Convert string to uppercase form using full case conversion and
--   --  tailoring provided by given locale.

private

   type Locale is tagged null record;

end VSS.Locales;
