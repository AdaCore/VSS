------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                    Copyright (C) 2020-2021, AdaCore                      --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------
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
