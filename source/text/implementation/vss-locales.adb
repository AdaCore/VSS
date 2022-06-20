--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body VSS.Locales is

   --------------------
   -- Current_Locale --
   --------------------

   function Current_Locale return Locale is
   begin
      return Result : Locale;
   end Current_Locale;

   ----------------------------
   -- Default_Unicode_Locale --
   ----------------------------

   function Default_Unicode_Locale return Locale is
   begin
      return Result : Locale;
   end Default_Unicode_Locale;

end VSS.Locales;
