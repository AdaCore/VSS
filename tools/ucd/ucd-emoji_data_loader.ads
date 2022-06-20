--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package UCD.Emoji_Data_Loader is

   procedure Load (UCD_Root : Wide_Wide_String);
   --  Loads 'emoji/emoji-data.txt' file.
   --
   --  Note, 'UnicodeData.txt' file must be loaded before this file, to be
   --  able to initialize default value of the Extended_Pictographic property.

end UCD.Emoji_Data_Loader;
