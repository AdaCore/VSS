--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Constants for named characters.

package VSS.Characters.Punctuations is

   pragma Preelaborate;

   Line_Separator             : constant Virtual_Character :=
     Virtual_Character'Val (16#2028#);
   Paragraph_Separator        : constant Virtual_Character :=
     Virtual_Character'Val (16#2029#);
   Left_To_Right_Embedding    : constant Virtual_Character :=
     Virtual_Character'Val (16#202A#);
   Right_To_Left_Embedding    : constant Virtual_Character :=
     Virtual_Character'Val (16#202B#);
   Pop_Directional_Formatting : constant Virtual_Character :=
     Virtual_Character'Val (16#202C#);
   Left_To_Right_Override     : constant Virtual_Character :=
     Virtual_Character'Val (16#202D#);
   Right_To_Left_Override     : constant Virtual_Character :=
     Virtual_Character'Val (16#202E#);

   Left_To_Right_Isolate      : constant Virtual_Character :=
     Virtual_Character'Val (16#2066#);
   Right_To_Left_Isolate      : constant Virtual_Character :=
     Virtual_Character'Val (16#2067#);
   First_Strong_Isolate       : constant Virtual_Character :=
     Virtual_Character'Val (16#2068#);
   Pop_Directional_Isolate    : constant Virtual_Character :=
     Virtual_Character'Val (16#2069#);

end VSS.Characters.Punctuations;
