--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Constants for named characters.

package VSS.Characters.Latin is

   pragma Preelaborate;

   Nul       : constant Virtual_Character := Virtual_Character'Val (16#0000#);

   Backspace : constant Virtual_Character := Virtual_Character'Val (16#0008#);
   Character_Tabulation : constant Virtual_Character :=
     Virtual_Character'Val (16#0009#);
   Line_Feed : constant Virtual_Character := Virtual_Character'Val (16#000A#);

   Form_Feed : constant Virtual_Character := Virtual_Character'Val (16#000C#);
   Carriage_Return : constant Virtual_Character :=
     Virtual_Character'Val (16#000D#);

   Space     : constant Virtual_Character := Virtual_Character'Val (16#0020#);

   Quotation_Mark    : constant Virtual_Character :=
     Virtual_Character'Val (16#0022#);

   Ampersand         : constant Virtual_Character :=
     Virtual_Character'Val (16#0026#);
   Apostrophe        : constant Virtual_Character :=
     Virtual_Character'Val (16#0027#);

   Less_Than_Sign    : constant Virtual_Character :=
     Virtual_Character'Val (16#003C#);
   Equals_Sign       : constant Virtual_Character :=
     Virtual_Character'Val (16#003D#);
   Greater_Than_Sign : constant Virtual_Character :=
     Virtual_Character'Val (16#003E#);

   Grave_Accent      : constant Virtual_Character :=
     Virtual_Character'Val (16#0060#);

end VSS.Characters.Latin;
