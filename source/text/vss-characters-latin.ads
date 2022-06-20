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

   Line_Feed : constant Virtual_Character := Virtual_Character'Val (16#000A#);

end VSS.Characters.Latin;
