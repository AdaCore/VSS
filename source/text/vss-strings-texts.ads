--
--  Copyright (C) 2020-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  API to access to string data using line:column indexing.

package VSS.Strings.Texts is

   pragma Preelaborate;

   type Magic_Text is new Virtual_String with null record;

end VSS.Strings.Texts;
