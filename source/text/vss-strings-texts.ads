--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  API to access to string data using line:column indexing.

package VSS.Strings.Texts is

   pragma Preelaborate;

   type Line_Count is new Natural;
   subtype Line_Index is Line_Count range 1 .. Line_Count'Last;

   type Column_Count is new Grapheme_Cluster_Count;
   subtype Column_Index is Column_Count range 1 .. Column_Count'Last;

   type Magic_Text is new Virtual_String with null record;

end VSS.Strings.Texts;
