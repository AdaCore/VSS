--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings;

package VSS.IRIs is

   pragma Preelaborate;

   type IRI is tagged private;

   Empty_IRI : constant IRI;

   function To_IRI (Image : VSS.Strings.Virtual_String) return IRI;

   function To_Virtual_String
     (Self : IRI'Class) return VSS.Strings.Virtual_String;

private

   type IRI is new VSS.Strings.Virtual_String with null record;

   Empty_IRI : constant IRI := (VSS.Strings.Virtual_String with null record);

end VSS.IRIs;
