--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body VSS.IRIs is

   ------------
   -- To_IRI --
   ------------

   function To_IRI (Image : VSS.Strings.Virtual_String) return IRI is
   begin
      return (Image with null record);
   end To_IRI;

   -----------------------
   -- To_Virtual_String --
   -----------------------

   function To_Virtual_String
     (Self : IRI'Class) return VSS.Strings.Virtual_String is
   begin
      return VSS.Strings.Virtual_String (Self);
   end To_Virtual_String;

end VSS.IRIs;
