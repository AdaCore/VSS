--
--  Copyright (C) 2023-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Generic API to transform text data.

with VSS.Strings;

package VSS.Transformers is

   pragma Preelaborate;

   type Abstract_Transformer is limited interface;

   function Transform
     (Self : Abstract_Transformer;
      Item : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String is abstract;

   procedure Transform
     (Self : Abstract_Transformer;
      Item : in out VSS.Strings.Virtual_String'Class) is abstract;

end VSS.Transformers;
