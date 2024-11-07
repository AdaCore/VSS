--
--  Copyright (C) 2020-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  @private
--
--  Root of hierarchy of the internal implementation units. All code in this
--  hierarchy is subject to change. Applications MUST NOT use this package and
--  its children packages.

package VSS.Implementation is

   pragma Pure;

   type Normalization_Form is
     (Normalization_Form_D,
      Normalization_Form_C,
      Normalization_Form_KD,
      Normalization_Form_KC);

end VSS.Implementation;
