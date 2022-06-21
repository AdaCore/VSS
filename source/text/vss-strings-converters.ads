--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package VSS.Strings.Converters is

   pragma Preelaborate;

   type Converter_Flag is
     (Stateless,
      --  Converter doesn't save state between two conversions, and report an
      --  error when provided data is incomplete.

      Stop_On_Error,
      --  Stop conversion on first found error and report it. Consequential
      --  calls of converter's will do nothing, till its state is reset.
      --
      --  Otherwise, errors of conversion are reported, one or more replacement
      --  characters (uFFFD) are added at place of error and conversion
      --  continues.

      Process_BOM);
      --  Process BOM by converter. When enabled, decoder will skip BOM, and
      --  encoder will generate BOM.

   type Converter_Flags is array (Converter_Flag) of Boolean
     with Pack;

   Default_Converter_Flags : constant Converter_Flags := (others => False);

end VSS.Strings.Converters;
