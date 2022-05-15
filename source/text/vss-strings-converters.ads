------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                    Copyright (C) 2021-2022, AdaCore                      --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

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
