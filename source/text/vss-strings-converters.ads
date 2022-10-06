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

private

   function To_Encoding_Name
     (Encoding_Label : VSS.Strings.Virtual_String'Class)
      return VSS.Strings.Virtual_String;
   --  Convert given encoding label to encoding name by applying some
   --  transformations. Return an empty string when encoding name is invalid.
   --
   --  Encoding label is expected to be in form of charset field in MIME
   --  encoded-words and MIME extended parameter values [RFC-2184]. Combined
   --  ABNF definition for such names is provided by section 2.3 of [RFC-2978]
   --  as:
   --
   --  mime-charset = 1*mime-charset-chars
   --  mime-charset-chars = ALPHA / DIGIT /
   --             "!" / "#" / "$" / "%" / "&" /
   --             "'" / "+" / "-" / "^" / "_" /
   --             "`" / "{" / "}" / "~"
   --  ALPHA        = "A".."Z"    ; Case insensitive ASCII Letter
   --  DIGIT        = "0".."9"    ; Numeric digit
   --
   --  Additionally, according to section 4.2 of the Encoding specification
   --
   --    https://encoding.spec.whatwg.org/
   --
   --  leading and trailing ASCII whitespaces (U+0009 TAB, U+000A LF, U+000C
   --  FF, U+000D CR, or U+0020 SPACE) are allowed, but removed from the
   --  result.
   --
   --  At the end, transformations defined by section 1.4 of UTS #22 are
   --  applied:
   --
   --  1. Delete all characters except a-z, A-Z, and 0-9.
   --  2. Map uppercase A-Z to the corresponding lowercase a-z.
   --  3. From left to right, delete each 0 that is not preceded by a digit.

end VSS.Strings.Converters;
