--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.IRIs;

package VSS.XML.Namespaces is

   HTML_Namespace   : constant VSS.IRIs.IRI :=
     VSS.IRIs.To_IRI ("http://www.w3.org/1999/xhtml");
   MathML_Namespace : constant VSS.IRIs.IRI :=
     VSS.IRIs.To_IRI ("http://www.w3.org/1998/Math/MathML");
   SVG_Namespace    : constant VSS.IRIs.IRI :=
     VSS.IRIs.To_IRI ("http://www.w3.org/2000/svg");
   XML_Namespace    : constant VSS.IRIs.IRI :=
     VSS.IRIs.To_IRI ("http://www.w3.org/XML/1998/namespace");
   XMLNS_Namespace  : constant VSS.IRIs.IRI :=
     VSS.IRIs.To_IRI ("http://www.w3.org/2000/xmlns/");

   TAL_Namespace    : constant VSS.IRIs.IRI :=
     VSS.IRIs.To_IRI ("http://xml.zope.org/namespaces/tal");

end VSS.XML.Namespaces;
