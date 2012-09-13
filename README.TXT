---+ A partial WSDL and SOAP library

The =wsdl= pack is designed to simply   access to web services described
by  the  WSDL  (Web  Service  Description  Language)  standard,  and  in
particular the SOAP  variant  thereof.  The   library  consists  of  the
following components:

  * library(wsdl)
  This library can be used to parse a WSDL description and asserts a
  number of facts (described in the library source) that describe the
  functionality of the web-service.  This can be queried using
  wsdl_function/6.

  * library(xml_schema)
  This library is used to compose and decompose XML SOAP messages based
  on the XSD type and a description of the parameters. I.e., it is
  passed the target (XML) type and data to put in there and it creates
  an XML DOM (element(Name, Attributes, Content)) tree that satisfies
  the XML type and contains the parameter values.

  * library(soap)
  This library connects the two above.

The pack comes with a  directory   =examples=,  which provides a working
example that connects it all together.

@tbd	This library is far from complete.  Notably

	 - WSDL plain HTTP get/post methods are not supported, while
	 they typically provide a much simpler API than the SOAP API.
	 - The library(xml_schema) is incomplete and doesn't show a
	 clear view on how XML packages SOAP messages must be
	 represented consise and complete as Prolog terms.

