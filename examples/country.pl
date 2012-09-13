:- use_module(library(wsdl)).
:- use_module(library(xpath)).
:- use_module(library(soap)).

:- debug(soap).

:- multifile
	sgml_write:xmlns/2.

sgml_write:xmlns(country, 'http://www.webserviceX.NET').


:- initialization
	wsdl_read('country.wsdl').

get_isd(Country, Reply) :-
	Operation = ('http://www.webserviceX.NET':countrySoap) /
	            ('http://www.webserviceX.NET':'GetISD'),
	soap_call(Operation,
		  [ 'CountryName'=Country
		  ],
		  Reply).
