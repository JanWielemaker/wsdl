:- module(country,
	  [ get_isd/2			% +CountryName, -ISD
	  ]).
:- use_module(library(wsdl)).
:- use_module(library(xpath)).
:- use_module(library(soap)).

/** <module>

Simple demo that  uses   http://www.webservicex.net/country.asmx  to get
basic facts about countries such as their dialing code, currency etc.

The WSDL description is in =|country.wsdl|=.   The latest version can be
downloaded from http://www.webservicex.net/country.asmx?wsdl
*/

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
