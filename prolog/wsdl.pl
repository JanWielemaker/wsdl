/*  Part of SWI-Prolog WSDL pack

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2012, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(wsdl,
	  [ wsdl_read/1,		% :WSDLFile
	    wsdl_ensure_loaded/1,	% :WSDLFile
	    wsdl_function/6		% ?Name, ?Version, ?URL, ?Action, ?Input, ?Output
	  ]).
:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- use_module(library(assoc)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(xml_schema).


:- meta_predicate
	wsdl_read(:),
	wsdl_ensure_loaded(:),
	wsdl_function(:, ?, ?, ?, ?, ?).

/** <module> Read WSDL files

This library reads WSDL files  using   wsdl_read/1,  which asserts facts
about the WSDL interface in the   calling module. The provided interface
can be queried using wsdl_function/6.

The current version concentrates on the   SOAP binding. There is partial
support for other bindings.

We assume (but verify) that:

	- SOAP bindings use style =document= and transport =http=
	- Parameters use =literal=
*/

ns('http://schemas.xmlsoap.org/wsdl/soap/',   soap11).
ns('http://schemas.xmlsoap.org/wsdl/soap12/', soap12).

:- dynamic
	wsdl_read/2.			% File, Module

%%	wsdl_ensure_loaded(:File) is det.
%
%	True if File is loaded into the context.

wsdl_ensure_loaded(Module:File) :-
	wsdl_read(File, Module), !.
wsdl_ensure_loaded(Spec) :-
	wsdl_read(Spec),
	Spec = Module:File,
	assertz(wsdl_read(File, Module)).


%%	wsdl_read(:File) is det.
%
%	Read operations from a  WSDL  file.   This  operation  creates a
%	number of predicates in the calling module:
%
%	  * wsdl_operation(PortType, Operation, Input, Output)
%	  Where PortType is `library', Operation is the concrete
%	  operation and Input/Output are message names that describes
%	  the input and output types.
%
%	  * wsdl_message(Type, Params)
%	  Defines the message types for input and output.  Params is
%	  a list Name:Type.
%
%	  * wsdl_binding(PortType, Binding, Style, Transport)
%	  General properties of the binding. Style must be =document=
%	  and Transport must be =http=.
%
%	  * wsdl_binding_operation(Binding, Operation,
%				   Action, Version, Input, Output)
%	  Binding for a specific Operation. Action is the URL, and
%	  Input/Output document the encoding style. This is always
%	  =literal=
%
%	  * wsdl_port(Binding, URL)
%	  HTTP location to contact for Binding.

wsdl_read(Module:File) :-
	retractall(Module:wsdl_message(_,_)),
	retractall(Module:wsdl_operation(_,_,_,_)),
	retractall(Module:wsdl_binding(_,_,_,_)),
	retractall(Module:wsdl_binding_operation(_,_,_,_,_,_)),
	retractall(Module:wsdl_port(_,_)),
	load_structure(File, [DOM],
		       [ dialect(xmlns),
			 space(remove)
		       ]),
	prefix_map(DOM, PrefixMap),
	(   xpath(DOM, /(_:definitions(@targetNamespace)), TargetNameSpace)
	->  TSOptions = [target_namespace(TargetNameSpace)]
	;   TSOptions = []
	),
	Options = [prefixmap(PrefixMap),file(File)|TSOptions],
	extract_messages(DOM, Module, Options),
	extract_operations(DOM, Module, Options),
	extract_bindings(DOM, Module, Options),
	extract_ports(DOM, Module, Options),
	extract_types(DOM, Module, Options).

prefix_map(element(_, Attrs, _), PrefixMap) :-
	prefix_list(Attrs, Pairs),
	list_to_assoc(Pairs, PrefixMap).

prefix_list([], []).
prefix_list([xmlns:Name=Prefix|T0], [Name-Prefix|T]) :- !,
	prefix_list(T0, T).
prefix_list([xmlns=Prefix|T0], [''-Prefix|T]) :- !,
	prefix_list(T0, T).
prefix_list([_|T0], T) :-
	prefix_list(T0, T).


%%	extract_messages(+DOM, +Module, +Options) is det.
%
%	Extract the messages

extract_messages(DOM, Module, Options) :-
	forall(xpath(DOM, _:message(@name=Name), Message),
	       ( qualify_name(Name, QName, Options),
		 extract_message(Message, QName, Module, Options))).

extract_message(Message, Name, Module, Options) :-
	findall(Arg,
		message_part(Message, Arg, Options),
		Arguments),
	assertz(Module:wsdl_message(Name, Arguments)).

message_part(Message, arg(AName, element(QType)), Options) :-
	xpath(Message, _:part(@element=Element, @name=AName), _),
	qualify_name(Element, QType, Options).
message_part(Message, arg(AName, type(QType)), Options) :-
	xpath(Message, _:part(@type=Type, @name=AName), _),
	qualify_name(Type, xmlns, QType, Options).


%%	extract_operations(+DOM, +Module, +Options) is det.

extract_operations(DOM, Module, Options) :-
	forall(xpath(DOM, //(_:portType(@name=Name)), PT),
	       ( qualify_name(Name, QName, Options),
		 port_operations(PT, QName, Module, Options))).

port_operations(PT, PortType, Module, Options) :-
	forall(xpath(PT, _:operation(@name=Name), Op),
	       ( qualify_name(Name, QName, Options),
		 port_operation(Op, PortType, QName, Module, Options))).

port_operation(Op, PortType, Operation, Module, Options) :-
	(   xpath(Op, _:input(@message), Input)
	->  qualify_name(Input, QInput, Options)
	;   QInput = (-)
	),
	(   xpath(Op, _:output(@message), Output)
	->  qualify_name(Output, QOutput, Options)
	;   QOutput = (-)
	),
	assertz(Module:wsdl_operation(PortType, Operation, QInput, QOutput)).

%%	extract_bindings(+DOM, +Module, +Options) is det.
%
%	Extract the binding declarations

extract_bindings(DOM, Module, Options) :-
	forall(xpath(DOM, _:binding(@type=Type, @name=Name), Binding),
	       ( qualify_name(Name, QName, Options),
		 qualify_name(Type, xmlns, QType, Options),
	         extract_binding(Binding, QType, QName, Module, Options)
	       )).

extract_binding(Binding, QType, QName, Module, Options) :-
	(   xpath(Binding, _:binding(@style=Style, @transport=Transport), _)
	->  true
	;   xpath(Binding, _:binding(@transport=Transport), _)
	->  Style = document
	),
	(   transport_id(Transport, TransportId)
	->  true
	;   domain_error(soap_transport, Transport)
	),
	assert(Module:wsdl_binding(QType, QName, Style, TransportId)),
	forall(xpath(Binding, _:operation(self), Operation),
	       extract_binding_operation(Operation, QName, Module, Options)), !.
extract_binding(Binding, QType, QName, Module, Options) :-
	xpath(Binding, _:binding(@verb=Verb), _), !,
	assert(Module:wsdl_binding(QType, QName, Verb, http)),
	forall(xpath(Binding, _:operation(self), Operation),
	       extract_binding_operation(Operation, QName, Module, Options)), !.
extract_binding(Binding, QType, QName, Module, Options) :-
	print_message(error, failed(extract_binding)),
	gtrace,
	extract_binding(Binding, QType, QName, Module, Options).


transport_id('http://schemas.xmlsoap.org/soap/http', http).

extract_binding_operation(Operation, QName, Module, Options) :-
	xpath(Operation, NS:operation(@soapAction=Action), _),
	xpath(Operation, (_:input)/(_:body(@use=InputUse)), _),
	xpath(Operation, (_:output)/(_:body(@use=OutputUse)), _), !,
	(   Action == ''
	->  QAction = Action
	;   qualify_name(Action, QAction, Options)
	),
	(   ns(NS, Soap)
	->  true
	;   existence_error(wsdl_soap_namespace, NS)
	),
	(   xpath(Operation, /(_:operation(@name=OName)), _)
	->  qualify_name(OName, QOp, Options)
	;   QOp = QName
	),
	assertz(Module:wsdl_binding_operation(
			   QName, QOp, QAction, Soap, InputUse, OutputUse)).
extract_binding_operation(Operation, QName, Module, Options) :-
	xpath(Operation, _:operation(@location=Location), _),
	xpath(Operation, _:input(self), Input),
	xpath(Operation, _:output(self), Output),
	verb_input(Input, InputUse),
	verb_output(Output, OutputUse, Options), !,
	(   xpath(Operation, /(_:operation(@name=OName)), _)
	->  qualify_name(OName, QOp, Options)
	;   QOp = QName
	),
	assertz(Module:wsdl_binding_operation(
			   QName, QOp, Location, http, InputUse, OutputUse)).
extract_binding_operation(Operation, QName, Module, Options) :-
	print_message(error, failed(extract_binding_operation)),
	gtrace,
	extract_binding_operation(Operation, QName, Module, Options).

verb_input(Input, url_encoded) :-
	xpath(Input, _:urlEncoded, _), !.
verb_input(Input, Type) :-
	xpath(Input, _:content(@type=Type), _), !.

verb_output(Output, xml(QElement), Options) :-
	xpath(Output, _:mimeXml(@part=Element), _),
	qualify_name(Element, QElement, Options).


%%	extract_ports(+DOM, +Module, +Options) is det.
%
%	Handle port elements  that  binds   operations  to  actual  HTTP
%	addresses.

extract_ports(DOM, Module, Options) :-
	xpath_chk(DOM, _:service, Service),
	forall(xpath(Service, _:port(@binding=Binding), Port),
	       ( qualify_name(Binding, QBinding, Options),
		 extract_port(Port, QBinding, Module, Options))).

extract_port(Port, QBinding, Module, _Options) :-
	xpath(Port, _:address(@location=Location), _), !,
	assertz(Module:wsdl_port(QBinding, Location)), !.
extract_port(_Port, QBinding, _Module, _Options) :- fail, !,
	print_message(warning, wsdl(missing_binding(QBinding))).
extract_port(Port, QBinding, Module, Options) :-
	print_message(error, failed(extract_port)),
	gtrace,
	extract_port(Port, QBinding, Module, Options).


%%	extract_types(+DOM, +Module, +Options) is det.
%
%	Extract the XML schema types.

extract_types(DOM, Module, Options) :-
	xpath_chk(DOM, //(_:schema(self)), Schema),
	xsd_load(Module:Schema, Options).


		 /*******************************
		 *	      QUERY		*
		 *******************************/

%%	wsdl_function(:Name, -Version, -URL, -Action, -Input, -Output)
%%	is nondet.

wsdl_function(Module:PortType/Operation, Version, URL, Action, Input, Output) :-
	Module:wsdl_operation(PortType, Operation, InputMsg, OutputMsg),
	Module:wsdl_binding(PortType, Binding, Document, HTTP),
	assertion(Document == document),
	assertion(HTTP == http),
	Module:wsdl_binding_operation(Binding, Operation, Action, Version,
				      InputBinding, OutputBinding),
	assertion(InputBinding == literal),
	assertion(OutputBinding == literal),
	once(Module:wsdl_message(InputMsg, Input)),
	once(Module:wsdl_message(OutputMsg, Output)),
	Module:wsdl_port(Binding, URL).



		 /*******************************
		 *	       UTIL		*
		 *******************************/

%%	qualify_name(+Name, -QName, -Options) is det.

qualify_name(Name, QName, Options) :-
	qualify_name(Name, tns, QName, Options).

qualify_name(Name, _, QName, _Options) :-
	sub_atom(Name, 0, _, _, 'http://'), !,
	QName = Name.
qualify_name(Name, _, QName, _Options) :-
	sub_atom(Name, 0, _, _, 'https://'), !,
	QName = Name.
qualify_name(Name, _, Prefix:LN, Options) :-
	sub_atom(Name, B, _, A, :), !,
	sub_atom(Name, 0, B, _, NS),
	sub_atom(Name, _, A, 0, LN),
	option(prefixmap(PrefixMap), Options),
	(   get_assoc(NS, PrefixMap, Prefix)
	->  true
	;   existence_error(namespace, NS)
	).
qualify_name(Name, xmlns, Prefix:Name, Options) :- !,
	option(prefixmap(PrefixMap), Options),
	get_assoc('', PrefixMap, Prefix),
	(   Prefix == 'http://www.w3.org/2001/XMLSchema'
	->  true
	;   writeln(Prefix)
	).
qualify_name(Name, tns, Prefix:Name, Options) :-
	option(target_namespace(Prefix), Options).
