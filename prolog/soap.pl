:- module(soap,
	  [ soap_call/3			% +Method, +Input, -Output
	  ]).
:- use_module(library(debug)).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(library(xpath)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_ssl_plugin)).

:- use_module(wsdl).
:- use_module(xml_schema).

:- meta_predicate
	soap_call(:, +, -).

soap_version(soap11,
	     'http://schemas.xmlsoap.org/soap/envelope/',
	     'text/xml; charset=UTF-8'
	    ).
soap_version(soap12,
	     'http://www.w3.org/2003/05/soap-envelope',
	     'application/soap+xml; charset=UTF-8').

%%	soap_call(:Operation, +Input, -Output)
%
%	Generate  a  soap  call.  Input  is  a  list  of  input  values.
%	Currently,  each  element  is   of    the   form  Name=Value  or
%	Name(Value), where name is the  local   part  of  the element or
%	attribute name that needs to be   filled and Value satisfies the
%	XML simple type required by the  field. For multi-valued fields,
%	i.e., fields that have a =maxOccurs= that is not =1=, the values
%	must be combined in a list.   Optional fields, i.e., fields that
%	have =minOccurs= set to =0=, can be omitted from the Input list.
%
%	@tbd	The input specification is over simplified:
%		  - XML namespaces cannot be specified
%		  - The same (named) field cannot appear in two places
%		  - There is no way to specify multiple values for
%		    complex types.

soap_call(Operation, Input, Reply) :-
	Operation = M:_,
%	Version = soap12,		% TBD: How to sort this out nicely
	wsdl_function(Operation, Version, URL, Action,
		      InputElements, OutputElements), !,
	debug(soap, '~w: URL=~q', [Version, URL]),
	soap_action(Action, Version, SoapOptions),
	assertion(length(InputElements, 1)),
	assertion(length(InputElements, 1)),
	InputElements = [arg(_Name, element(InputElement))],
	xsd_create_element(InputElement, M:Input, InputContentDOM0),
	dom_local_ns(InputContentDOM0, InputContentDOM),
	soap_version(Version, SoapPrefix, ContentType),
	InputDOM = element(SoapPrefix:'Envelope', [],
			   [ element(SoapPrefix:'Body', [], [InputContentDOM])
			   ]),
	(   debugging(soap)
	->  http_post_data(xml(ContentType, InputDOM),
			   user_error, [])
	;   true
	),
	setup_call_cleanup(
	    http_open(URL, In,
		      [ method(post),
			post(xml(ContentType, InputDOM)),
			cert_verify_hook(cert_verify),
			status_code(Code),
			header(content_type, ReplyContentType)
		      | SoapOptions
		      ]),
	    soap_read_reply(Code, ReplyContentType, In, ReplyDOM),
	    close(In)),
	soap_reply(Code, SoapPrefix, ReplyDOM, OutputElements, M, Reply).


soap_action(Action, soap11, [request_header('SOAPAction'=QAction)]) :- !,
	atomic_list_concat(['"',Action,'"'], QAction),
	debug(soap, 'SOAPAction: ~w', [QAction]).
soap_action('', _, []).


soap_read_reply(Code, ContentType, In, DOM) :-
	debug(soap, 'Status = ~w; content = ~q', [Code, ContentType]),
	load_structure(stream(In), DOM,
		       [ dialect(xmlns),
			 space(remove)
		       ]).


%%	soap_reply(+Code, +SoapPrefix, +ReplyDOM, +OutputElements,
%%		   +Module, -Reply)

soap_reply(200, SoapPrefix, ReplyDOM, OutputElements, Module, Reply) :- !,
	xpath_chk(ReplyDOM, //(SoapPrefix:'Body'(self)), Body),
	Body = element(_,_,Content),
	Content = [OutputDOM],
	OutputElements = [arg(_Name, element(OutputElement))],
	xsd_create_element(OutputElement, Module:Reply, OutputDOM).
soap_reply(_, _SoapPrefix, ReplyDOM, _, _, _Reply) :-
	(   debugging(soap)
	->  xml_write(user_error, ReplyDOM, [])
	;   true
	),
	xpath(ReplyDOM, //faultstring(text), Text), !,
	throw(error(soap_error(Text))).
soap_reply(_, _SoapPrefix, ReplyDOM, _, _, _Reply) :-
	xml_write(user_error, ReplyDOM, []).


%%	dom_local_ns(+DOM0, -DOM) is det.
%
%	Rewrite the DOM, which  that  the   XML  namespace  of the outer
%	element is the default  for  the   inside.  This  should make no
%	difference, but it does  make  a   difference  to  some  systems
%	handling envelopes and not properly   propagating  the namespace
%	declarations.
%
%	@tbd:	Make sgml_write:add_missing_namespaces/3 public

dom_local_ns(element(DefNS:Name, Attrs, Content), DOM) :-
	DOM1 = element(DefNS:Name, [xmlns=DefNS|Attrs], Content),
	sgml_write:add_missing_namespaces(DOM1, [[]=DefNS], DOM).


%%	cert_verify(SSL, ProblemCert, AllCerts, FirstCert, Error)
%
%	Just accept all SSL certificates.  We do not care.

cert_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, _Error) :-
        format(user_error, 'Accepting certificate~n', []).
